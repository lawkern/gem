/* /////////////////////////////////////////////////////////////////////////// */
/* (c) copyright 2022 Lawrence D. Kern /////////////////////////////////////// */
/* /////////////////////////////////////////////////////////////////////////// */

#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

#include <GL/gl.h>
#include <GL/glx.h>
#include <GL/glu.h>

#include <stdarg.h>
#include <time.h>

#include "gem.c"
#include "renderer_opengl.c"

struct linux_window_dimensions
{
   s32 width;
   s32 height;
};

#define LINUX_SECONDS_ELAPSED(start, end) ((float)((end).tv_sec - (start).tv_sec) \
        + (1e-9f * (float)((end).tv_nsec - (start).tv_nsec)))

global bool linux_global_is_running;
global bool linux_global_is_paused;
global Display *linux_global_display;

function
PLATFORM_LOG(platform_log)
{
   char message[PLATFORM_LOG_MAX_LENGTH];

   va_list arguments;
   va_start(arguments, format);
   {
      vsnprintf(message, sizeof(message), format, arguments);
   }
   va_end(arguments);

   printf("%s", message);
}

function void *
linux_allocate(size_t size)
{
   // NOTE(law): munmap() requires the size of the allocation in order to free
   // the virtual memory. This function smuggles the allocation size just before
   // the address that it actually returns.

   size_t allocation_size = size + sizeof(size_t);
   void *allocation = mmap(0, allocation_size, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);

   if(allocation == MAP_FAILED)
   {
      platform_log("ERROR: Linux failed to allocate virtual memory.");
      return(0);
   }

   *(size_t *)allocation = allocation_size;

   void *result = (void *)((u8 *)allocation + sizeof(size_t));
   return(result);
}

function void
linux_deallocate(void *memory)
{
   // NOTE(law): munmap() requires the size of the allocation in order to free
   // the virtual memory. We always just want to dump the entire thing, so
   // allocate() hides the allocation size just before the address it returns.

   void *allocation = (void *)((u8 *)memory - sizeof(size_t));
   size_t allocation_size = *(size_t *)allocation;

   if(munmap(allocation, allocation_size) != 0)
   {
      platform_log("ERROR: Linux failed to deallocate virtual memory.");
   }
}

function
PLATFORM_FREE_FILE(platform_free_file)
{
   linux_deallocate(file->memory);
   zero_memory(file, sizeof(*file));
}

function
PLATFORM_LOAD_FILE(platform_load_file)
{
   // TODO(law): Better file I/O once file access is needed anywhere besides
   // program startup.

   struct platform_file result = {0};

   struct stat file_information;
   if(stat(file_path, &file_information) == -1)
   {
      platform_log("ERROR: Linux failed to read file size of file: \"%s\".\n", file_path);
      return(result);
   }

   int file = open(file_path, O_RDONLY);
   if(file == -1)
   {
      platform_log("ERROR: Linux failed to open file: \"%s\".\n", file_path);
      return(result);
   }

   size_t size = file_information.st_size;

   result.memory = linux_allocate(size);
   if(result.memory)
   {
      result.size = size;
      read(file, result.memory, result.size);
   }
   else
   {
      platform_log("ERROR: Linux failed to allocate memory for file: \"%s\".\n", file_path);
   }

   close(file);

   return(result);
}

function void
linux_get_window_dimensions(Window window, struct linux_window_dimensions *dimensions)
{
   Display *display = linux_global_display;

   XWindowAttributes window_attributes = {0};
   XGetWindowAttributes(display, window, &window_attributes);

   dimensions->width  = (s32)window_attributes.width;
   dimensions->height = (s32)window_attributes.height;
}

function void
linux_set_resolution_scale(Window window, u32 scale)
{
   u32 width = RESOLUTION_BASE_WIDTH << scale;
   u32 height = RESOLUTION_BASE_HEIGHT << scale;

   XResizeWindow(linux_global_display, window, width, height);
}

function Window
linux_create_window(struct pixel_bitmap bitmap, XVisualInfo *visual_info)
{
   Display *display = linux_global_display;
   assert(display);

   Window root = DefaultRootWindow(display);

   s32 screen_number = DefaultScreen(display);
   s32 screen_bit_depth = 24;

   XSetWindowAttributes window_attributes = {0};
   u64 attribute_mask = 0;

   window_attributes.background_pixel = 0;
   attribute_mask |= CWBackPixel;

   window_attributes.border_pixel = 0;
   attribute_mask |= CWBorderPixel;

   // NOTE(law): Seeting the bit gravity to StaticGravity prevents flickering
   // during window resize.
   window_attributes.bit_gravity = StaticGravity;
   attribute_mask |= CWBitGravity;

   window_attributes.colormap = XCreateColormap(display, root, visual_info->visual, AllocNone);
   attribute_mask |= CWColormap;

   window_attributes.event_mask = (ExposureMask |
                                   KeyPressMask |
                                   KeyReleaseMask |
                                   ButtonPressMask |
                                   ButtonReleaseMask |
                                   StructureNotifyMask |
                                   PropertyChangeMask);
   attribute_mask |= CWEventMask;

   Window window = XCreateWindow(display,
                                 root,
                                 0,
                                 0,
                                 bitmap.width,
                                 bitmap.height,
                                 0,
                                 visual_info->depth,
                                 InputOutput,
                                 visual_info->visual,
                                 attribute_mask,
                                 &window_attributes);

   assert(window);

   XStoreName(display, window, "Game Boy Emulator (GEM)");

   XSizeHints size_hints = {0};
   size_hints.flags = PMinSize|PMaxSize;
   size_hints.min_width = bitmap.width / 2;
   size_hints.min_height = bitmap.height / 2;
   size_hints.max_width = bitmap.width;
   size_hints.max_height = bitmap.height;
   XSetWMNormalHints(display, window, &size_hints);

   XMapWindow(display, window);
   XFlush(display);

   return(window);
}

function Window
linux_initialize_opengl(struct pixel_bitmap bitmap)
{
   // TODO(law): Better checking for available GL extensions.

   Display *display = linux_global_display;
   s32 screen_number = DefaultScreen(display);

   int error_base;
   int event_base;
   Bool glx_is_supported = glXQueryExtension(display, &error_base, &event_base);
   assert(glx_is_supported);

   // NOTE(law): Get glX frame buffer configuration.
   int configuration_attributes[] =
   {
      GLX_X_RENDERABLE, True,
      GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
      GLX_RENDER_TYPE, GLX_RGBA_BIT,
      GLX_X_VISUAL_TYPE, GLX_TRUE_COLOR,
      GLX_RED_SIZE, 8,
      GLX_GREEN_SIZE, 8,
      GLX_BLUE_SIZE, 8,
      GLX_ALPHA_SIZE, 8,
      GLX_DEPTH_SIZE, 24,
      GLX_STENCIL_SIZE, 8,
      GLX_DOUBLEBUFFER, True,
      // GLX_FRAMEBUFFER_SRGB_CAPABLE_ARB, True,
      GLX_SAMPLE_BUFFERS, 1,
      GLX_SAMPLES, 4,
      None
   };

   s32 configuration_count = 0;
   GLXFBConfig *configurations = glXChooseFBConfig(display, screen_number, configuration_attributes, &configuration_count);

   GLXFBConfig configuration;
   bool found_valid_configuration = false;
   for(u32 configuration_index = 0; configuration_index < configuration_count; ++configuration_index)
   {
      configuration = configurations[configuration_index];

      XVisualInfo *visual_info = glXGetVisualFromFBConfig(display, configuration);
      if(visual_info)
      {
         s32 visual_id = visual_info->visualid;
         XFree(visual_info);

         if(visual_id)
         {
            found_valid_configuration = true;
            break;
         }
      }
   }
   assert(found_valid_configuration);
   XFree(configurations);

   XVisualInfo *visual_info = glXGetVisualFromFBConfig(display, configuration);
   Window window = linux_create_window(bitmap, visual_info);

   // NOTE(law): Load any Linux-specific OpenGL functions we need.
   typedef GLXContext opengl_function_glXCreateContextAttribsARB(Display *, GLXFBConfig, GLXContext, Bool, const int *);
   typedef       void opengl_function_glXSwapIntervalEXT(Display *, GLXDrawable, int);

   DECLARE_OPENGL_FUNCTION(glXCreateContextAttribsARB);
   DECLARE_OPENGL_FUNCTION(glXSwapIntervalEXT);

   LINUX_LOAD_OPENGL_FUNCTION(glXCreateContextAttribsARB);
   LINUX_LOAD_OPENGL_FUNCTION(glXSwapIntervalEXT);

   assert(glXCreateContextAttribsARB);

   s32 context_attributes[] =
   {
      GLX_CONTEXT_MAJOR_VERSION_ARB, 3,
      GLX_CONTEXT_MINOR_VERSION_ARB, 3,
#if DEVELOPMENT_BUILD
      GLX_CONTEXT_FLAGS_ARB, GLX_CONTEXT_DEBUG_BIT_ARB,
#endif
      GLX_CONTEXT_PROFILE_MASK_ARB, GLX_CONTEXT_CORE_PROFILE_BIT_ARB,
      None
   };

   GLXContext gl_context = glXCreateContextAttribsARB(display, configuration, 0, True, context_attributes);
   assert(gl_context);

   Bool context_attached = glXMakeCurrent(display, window, gl_context);
   assert(context_attached);

   // NOTE(law): If we have access to vsync through glX, try to turn it on.
   if(glXSwapIntervalEXT)
   {
      // TODO(law): Make it possible to toggle vsync.
      glXSwapIntervalEXT(display, window, 1);
   }

   int glx_major_version;
   int glx_minor_version;
   glXQueryVersion(display, &glx_major_version, &glx_minor_version);

   platform_log("=====\n");
   platform_log("Version (glX): %d.%d\n", glx_major_version, glx_minor_version);
   platform_log("=====\n");

   // NOTE(law): Load any OpenGL function pointers that we don't expect to have
   // by default before initializing the platform-independent OpenGL code.
#define X(name) LINUX_LOAD_OPENGL_FUNCTION(name);
   OPENGL_FUNCTION_POINTERS;
#undef X

   // NOTE(law): Initialize the platform-independent side of OpenGL.
   opengl_initialize();

   return(window);
}

function void
linux_display_bitmap(Window window, struct pixel_bitmap bitmap)
{
   struct linux_window_dimensions dimensions;
   linux_get_window_dimensions(window, &dimensions);

   opengl_display_bitmap(&bitmap, dimensions.width, dimensions.height);

   glXSwapBuffers(linux_global_display, window);
}

function void
linux_process_input(Window window, XEvent event)
{
   // Keyboard handling:
   if(event.type == KeyPress || event.type == KeyRelease)
   {
      XKeyEvent key_event = event.xkey;
      bool alt_key_pressed = (key_event.state | XK_Meta_L | XK_Meta_R);

      char buffer[256];
      KeySym keysym;
      XLookupString(&key_event, buffer, ARRAY_LENGTH(buffer), &keysym, 0);

      if(event.type == KeyPress)
      {
         if(keysym == XK_Escape || (alt_key_pressed && keysym == XK_F4))
         {
            linux_global_is_running = false;
         }
         else if(keysym == XK_1)
         {
            linux_set_resolution_scale(window, 0);
         }
         else if(keysym == XK_2)
         {
            linux_set_resolution_scale(window, 1);
         }
         else if(keysym == XK_4)
         {
            linux_set_resolution_scale(window, 2);
         }
         else if(keysym == XK_8)
         {
            linux_set_resolution_scale(window, 3);
         }
         else if(keysym == XK_h)
         {
            // NOTE(law): Print the contents of the cartridge header.
            if(map.load_complete)
            {
               dump_cartridge_header(map.rom_banks[0].memory);
            }
            else
            {
               platform_log("No cartridge is currently loaded.\n");
            }
         }
         else if(keysym == XK_d)
         {
            // NOTE(law): Print the disassembly.
            if(map.load_complete)
            {
               platform_log("Parsing instruction stream...\n");
               disassemble_stream(0, 0x10000);
            }
            else
            {
               platform_log("No cartridge is currently loaded.\n");
            }
         }
         else if(keysym == XK_n)
         {
            // NOTE(law): Fetch and execute the next instruction.
            if(map.load_complete)
            {
               platform_log("Fetching and executing instruction...\n");
               disassemble_instruction(registers.pc);

               handle_interrupts();
               fetch_and_execute();
            }
            else
            {
               platform_log("No cartridge is currently loaded.\n");
            }
         }
         else if(keysym == XK_p)
         {
            linux_global_is_paused = !linux_global_is_paused;
         }
         else if(keysym == XK_c)
         {
            if(++gem_global_color_scheme >= MONOCHROME_COLOR_SCHEME_COUNT)
            {
               gem_global_color_scheme = 0;
            }
         }
      }
   }
}

function void
linux_process_events(Window window)
{
   Display *display = linux_global_display;

   while(linux_global_is_running && XPending(display))
   {
      XEvent event;
      XNextEvent(display, &event);

      // NOTE(law): Prevent key repeating.
      if(event.type == KeyRelease && XEventsQueued(display, QueuedAfterReading))
      {
         XEvent next_event;
         XPeekEvent(display, &next_event);
         if(next_event.type == KeyPress &&
             next_event.xkey.time == event.xkey.time &&
             next_event.xkey.keycode == event.xkey.keycode)
         {
            XNextEvent(display, &event);
            continue;
         }
      }

      switch (event.type)
      {
         case DestroyNotify:
         {
            XDestroyWindowEvent destroy_notify_event = event.xdestroywindow;
            if(destroy_notify_event.window == window)
            {
               linux_global_is_running = false;
            }
         } break;

         case Expose:
         {
            XExposeEvent expose_event = event.xexpose;
            if(expose_event.count != 0)
            {
               continue;
            }
         } break;

         case ConfigureNotify:
         {
            s32 window_width  = event.xconfigure.width;
            s32 window_height = event.xconfigure.height;

            // TODO(law): Handle resizing the window.
         } break;

         case KeyPress:
         case KeyRelease:
         case ButtonPress:
         case ButtonRelease:
         {
            linux_process_input(window, event);
         } break;

         default:
         {
            // platform_log("Unhandled X11 event.\n");
         } break;
      }
   }
}

int
main(int argument_count, char **arguments)
{
   char *program_name = arguments[0];

   if(argument_count != 2)
   {
      platform_log("USAGE: %s <path to ROM file>\n", program_name);
      return(0);
   }

   // NOTE(law): Perform general dynamic allocations up front.
   struct memory_arena arena = {0};
   arena.size = MEBIBYTES(64);
   arena.base_address = linux_allocate(arena.size);

   // NOTE(law) Set up the rendering bitmap.
   struct pixel_bitmap bitmap = {RESOLUTION_BASE_WIDTH, RESOLUTION_BASE_HEIGHT};

   size_t bytes_per_pixel = sizeof(u32);
   size_t bitmap_size = bitmap.width * bitmap.height * bytes_per_pixel;
   bitmap.memory = linux_allocate(bitmap_size);

   // NOTE(law): Initialize the global display here.
   linux_global_display = XOpenDisplay(0);
   Window window = linux_initialize_opengl(bitmap);

   char *rom_path = arguments[argument_count - 1];
   load_cartridge(&arena, rom_path);

   float frame_seconds_elapsed = 0;
   float target_seconds_per_frame = 1.0f / VERTICAL_SYNC_HZ;
   u32 target_cycles_per_frame = (u32)(CPU_HZ / VERTICAL_SYNC_HZ);

   u32 clear_color = get_display_off_color();
   clear(&bitmap, clear_color);

   struct cycle_clocks clocks = {0};
   struct sound_samples sound = {0};
   sound.size = SOUND_OUTPUT_HZ * SOUND_OUTPUT_BYTES_PER_SAMPLE;
   sound.samples = linux_allocate(sound.size);

   struct timespec frame_start_count;
   clock_gettime(CLOCK_MONOTONIC, &frame_start_count);

   linux_global_is_running = true;
   while(linux_global_is_running)
   {
      linux_process_events(window);

      if(!map.load_complete)
      {
         clear_color = get_display_off_color();
         clear(&bitmap, clear_color);
      }

      if(!linux_global_is_paused && map.load_complete)
      {
         clocks.cpu %= target_cycles_per_frame;
         while(clocks.cpu < target_cycles_per_frame)
         {
            cpu_tick(&clocks, &bitmap, &sound);
         }

         clear_sound_samples(&sound);
      }

      // NOTE(law): Blit bitmap to screen.
      linux_display_bitmap(window, bitmap);

      // NOTE(law): Calculate elapsed frame time.
      struct timespec frame_end_count;
      clock_gettime(CLOCK_MONOTONIC, &frame_end_count);

      u32 sleep_us = 0;
      u32 instructions_executed = 0;
      frame_seconds_elapsed = LINUX_SECONDS_ELAPSED(frame_start_count, frame_end_count);

      float sleep_fraction = 0.9f;
      if(frame_seconds_elapsed < target_seconds_per_frame)
      {
         sleep_us = (u32)((target_seconds_per_frame - frame_seconds_elapsed) * 1000.0f * 1000.0f * sleep_fraction);
         if(sleep_us > 0)
         {
            usleep(sleep_us);
         }
      }

      while(frame_seconds_elapsed < target_seconds_per_frame)
      {
         clock_gettime(CLOCK_MONOTONIC, &frame_end_count);
         frame_seconds_elapsed = LINUX_SECONDS_ELAPSED(frame_start_count, frame_end_count);
      }
      frame_start_count = frame_end_count;

      platform_log("Frame time: %0.03fms, ", frame_seconds_elapsed * 1000.0f);
      platform_log("Sleep: %uus, ", sleep_us);
      platform_log("Cycles: %u, ", clocks.cpu);
      platform_log("Cycle time: %0.05fus\n", (frame_seconds_elapsed * 1000.0f * 1000.0f) / clocks.cpu);
   }

   XCloseDisplay(linux_global_display);

   return(0);
}
