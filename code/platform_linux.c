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

#include <stdarg.h>
#include <string.h>
#include <time.h>

#include "gem.c"

typedef struct
{
   s32 width;
   s32 height;
} Linux_Window_Dimensions;

#define LINUX_SECONDS_ELAPSED(start, end) ((float)((end).tv_sec - (start).tv_sec) \
        + (1e-9f * (float)((end).tv_nsec - (start).tv_nsec)))

static bool linux_global_is_running;
static bool linux_global_is_paused;

static Display *linux_global_display;
static XImage *linux_global_window_buffer;
static GC linux_global_graphics_context;

static
PLATFORM_LOG(platform_log)
{
   char message[1024];

   va_list arguments;
   va_start(arguments, format);
   {
      vsnprintf(message, sizeof(message), format, arguments);
   }
   va_end(arguments);

   printf("%s", message);
}

static
PLATFORM_FREE_FILE(platform_free_file)
{
   free(file->memory);
   file->size = 0;
   file->memory = 0;
}

static
PLATFORM_LOAD_FILE(platform_load_file)
{
   // TODO(law): Better file I/O once file access is needed anywhere besides
   // program startup.

   struct platform_file result = {0};

   struct stat file_information;
   if(stat(file_path, &file_information) == -1)
   {
      fprintf(stderr, "ERROR: Failed to read file size of file: \"%s\".\n", file_path);
      return(result);
   }

   int file = open(file_path, O_RDONLY);
   if(file == -1)
   {
      fprintf(stderr, "ERROR: Failed to open file: \"%s\".\n", file_path);
      return(result);
   }

   size_t size = file_information.st_size;

   result.memory = malloc(size);
   if(result.memory)
   {
      result.size = size;
      read(file, result.memory, result.size);
   }
   else
   {
      fprintf(stderr, "ERROR: Failed to allocate memory for file: \"%s\".\n", file_path);
   }

   close(file);

   return(result);
}

static void
linux_process_input(XEvent event)
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
               disassemble_instruction(register_pc);

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

static void
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
            fprintf(stdout, "Event: DestroyNotify\n");

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
               fprintf(stdout, "Event: Expose\n");
               continue;
            }
            else
            {
               fprintf(stdout, "Event: Expose (final)\n");
            }
         } break;

         case ConfigureNotify:
         {
            fprintf(stdout, "Event: ConfigureNotify\n");

            s32 window_width  = event.xconfigure.width;
            s32 window_height = event.xconfigure.height;

            // TODO(law): Handle resizing the window.
         } break;

         case KeyPress:
         case KeyRelease:
         case ButtonPress:
         case ButtonRelease:
         {
            linux_process_input(event);
         } break;

         default:
         {
            fprintf(stdout, "Event: unhandled\n");
         } break;
      }
   }
}

static void
linux_get_window_dimensions(Window window, Linux_Window_Dimensions *dimensions)
{
   Display *display = linux_global_display;

   XWindowAttributes window_attributes = {0};
   XGetWindowAttributes(display, window, &window_attributes);

   dimensions->width  = (s32)window_attributes.width;
   dimensions->height = (s32)window_attributes.height;
}

static void
linux_display_bitmap(Window window, struct pixel_bitmap bitmap)
{
   Display *display = linux_global_display;
   XImage *image = linux_global_window_buffer;
   GC graphics_context = linux_global_graphics_context;

   Linux_Window_Dimensions dimensions;
   linux_get_window_dimensions(window, &dimensions);

   s32 client_width = dimensions.width;
   s32 client_height = dimensions.height;

   // TODO(law): There doesn't seem to be a good way using just the X11 protocol
   // to simulate StretchDIBits on Windows (short of just recreating the image),
   // so don't even bother to scale the image. We'll probably end up using
   // OpenGL for blitting in the end.

   s32 target_width  = bitmap.width;
   s32 target_height = bitmap.height;

   XPutImage(display, window, graphics_context, image, 0, 0, 0, 0, target_width, target_height);

   if(client_width > target_width)
   {
      s32 gutter_width = client_width - target_width;
      XClearArea(display, window, target_width, 0, gutter_width, client_height, False);
   }

   if(client_height > target_height)
   {
      s32 gutter_height = client_height - target_height;
      XClearArea(display, window, 0, target_height, client_width, gutter_height, False);
   }
}

static Window
linux_create_window(struct pixel_bitmap bitmap)
{
   Display *display = linux_global_display;
   if(!display)
   {
      fprintf(stderr, "Failed to open a display.\n");
      exit(1);
   }

   Window root = DefaultRootWindow(display);
   s32 screen_number = DefaultScreen(display);

   s32 screen_bit_depth = 24;
   XVisualInfo visual_info = {0};
   if(!XMatchVisualInfo(display, screen_number, screen_bit_depth, TrueColor, &visual_info))
   {
      fprintf(stderr, "Failed to find a matching XVisualInfo.\n");
      exit(1);
   }

   linux_global_window_buffer = XCreateImage(display, visual_info.visual, visual_info.depth, ZPixmap, 0,
                                             (char *)bitmap.memory, bitmap.width, bitmap.height, 32, 0);

   linux_global_graphics_context = DefaultGC(display, screen_number);

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

   window_attributes.colormap = XCreateColormap(display, root, visual_info.visual, AllocNone);
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
                                 visual_info.depth,
                                 InputOutput,
                                 visual_info.visual,
                                 attribute_mask,
                                 &window_attributes);

   if(!window)
   {
      fprintf(stderr, "Failed to open a display.\n");
      exit(1);
   }

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

int
main(int argument_count, char **arguments)
{
   char *program_name = arguments[0];

   if(argument_count != 2)
   {
      fprintf(stdout, "USAGE: %s <path to ROM file>\n", program_name);
      return(0);
   }

   // NOTE(law): Perform general dynamic allocations up front.
   struct memory_arena arena = {0};
   arena.size = MEBIBYTES(64);
   arena.base_address = mmap(0, arena.size, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);

   // NOTE(law) Set up the rendering bitmap.
   struct pixel_bitmap bitmap = {RESOLUTION_BASE_WIDTH, RESOLUTION_BASE_HEIGHT};

   size_t bytes_per_pixel = sizeof(u32);
   size_t bitmap_size = bitmap.width * bitmap.height * bytes_per_pixel;
   bitmap.memory = mmap(0, bitmap_size, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);

   linux_global_display = XOpenDisplay(0);
   Window window = linux_create_window(bitmap);

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
   sound.samples = mmap(0, sound.size, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);

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
