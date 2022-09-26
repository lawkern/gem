/* /////////////////////////////////////////////////////////////////////////// */
/* (c) copyright 2022 Lawrence D. Kern /////////////////////////////////////// */
/* /////////////////////////////////////////////////////////////////////////// */

#include <windows.h>
#include <commctrl.h>
#include <dsound.h>
#include <stdarg.h>

#include "gem.c"

#define WIN32_SECONDS_ELAPSED(start, end) ((float)((end).QuadPart - (start).QuadPart) \
      / (float)win32_global_counts_per_second.QuadPart)

static bool win32_global_is_running;
static bool win32_global_is_paused;
static LARGE_INTEGER win32_global_counts_per_second;
static Monochrome_Color_Scheme win32_global_color_scheme;

static Memory_Arena *win32_global_arena;
static Platform_Bitmap *win32_global_bitmap;
static BITMAPINFO *win32_global_bitmap_info;

static HMENU win32_global_menu;
static HWND win32_global_status_bar;
static WINDOWPLACEMENT win32_global_previous_window_placement =
{
   sizeof(win32_global_previous_window_placement)
};

enum
{
   WIN32_MENU_FILE_OPEN = 9001,
   WIN32_MENU_FILE_EXIT,
   WIN32_MENU_VIEW_COLOR_DMG,
   WIN32_MENU_VIEW_COLOR_MGB,
   WIN32_MENU_VIEW_COLOR_LIGHT,
   WIN32_MENU_VIEW_RESOLUTION_1X,
   WIN32_MENU_VIEW_RESOLUTION_2X,
   WIN32_MENU_VIEW_RESOLUTION_4X,
   WIN32_MENU_VIEW_RESOLUTION_8X,
   WIN32_MENU_VIEW_FULLSCREEN,
   WIN32_STATUS_BAR,
};

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

   OutputDebugStringA(message);
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
   Platform_File result = {0};

   WIN32_FIND_DATAA file_data;
   HANDLE find_file = FindFirstFileA(file_path, &file_data);
   if(find_file == INVALID_HANDLE_VALUE)
   {
      platform_log("ERROR: Failed to find file \"%s\".\n", file_path);
      return(result);
   }
   FindClose(find_file);

   size_t size = (file_data.nFileSizeHigh * (MAXDWORD + 1)) + file_data.nFileSizeLow;
   result.memory = malloc(size);
   if(!result.memory)
   {
      platform_log("ERROR: Failed to allocate memory for file \"%s\".\n", file_path);
      return(result);
   }

   // NOTE(law): ReadFile is limited to reading 32-bit file sizes. As a result,
   // the Win32 platform can't actually use the full 64-bit size_t file size
   // defined in the non-platform code - it caps out at 4GB.

   HANDLE file = CreateFileA(file_path, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
   DWORD bytes_read;
   if(ReadFile(file, result.memory, (DWORD)size, &bytes_read, 0) && size == (size_t)bytes_read)
   {
      result.size = size;
   }
   else
   {
      platform_log("ERROR: Failed to read file \"%s.\"\n", file_path);
      platform_free_file(&result);
   }
   CloseHandle(file);

   return(result);
}

static void
win32_load_cartridge(Memory_Arena *arena, HWND window, char *file_path)
{
   load_cartridge(arena, file_path);

   HWND status_bar = GetDlgItem(window, WIN32_STATUS_BAR);
   SendMessage(status_bar, WM_SETTEXT, 0, (LPARAM)file_path);
}

static void
win32_open_file_dialog(Memory_Arena *arena, HWND window)
{

   // TODO(law): We want something better than MAX_PATH here.
   char file_name[MAX_PATH] = "";

   OPENFILENAME open_file_name = {0};
   open_file_name.lStructSize = sizeof(open_file_name);
   open_file_name.hwndOwner = window;
   open_file_name.lpstrFilter = "(*.gb)\0*.gb\0All Files (*.*)\0*.*\0";
   open_file_name.lpstrFile = file_name;
   open_file_name.nMaxFile = MAX_PATH;
   open_file_name.Flags = OFN_EXPLORER|OFN_PATHMUSTEXIST;
   open_file_name.lpstrDefExt = "gb";

   if(GetOpenFileName(&open_file_name))
   {
      win32_load_cartridge(arena, window, file_name);
   }
}

static void
win32_toggle_fullscreen(HWND window)
{
   // NOTE(law): Based on version by Raymond Chen:
   // https://devblogs.microsoft.com/oldnewthing/20100412-00/?p=14353

   // TODO(law): Check what this does with multiple monitors.
   DWORD style = GetWindowLong(window, GWL_STYLE);
   if(style & WS_OVERLAPPEDWINDOW)
   {
      MONITORINFO monitor_info = {sizeof(monitor_info)};

      if(GetWindowPlacement(window, &win32_global_previous_window_placement) &&
         GetMonitorInfo(MonitorFromWindow(window, MONITOR_DEFAULTTOPRIMARY), &monitor_info))
      {
         int x = monitor_info.rcMonitor.left;
         int y = monitor_info.rcMonitor.top;
         int width = monitor_info.rcMonitor.right - monitor_info.rcMonitor.left;
         int height = monitor_info.rcMonitor.bottom - monitor_info.rcMonitor.top;

         SetWindowLong(window, GWL_STYLE, style & ~WS_OVERLAPPEDWINDOW);
         SetWindowPos(window, HWND_TOP, x, y, width, height, SWP_NOOWNERZORDER|SWP_FRAMECHANGED);
      }

      // NOTE(law): Hide menu bar and status bar in fullscreen mode.
      SetMenu(window, 0);
      ShowWindow(win32_global_status_bar, SW_HIDE);
   }
   else
   {
      SetWindowLong(window, GWL_STYLE, style|WS_OVERLAPPEDWINDOW);
      SetWindowPlacement(window, &win32_global_previous_window_placement);
      SetWindowPos(window, 0, 0, 0, 0, 0, SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER|SWP_NOOWNERZORDER|SWP_FRAMECHANGED);

      // NOTE(law): Display menu bar and status bar again after exiting
      // fullscreen mode.
      SetMenu(window, win32_global_menu);
      ShowWindow(win32_global_status_bar, SW_SHOW);
   }
}

static bool
win32_is_fullscreen(HWND window)
{
   DWORD style = GetWindowLong(window, GWL_STYLE);

   bool result = !(style & WS_OVERLAPPEDWINDOW);
   return(result);
}

static unsigned int
win32_get_status_height(HWND window)
{
   unsigned int result = 0;

   // TODO(law): Determine if this query is too slow to process every frame.
   HWND status_bar = GetDlgItem(window, WIN32_STATUS_BAR);

   if(IsWindowVisible(status_bar))
   {
      RECT status_rect = {0};
      GetWindowRect(status_bar, &status_rect);

      result = status_rect.bottom - status_rect.top;
   }

   return(result);
}

static void
win32_set_resolution_scale(HWND window, unsigned int scale)
{
   // NOTE(law): Prevent updating the resolution if the window is currently in
   // fullscreen mode.
   if(!win32_is_fullscreen(window))
   {
      DWORD window_style = WS_OVERLAPPEDWINDOW;

      RECT window_rect = {0};
      window_rect.bottom = GEM_BASE_RESOLUTION_HEIGHT << scale;
      window_rect.right  = GEM_BASE_RESOLUTION_WIDTH  << scale;
      AdjustWindowRect(&window_rect, window_style, true);

      unsigned int window_width  = window_rect.right - window_rect.left;
      unsigned int window_height = window_rect.bottom - window_rect.top;

      unsigned int status_height = win32_get_status_height(window);
      window_height += status_height;

      SetWindowPos(window, 0, 0, 0, window_width, window_height, SWP_NOMOVE);
   }
}

static void
win32_display_bitmap(Platform_Bitmap bitmap, HWND window, HDC device_context)
{
   RECT client_rect;
   GetClientRect(window, &client_rect);

   int client_width = client_rect.right - client_rect.left;
   int client_height = client_rect.bottom - client_rect.top;

   unsigned int status_height = win32_get_status_height(window);
   client_height -= status_height;

   float client_aspect_ratio = (float)client_width / (float)client_height;
   float target_aspect_ratio = (float)GEM_BASE_RESOLUTION_WIDTH / (float)GEM_BASE_RESOLUTION_HEIGHT;

   int target_width  = client_width;
   int target_height = client_height;
   int gutter_width  = 0;
   int gutter_height = 0;

   if(client_aspect_ratio > target_aspect_ratio)
   {
      // NOTE(law): The window is too wide, fill in the left and right sides
      // with black gutters.

      target_width = (int)(target_aspect_ratio * (float)client_height);
      target_height = client_height;
      gutter_width = (client_width - target_width) / 2;

      PatBlt(device_context, 0, 0, gutter_width, target_height, BLACKNESS);
      PatBlt(device_context, client_width - gutter_width, 0, gutter_width, target_height, BLACKNESS);
   }
   else if(client_aspect_ratio < target_aspect_ratio)
   {
      // NOTE(law): The window is too tall, fill in the top and bottom with
      // black gutters.

      target_width = client_width;
      target_height = (int)((1.0f / target_aspect_ratio) * (float)client_width);
      gutter_height = (client_height - target_height) / 2;

      PatBlt(device_context, 0, 0, target_width, gutter_height, BLACKNESS);
      PatBlt(device_context, 0, client_height - gutter_height, target_width, gutter_height, BLACKNESS);
   }

   StretchDIBits(device_context,
                 gutter_width, gutter_height, target_width, target_height, // Destination
                 0, 0, bitmap.width, bitmap.height, // Source
                 bitmap.memory, win32_global_bitmap_info, DIB_RGB_COLORS, SRCCOPY);
}

typedef struct
{
   DWORD index;
   DWORD buffer_size;
   LPDIRECTSOUNDBUFFER buffer;
} Win32_Sound_Output;

static void
win32_clear_sound_buffer(Win32_Sound_Output *output)
{
   VOID *region1;
   VOID *region2;
   DWORD size1;
   DWORD size2;
   if(IDirectSoundBuffer_Lock(output->buffer, 0, output->buffer_size, &region1, &size1, &region2, &size2, 0) != DS_OK)
   {
      platform_log("ERROR: DirectSound failed to lock the sound buffer.\n");
      return;
   }

   output->index = 0;

   unsigned char *destination = (unsigned char *)region1;
   for(DWORD index = 0; index < size1; ++index)
   {
      *destination++ = 0;
   }

   destination = (unsigned char *)region2;
   for(DWORD index = 0; index < size2; ++index)
   {
      *destination++ = 0;
   }

   if(IDirectSoundBuffer_Unlock(output->buffer, region1, size1, region2, size2) != DS_OK)
   {
      platform_log("ERROR: DirectSound failed to unlock the sound buffer.\n");
      return;
   }
}

static void
win32_initialize_sound(Win32_Sound_Output *output, HWND window)
{
   ZeroMemory(output, sizeof(Win32_Sound_Output));

   HMODULE library = LoadLibraryA("dsound.dll");
   if(!library)
   {
      platform_log("ERROR: Windows failed to load dsound.dll.\n");
      return;
   }

   typedef HRESULT DSC(LPCGUID pcGuidDevice, LPDIRECTSOUND *ppDS, LPUNKNOWN pUnkOuter);
   DSC *DirectSoundCreate = (DSC *)GetProcAddress(library, "DirectSoundCreate");

   if(!DirectSoundCreate)
   {
      platform_log("ERROR: Windows failed to find the address of DirectSoundCreate.\n");
      return;
   }

   LPDIRECTSOUND direct_sound;
   if(DirectSoundCreate(0, &direct_sound, 0) != DS_OK)
   {
      platform_log("ERROR: DirectSound failed to initialize.\n");
      return;
   }

   if(IDirectSound_SetCooperativeLevel(direct_sound, window, DSSCL_PRIORITY) != DS_OK)
   {
      platform_log("ERROR: DirectSound failed to set the cooperative level.\n");
      return;
   }

   DSBUFFERDESC primary_description = {0};
   primary_description.dwSize = sizeof(primary_description);
   primary_description.dwFlags = DSBCAPS_PRIMARYBUFFER;

   LPDIRECTSOUNDBUFFER primary_buffer;
   if(IDirectSound_CreateSoundBuffer(direct_sound, &primary_description, &primary_buffer, 0) != DS_OK)
   {
      platform_log("ERROR: DirectSound failed to create the primary sound buffer.\n");
      return;
   }

   WAVEFORMATEX wave_format = {0};
   wave_format.wFormatTag = WAVE_FORMAT_PCM;
   wave_format.nChannels = SOUND_OUTPUT_CHANNEL_COUNT;
   wave_format.nSamplesPerSec = SOUND_OUTPUT_HZ;
   wave_format.wBitsPerSample = SOUND_OUTPUT_SAMPLE_SIZE * 8;
   wave_format.nBlockAlign = SOUND_OUTPUT_CHANNEL_COUNT * SOUND_OUTPUT_SAMPLE_SIZE;
   wave_format.nAvgBytesPerSec = wave_format.nSamplesPerSec * wave_format.nBlockAlign;

   if(IDirectSoundBuffer_SetFormat(primary_buffer, &wave_format) != DS_OK)
   {
      platform_log("ERROR: DirectSound failed to set the primary sound buffer's format.\n");
      return;
   }

   DSBUFFERDESC secondary_description = {0};
   secondary_description.dwSize = sizeof(secondary_description);
   secondary_description.dwFlags = DSBCAPS_GLOBALFOCUS;
   secondary_description.dwBufferBytes = SOUND_OUTPUT_HZ * SOUND_OUTPUT_SAMPLE_SIZE * SOUND_OUTPUT_CHANNEL_COUNT;
   secondary_description.dwReserved = 0;
   secondary_description.lpwfxFormat = &wave_format;

   LPDIRECTSOUNDBUFFER sound_buffer;
   if(IDirectSound_CreateSoundBuffer(direct_sound, &secondary_description, &sound_buffer, 0) != DS_OK)
   {
      platform_log("ERROR: DirectSound failed to create the secondary sound buffer.\n");
      return;
   }

   // NOTE(law): Fill out the return struct.
   output->buffer_size = secondary_description.dwBufferBytes;
   output->buffer = sound_buffer;

   // NOTE(law): Clear the sound buffer and start playing.
   win32_clear_sound_buffer(output);
   if(IDirectSoundBuffer_Play(output->buffer, 0, 0, DSBPLAY_LOOPING) != DS_OK)
   {
      platform_log("WARNING: DirectSound failed to start playing sound.\n");
      return;
   }
}

static DWORD
win32_get_sound_write_size(Win32_Sound_Output *output)
{
   DWORD result = 0;

   DWORD play_cursor;
   DWORD unused; // write_cursor
   if(IDirectSoundBuffer_GetCurrentPosition(output->buffer, &play_cursor, &unused) != DS_OK)
   {
      platform_log("ERROR: DirectSound failed to find the play and write cursors.\n");
      return(result);
   }

   // TODO(law): Reduce the latency here!
   DWORD target_cursor = play_cursor;

   output->index %= output->buffer_size;
   if(target_cursor < output->index)
   {
      result = (output->buffer_size - output->index) + target_cursor;
   }
   else
   {
      result = target_cursor - output->index;
   }

   return(result);
}

static void
win32_output_sound_samples(Win32_Sound_Output *output, signed short *samples, DWORD write_size)
{
   VOID *region1;
   VOID *region2;

   DWORD size1;
   DWORD size2;

   if(IDirectSoundBuffer_Lock(output->buffer, output->index, write_size, &region1, &size1, &region2, &size2, 0) != DS_OK)
   {
      platform_log("ERROR: DirectSound failed to lock the sound buffer.\n");
      return;
   }

   signed short *source = samples;
   signed short *destination = (signed short *)region1;

   DWORD sample_count = size1 / (SOUND_OUTPUT_SAMPLE_SIZE * SOUND_OUTPUT_CHANNEL_COUNT);
   for(DWORD sample_index = 0; sample_index < sample_count; ++sample_index)
   {
      // NOTE(law): Channel 0
      *destination++ = *source++;
      output->index += SOUND_OUTPUT_SAMPLE_SIZE;

      // NOTE(law): Channel 1
      *destination++ = *source++;
      output->index += SOUND_OUTPUT_SAMPLE_SIZE;
   }

   destination = (signed short *)region2;

   sample_count = size2 / (SOUND_OUTPUT_SAMPLE_SIZE * SOUND_OUTPUT_CHANNEL_COUNT);
   for(DWORD sample_index = 0; sample_index < sample_count; ++sample_index)
   {
      // NOTE(law): Channel 0
      *destination++ = *source++;
      output->index += SOUND_OUTPUT_SAMPLE_SIZE;

      // NOTE(law): Channel 1
      *destination++ = *source++;
      output->index += SOUND_OUTPUT_SAMPLE_SIZE;
   }

   if(IDirectSoundBuffer_Unlock(output->buffer, region1, size1, region2, size2) != DS_OK)
   {
      platform_log("ERROR: DirectSound failed to unlock the sound buffer.\n");
      return;
   }
}

LRESULT
win32_window_callback(HWND window, UINT message, WPARAM wparam, LPARAM lparam)
{
   LRESULT result = 0;

   switch(message)
   {
      case WM_CREATE:
      {
         // NOTE(law): Create window menu bar.
         win32_global_menu = CreateMenu();

         HMENU file_menu = CreatePopupMenu();
         AppendMenu(file_menu, MF_STRING, WIN32_MENU_FILE_OPEN, "&Open ROM\tCtrl+O");
         AppendMenu(file_menu, MF_SEPARATOR, 0, 0);
         AppendMenu(file_menu, MF_STRING, WIN32_MENU_FILE_EXIT, "E&xit\tAlt+F4");
         AppendMenu(win32_global_menu, MF_STRING|MF_POPUP, (UINT_PTR)file_menu, "&File");

         HMENU view_menu = CreatePopupMenu();

         AppendMenu(view_menu, MF_STRING|MF_CHECKED, WIN32_MENU_VIEW_COLOR_DMG, "Dot Matrix Color Scheme");
         AppendMenu(view_menu, MF_STRING, WIN32_MENU_VIEW_COLOR_MGB, "Pocket Color Scheme");
         AppendMenu(view_menu, MF_STRING, WIN32_MENU_VIEW_COLOR_LIGHT, "Light Color Scheme");
         AppendMenu(view_menu, MF_SEPARATOR, 0, 0);
         AppendMenu(view_menu, MF_STRING, WIN32_MENU_VIEW_RESOLUTION_1X, "&1x Resolution (160 x 144)\t1");
         AppendMenu(view_menu, MF_STRING, WIN32_MENU_VIEW_RESOLUTION_2X, "&2x Resolution (320 x 288)\t2");
         AppendMenu(view_menu, MF_STRING, WIN32_MENU_VIEW_RESOLUTION_4X, "&4x Resolution (640 x 576)\t4");
         AppendMenu(view_menu, MF_STRING, WIN32_MENU_VIEW_RESOLUTION_8X, "&8x Resolution (1280 x 1152)\t8");
         AppendMenu(view_menu, MF_SEPARATOR, 0, 0);
         AppendMenu(view_menu, MF_STRING, WIN32_MENU_VIEW_FULLSCREEN, "Toggle &Fullscreen\tAlt-Enter");
         AppendMenu(win32_global_menu, MF_STRING|MF_POPUP, (UINT_PTR)view_menu, "&View");

         SetMenu(window, win32_global_menu);

         // NOTE(law): Create window status bar.
         win32_global_status_bar = CreateWindowA(STATUSCLASSNAME, 0, WS_CHILD|WS_VISIBLE, 0, 0, 0, 0,
                                                 window, (HMENU)WIN32_STATUS_BAR, GetModuleHandle(0), 0);
      } break;

      case WM_SIZE:
      {
         HWND status_bar = GetDlgItem(window, WIN32_STATUS_BAR);
         SendMessage(status_bar, WM_SIZE, 0, 0);
      }

      case WM_COMMAND:
      {
         switch(LOWORD(wparam))
         {
            case WIN32_MENU_FILE_OPEN:
            {
               win32_open_file_dialog(win32_global_arena, window);
            } break;

            case WIN32_MENU_FILE_EXIT:
            {
               win32_global_is_running = false;
               PostQuitMessage(0);
            } break;

            case WIN32_MENU_VIEW_COLOR_DMG:
            {
               win32_global_color_scheme = MONOCHROME_COLOR_OPTION_DMG;
               CheckMenuRadioItem(win32_global_menu,
                                  WIN32_MENU_VIEW_COLOR_DMG,
                                  WIN32_MENU_VIEW_COLOR_LIGHT,
                                  WIN32_MENU_VIEW_COLOR_DMG,
                                  MF_BYCOMMAND);
            } break;

            case WIN32_MENU_VIEW_COLOR_MGB:
            {
               win32_global_color_scheme = MONOCHROME_COLOR_OPTION_MGB;
               CheckMenuRadioItem(win32_global_menu,
                                  WIN32_MENU_VIEW_COLOR_DMG,
                                  WIN32_MENU_VIEW_COLOR_LIGHT,
                                  WIN32_MENU_VIEW_COLOR_MGB,
                                  MF_BYCOMMAND);
            } break;

            case WIN32_MENU_VIEW_COLOR_LIGHT:
            {
               win32_global_color_scheme = MONOCHROME_COLOR_OPTION_LIGHT;
               CheckMenuRadioItem(win32_global_menu,
                                  WIN32_MENU_VIEW_COLOR_DMG,
                                  WIN32_MENU_VIEW_COLOR_LIGHT,
                                  WIN32_MENU_VIEW_COLOR_LIGHT,
                                  MF_BYCOMMAND);
            } break;

            case WIN32_MENU_VIEW_RESOLUTION_1X:
            {
               win32_set_resolution_scale(window, 0);
            } break;

            case WIN32_MENU_VIEW_RESOLUTION_2X:
            {
               win32_set_resolution_scale(window, 1);
            } break;

            case WIN32_MENU_VIEW_RESOLUTION_4X:
            {
               win32_set_resolution_scale(window, 2);
            } break;

            case WIN32_MENU_VIEW_RESOLUTION_8X:
            {
               win32_set_resolution_scale(window, 3);
            } break;

            case WIN32_MENU_VIEW_FULLSCREEN:
            {
               win32_toggle_fullscreen(window);
            } break;
         }
      } break;

      case WM_CLOSE:
      {
         DestroyWindow(window);
      } break;

      case WM_DESTROY:
      {
         win32_global_is_running = false;
         PostQuitMessage(0);
      } break;

      case WM_SYSKEYDOWN:
      case WM_SYSKEYUP:
      case WM_KEYDOWN:
      case WM_KEYUP:
      {
         bool alt_key_pressed = lparam & (1 << 29);
         bool key_previously_down = lparam & (1 << 30);

         if(!key_previously_down)
         {
            if(alt_key_pressed && wparam == VK_F4)
            {
               win32_global_is_running = false;
            }
            else if(alt_key_pressed && wparam == VK_RETURN)
            {
               win32_toggle_fullscreen(window);
            }
            else if(wparam == '1')
            {
               win32_set_resolution_scale(window, 0);
            }
            else if(wparam == '2')
            {
               win32_set_resolution_scale(window, 1);
            }
            else if(wparam == '4')
            {
               win32_set_resolution_scale(window, 2);
            }
            else if(wparam == '8')
            {
               win32_set_resolution_scale(window, 3);
            }
            else if(wparam == 'O')
            {
               SHORT ctrl_state = GetKeyState(VK_CONTROL);
               bool ctrl_key_pressed = (ctrl_state & 0x8000);

               if(ctrl_key_pressed)
               {
                  win32_open_file_dialog(win32_global_arena, window);
               }
            }
            else if(wparam == 'H')
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
            else if(wparam == 'P')
            {
               win32_global_is_paused = !win32_global_is_paused;
            }
            else if(wparam == 'C')
            {
               win32_global_color_scheme++;
               if(win32_global_color_scheme >= MONOCHROME_COLOR_OPTION_COUNT)
               {
                  win32_global_color_scheme = 0;
               }

               WPARAM param = WIN32_MENU_VIEW_COLOR_DMG + win32_global_color_scheme;
               SendMessage(window, WM_COMMAND, param, 0);
            }
            else if(wparam == 'D')
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
            else if(wparam == 'N')
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
            else
            {
               result = DefWindowProc(window, message, wparam, lparam);
            }
         }
         else
         {
            result = DefWindowProc(window, message, wparam, lparam);
         }
      } break;

      case WM_PAINT:
      {
         PAINTSTRUCT paint;
         HDC device_context = BeginPaint(window, &paint);

         Platform_Bitmap bitmap = *win32_global_bitmap;
         win32_display_bitmap(bitmap, window, device_context);

         ReleaseDC(window, device_context);
      } break;

      default:
      {
         result = DefWindowProc(window, message, wparam, lparam);
      } break;
   }

   return(result);
}

static void *
win32_allocate(SIZE_T size)
{
   VOID *result = VirtualAlloc(0, size, MEM_COMMIT|MEM_RESERVE, PAGE_READWRITE);
   return(result);
}

int
WinMain(HINSTANCE instance, HINSTANCE previous_instance, LPSTR command_line, int show_command)
{
   (void)previous_instance;

   InitCommonControls();
   QueryPerformanceFrequency(&win32_global_counts_per_second);

   WNDCLASSEXA window_class = {0};
   window_class.cbSize = sizeof(window_class);
   window_class.style = CS_HREDRAW|CS_VREDRAW;
   window_class.lpfnWndProc = win32_window_callback;
   window_class.hInstance = instance;
   window_class.hIcon = LoadIcon(instance, MAKEINTRESOURCE(WIN32_ICON));
   window_class.hIconSm = LoadImage(instance, MAKEINTRESOURCE(WIN32_ICON), IMAGE_ICON, 16, 16, 0);
   window_class.hCursor = LoadCursorA(0, IDC_ARROW);
   window_class.lpszClassName = "Game_Boy_Emulator_GEM";

   if(!RegisterClassExA(&window_class))
   {
      platform_log("ERROR: Windows failed to register a window class.\n");
      return(1);
   }

   HWND window = CreateWindowExA(0,
                                 window_class.lpszClassName,
                                 "Game Boy Emulator (GEM)",
                                 WS_OVERLAPPEDWINDOW,
                                 CW_USEDEFAULT,
                                 CW_USEDEFAULT,
                                 CW_USEDEFAULT,
                                 CW_USEDEFAULT,
                                 0,
                                 0,
                                 instance,
                                 0);

   if(!window)
   {
      platform_log("ERROR: Windows failed to create a window.\n");
      return(1);
   }

   // NOTE(law): Perform general dynamic allocations up front.
   Memory_Arena arena = {0};
   arena.size = MEBIBYTES(64);
   arena.base_address = win32_allocate(arena.size);
   if(!arena.base_address)
   {
      platform_log("ERROR: Windows failed to allocate our memory arena.\n");
      return(1);
   }

   win32_global_arena = &arena;

   // NOTE(law) Set up the rendering bitmap.
   Platform_Bitmap bitmap = {GEM_BASE_RESOLUTION_WIDTH, GEM_BASE_RESOLUTION_HEIGHT};

   SIZE_T bytes_per_pixel = sizeof(unsigned int);
   SIZE_T bitmap_size = bitmap.width * bitmap.height * bytes_per_pixel;
   bitmap.memory = win32_allocate(bitmap_size);
   if(!bitmap.memory)
   {
      platform_log("ERROR: Windows failed to allocate our bitmap.\n");
      return(1);
   }

   BITMAPINFOHEADER bitmap_header = {0};
   bitmap_header.biSize = sizeof(BITMAPINFOHEADER);
   bitmap_header.biWidth = bitmap.width;
   bitmap_header.biHeight = -(signed int)bitmap.height; // NOTE(law): Negative will indicate a top-down bitmap.
   bitmap_header.biPlanes = 1;
   bitmap_header.biBitCount = 32;
   bitmap_header.biCompression = BI_RGB;

   BITMAPINFO bitmap_info = {bitmap_header};

   win32_global_bitmap = &bitmap;
   win32_global_bitmap_info = &bitmap_info;

   // NOTE(law): Display the created window.
   ShowWindow(window, show_command);
   UpdateWindow(window);

   // NOTE(law): Set the window size after creating and showing the window to
   // ensure the status bar both exists and its parent window is visible (for
   // the IsWindowVisible check during height calculation). This allows the
   // status bar height to be properly accounted for and prevents it from
   // occluding some of the client area we intend to use.
   win32_set_resolution_scale(window, 1);

   // NOTE(law): Initialize sound.
   Win32_Sound_Output sound_output;
   win32_initialize_sound(&sound_output, window);

   signed short *samples = win32_allocate(sound_output.buffer_size);
   if(!samples)
   {
      platform_log("ERROR: Windows failed to allocate our sound samples.\n");
      return(1);
   }

   // NOTE(law): Attempt to load a ROM in case a path to one was provided as the
   // command line argument.
    win32_load_cartridge(&arena, window, command_line);

   float target_seconds_per_frame = 1.0f / 59.7f;
   float frame_seconds_elapsed = 0;

   clear(&bitmap, win32_global_color_scheme);

   LARGE_INTEGER frame_start_count;
   QueryPerformanceCounter(&frame_start_count);

   win32_global_is_running = true;
   while(win32_global_is_running)
   {
      MSG message;
      while(PeekMessage(&message, 0, 0, 0, PM_REMOVE))
      {
         TranslateMessage(&message);
         DispatchMessage(&message);
      }

      if(!win32_global_is_paused && map.load_complete)
      {
         // NOTE(law): Just loop over VRAM and display the contents as tiles.
         static int tile_offset = 0;
         dump_vram(&bitmap, tile_offset++, PALETTE_DATA_BG, win32_global_color_scheme);

         if(tile_offset >= 512 || register_pc == 0)
         {
            tile_offset = 0;
         }
      }

      // NOTE(law): Blit bitmap to screen.
      HDC device_context = GetDC(window);
      win32_display_bitmap(bitmap, window, device_context);
      ReleaseDC(window, device_context);

      // NOTE(law): Output sound.
      DWORD sound_write_size = win32_get_sound_write_size(&sound_output);
      unsigned int sample_count = sound_write_size / (SOUND_OUTPUT_SAMPLE_SIZE * SOUND_OUTPUT_CHANNEL_COUNT);

      generate_sound_samples(samples, sample_count);
      win32_output_sound_samples(&sound_output, samples, sound_write_size);

      // NOTE(law): Calculate elapsed frame time.
      LARGE_INTEGER frame_end_count;
      QueryPerformanceCounter(&frame_end_count);

      unsigned int instructions_executed = 0;
      frame_seconds_elapsed = WIN32_SECONDS_ELAPSED(frame_start_count, frame_end_count);

      while(frame_seconds_elapsed < target_seconds_per_frame)
      {
         if(!win32_global_is_paused && map.load_complete)
         {
            if(!map.boot_complete && read_memory(0xFF50))
            {
               map.boot_complete = true;
            }

            handle_interrupts();
            fetch_and_execute();

            instructions_executed++;
         }

         QueryPerformanceCounter(&frame_end_count);
         frame_seconds_elapsed = WIN32_SECONDS_ELAPSED(frame_start_count, frame_end_count);
      }
      frame_start_count = frame_end_count;

      float average_us = (frame_seconds_elapsed * 1000.0f * 1000.0f) / (float)instructions_executed;
      platform_log("Frame time: %0.03fms, ", frame_seconds_elapsed * 1000.0f);
      platform_log("Average instruction time: %0.03fus\n", average_us);
   }

   return(0);
}
