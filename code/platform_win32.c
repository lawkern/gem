/* /////////////////////////////////////////////////////////////////////////// */
/* (c) copyright 2022 Lawrence D. Kern /////////////////////////////////////// */
/* /////////////////////////////////////////////////////////////////////////// */

#include <windows.h>
#include <commctrl.h>
#include <stdarg.h>

#include "gem.c"

typedef struct
{
   size_t size;
   unsigned char *memory;
} Win32_File;

#define WIN32_SECONDS_ELAPSED(start, end) ((float)((end).QuadPart - (start).QuadPart) \
      / (float)win32_global_counts_per_second.QuadPart)

static bool win32_global_is_running;
static bool win32_global_is_paused;
static Win32_File win32_global_rom;
static unsigned char *win32_global_memory_map;
static LARGE_INTEGER win32_global_counts_per_second;

static BITMAPINFO win32_global_bitmap_info;
static Platform_Bitmap win32_global_bitmap;

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

static void
win32_free_file(Win32_File *file)
{
   free(file->memory);
   file->size = 0;
   file->memory = 0;
}

static Win32_File
win32_load_file(char *file_path)
{
   Win32_File result = {0};

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

   HANDLE file = CreateFileA(file_path, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
   DWORD bytes_read;
   if(ReadFile(file, result.memory, size, &bytes_read, 0) && size == bytes_read)
   {
      result.size = size;
   }
   else
   {
      platform_log("ERROR: Failed to read file \"%s.\"\n", file_path);
      win32_free_file(&result);
   }
   CloseHandle(file);

   return(result);
}

static unsigned char *
win32_create_memory_map(unsigned char *cartridge_memory)
{
   unsigned char *result = malloc(0x10000);

   Cartridge_Header *header = get_cartridge_header(cartridge_memory);
   ZeroMemory(result, 0x10000);

   // TODO(law): Support the remaining cartridge types!
   switch(header->cartridge_type)
   {
      case 0x00: // ROM ONLY
      {
         // NOTE(law): The cartridge contains 32KiB of ROM, which can be mapped
         // directly into 0x0000 to 0x7FFF.

         memcpy(result, cartridge_memory, 0x8000);
      } break;

      default:
      {
         assert(!"UNHANDLED CARTRIDGE TYPE");
      } break;
   }

   // TODO(law): Set the boot ROM up in such a way that it can be unmapped after
   // it is executed.
   memcpy(result, boot_rom, sizeof(boot_rom));

   return(result);
}

static void
win32_load_rom(HWND window, char *rom_path)
{
   platform_log("Loading ROM at \"%s\"...\n", rom_path);
   Win32_File rom = win32_load_file(rom_path);

   if(!rom.memory)
   {
      platform_log("ERROR: The loaded ROM was not updated\n", rom_path);
      return;
   }

   platform_log("Validating cartridge header...\n");
   if(!validate_cartridge_header(rom.memory, rom.size))
   {
      platform_log("ERROR: The loaded ROM was not updated\n", rom_path);
      win32_free_file(&rom);
      return;
   }

   if(rom.size < (0x100 + sizeof(Cartridge_Header)))
   {
      platform_log("ERROR: The loaded ROM was too small to contain a full header.\n", rom_path);
      win32_free_file(&rom);
      return;
   }

   Cartridge_Header *header = get_cartridge_header(rom.memory);

   // TODO(law): Implement Memory Bank Controllers.
   if(header->ram_size != 0)
   {
      platform_log("ERROR: The Memory Bank Controller used by the loaded ROM is not supported.\n");
      win32_free_file(&rom);
      return;
   }

   if(win32_global_rom.memory)
   {
      win32_free_file(&win32_global_rom);
   }

   if(win32_global_memory_map)
   {
      free(win32_global_memory_map);
      win32_global_memory_map = 0;
   }

   win32_global_rom = rom;
   win32_global_memory_map = win32_create_memory_map(rom.memory);

   // TODO(law): This value refers the current horizontal line, and a value of
   // 0x90 represents the beginning of a VBlank period. Since the boot ROM waits
   // on VBlank for the logo processing, just hard code it until rendering is
   // actually handled.
   win32_global_memory_map[0xFF44] = 0x90;

   HWND status_bar = GetDlgItem(window, WIN32_STATUS_BAR);
   SendMessage(status_bar, WM_SETTEXT, 0, (LPARAM)rom_path);
}

static void
win32_open_file_dialog(HWND window)
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
      win32_load_rom(window, file_name);
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

      if (GetWindowPlacement(window, &win32_global_previous_window_placement) &&
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
win32_display_bitmap(HWND window, HDC device_context)
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
                 0, 0, win32_global_bitmap.width, win32_global_bitmap.height, // Source
                 win32_global_bitmap.memory,
                 &win32_global_bitmap_info,
                 DIB_RGB_COLORS,
                 SRCCOPY);
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
               win32_open_file_dialog(window);
            } break;

            case WIN32_MENU_FILE_EXIT:
            {
               win32_global_is_running = false;
               PostQuitMessage(0);
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
                  win32_open_file_dialog(window);
               }
            }
            else if(wparam == 'H')
            {
               // NOTE(law): Print the contents of the cartridge header.
               if(win32_global_rom.memory)
               {
                  dump_cartridge_header(win32_global_rom.memory);
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
            else if(wparam == 'D')
            {
               // NOTE(law): Print the disassembly.
               if(win32_global_rom.memory)
               {
                  platform_log("Parsing instruction stream...\n");
                  disassemble_stream(win32_global_memory_map, 0, 0x10000);
               }
               else
               {
                  platform_log("No cartridge is currently loaded.\n");
               }
            }
            else if(wparam == 'N')
            {
               // NOTE(law): Fetch and execute the next instruction.
               if(win32_global_rom.memory)
               {
                  platform_log("Fetching and executing instruction...\n");
                  disassemble_instruction(win32_global_memory_map, register_pc);

                  handle_interrupts(win32_global_memory_map);
                  fetch_and_execute(win32_global_memory_map);
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

         win32_display_bitmap(window, device_context);

         ReleaseDC(window, device_context);
      } break;

      default:
      {
         result = DefWindowProc(window, message, wparam, lparam);
      } break;
   }

   return(result);
}

int
WinMain(HINSTANCE instance, HINSTANCE previous_instance, LPSTR command_line, int show_command)
{
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
      platform_log("ERROR: Failed to register a window class.\n");
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
      platform_log("ERROR: Failed to create a window.\n");
      return(1);
   }

   // NOTE(law) Set up the rendering bitmap.
   Platform_Bitmap bitmap = {GEM_BASE_RESOLUTION_WIDTH, GEM_BASE_RESOLUTION_HEIGHT};

   SIZE_T bytes_per_pixel = sizeof(unsigned int);
   SIZE_T bitmap_size = bitmap.width * bitmap.height * bytes_per_pixel;
   bitmap.memory = VirtualAlloc(0, bitmap_size, MEM_COMMIT|MEM_RESERVE, PAGE_READWRITE);

   BITMAPINFOHEADER bitmap_header = {0};
   bitmap_header.biSize = sizeof(BITMAPINFOHEADER);
   bitmap_header.biWidth = bitmap.width;
   bitmap_header.biHeight = -bitmap.height; // NOTE(law): Negative will indicate a top-down bitmap.
   bitmap_header.biPlanes = 1;
   bitmap_header.biBitCount = 32;
   bitmap_header.biCompression = BI_RGB;

   BITMAPINFO bitmap_info = {bitmap_header};

   win32_global_bitmap = bitmap;
   win32_global_bitmap_info = bitmap_info;

   // NOTE(law): Display the created window.
   ShowWindow(window, show_command);
   UpdateWindow(window);

   // NOTE(law): Set the window size after creating and showing the window to
   // ensure the status bar both exists and its parent window is visible (for
   // the IsWindowVisible check during height calculation). This allows the
   // status bar height to be properly accounted for and prevents it from
   // occluding some of the client area we intend to use.
   win32_set_resolution_scale(window, 1);

   // NOTE(law): Attempt to load a ROM in case a path to one was provided as the
   // command line argument.
   win32_load_rom(window, command_line);

   LARGE_INTEGER frame_start_count;
   QueryPerformanceCounter(&frame_start_count);

   float seconds_elapsed = 0;
   unsigned int instructions_executed = 0;

   win32_global_is_running = true;
   while(win32_global_is_running)
   {
      MSG message;
      while(PeekMessage(&message, 0, 0, 0, PM_REMOVE))
      {
         TranslateMessage(&message);
         DispatchMessage(&message);
      }

      if(!win32_global_is_paused && win32_global_memory_map)
      {
         handle_interrupts(win32_global_memory_map);
         fetch_and_execute(win32_global_memory_map);
      }

      // NOTE(law): Draw pixels into bitmap.
      for(unsigned int y = 0; y < bitmap.height; ++y)
      {
         for(unsigned int x = 0; x < bitmap.width; ++x)
         {
            bitmap.memory[(bitmap.width * y) + x] = 0xFFE0F8D0;
         }
      }

      // NOTE(law): Blit bitmap to screen.
      HDC device_context = GetDC(window);
      win32_display_bitmap(window, device_context);
      ReleaseDC(window, device_context);

      // NOTE(law): Calculate elapsed frame time.
      LARGE_INTEGER frame_end_count;
      QueryPerformanceCounter(&frame_end_count);

      seconds_elapsed += WIN32_SECONDS_ELAPSED(frame_start_count, frame_end_count);
      frame_start_count = frame_end_count;

      if(instructions_executed++ == 10000)
      {
         float average_us = (seconds_elapsed / (float)instructions_executed) * 1000.0f * 1000.0f;
         platform_log("Average instruction time: %0.03fus\n", average_us);

         seconds_elapsed = 0;
         instructions_executed = 0;
      }
   }

   return(0);
}
