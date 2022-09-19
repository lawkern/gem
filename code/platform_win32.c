/* /////////////////////////////////////////////////////////////////////////// */
/* (c) copyright 2022 Lawrence D. Kern /////////////////////////////////////// */
/* /////////////////////////////////////////////////////////////////////////// */

#include <windows.h>
#include <commctrl.h>
#include <stdarg.h>

#include "gem.c"

static bool win32_global_is_running = true;
static Platform_File win32_global_rom;
static unsigned char *win32_global_memory_map;
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
PLATFORM_FREE_FILE(free_file)
{
   free(file->memory);
   file->size = 0;
   file->memory = 0;
}

static
PLATFORM_LOAD_FILE(load_file)
{
   Platform_File result = {0};

   WIN32_FIND_DATAA file_data;
   HANDLE find_file = FindFirstFileA(file_path, &file_data);
   if(find_file == INVALID_HANDLE_VALUE)
   {
      log("ERROR: Failed to find file \"%s\".\n", file_path);
      return(result);
   }
   FindClose(find_file);

   size_t size = (file_data.nFileSizeHigh * (MAXDWORD + 1)) + file_data.nFileSizeLow;
   result.memory = malloc(size);
   if(!result.memory)
   {
      log("ERROR: Failed to allocate memory for file \"%s\".\n", file_path);
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
      log("ERROR: Failed to read file \"%s.\"\n", file_path);
      free_file(&result);
   }
   CloseHandle(file);

   return(result);
}

static
PLATFORM_LOG(log)
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
win32_load_rom(HWND window, char *rom_path)
{
   log("Loading ROM at \"%s\"...\n", rom_path);
   Platform_File rom = load_file(rom_path);

   if(!rom.memory)
   {
      log("ERROR: The loaded ROM was not updated\n", rom_path);
      return;
   }

   log("Validating cartridge header...\n");
   if(!validate_cartridge_header(rom.memory, rom.size))
   {
      log("ERROR: The loaded ROM was not updated\n", rom_path);
      free_file(&rom);
      return;
   }

   if(rom.size < (0x100 + sizeof(Cartridge_Header)))
   {
      log("ERROR: The loaded ROM was too small to contain a full header.\n", rom_path);
      free_file(&rom);
      return;
   }

   Cartridge_Header *header = get_cartridge_header(rom.memory);

   // TODO(law): Implement Memory Bank Controllers.
   if(header->ram_size != 0)
   {
      log("ERROR: The Memory Bank Controller used by the loaded ROM is not supported.\n");
      free_file(&rom);
      return;
   }

   if(win32_global_rom.memory)
   {
      free_file(&win32_global_rom);
   }

   if(win32_global_memory_map)
   {
      free(win32_global_memory_map);
   }

   // TODO(law): Load boot ROM and start read from address 0x0000;
   register_pc = 0x100;

   // TODO(law): Additional memory size will be needed to support multiple Memory
   // Banks.
   size_t memory_map_size = 0xFFFF;
   unsigned char *memory_map = malloc(memory_map_size);
   memcpy(memory_map, rom.memory, memory_map_size);

   win32_global_rom = rom;
   win32_global_memory_map = memory_map;

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
   }
   else
   {
      SetWindowLong(window, GWL_STYLE, style|WS_OVERLAPPEDWINDOW);
      SetWindowPlacement(window, &win32_global_previous_window_placement);
      SetWindowPos(window, 0, 0, 0, 0, 0, SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER|SWP_NOOWNERZORDER|SWP_FRAMECHANGED);
   }
}

static unsigned int
win32_get_status_height(HWND window)
{
   // TODO(law): Determine if this query is too slow to process every frame.
   HWND status_bar = GetDlgItem(window, WIN32_STATUS_BAR);

   RECT status_rect = {0};
   GetWindowRect(status_bar, &status_rect);

   unsigned int result = status_rect.bottom - status_rect.top;
   return(result);
}

static void
win32_set_resolution_scale(HWND window, unsigned int scale)
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

   PatBlt(device_context, 0, 0, client_width, client_height, WHITENESS);

   if(client_aspect_ratio > target_aspect_ratio)
   {
      // NOTE(law): The window is too wide, fill in the left and right sides
      // with black gutters.

      int target_width = (int)(target_aspect_ratio * (float)client_height);
      int target_height = client_height;
      int gutter_width = (client_width - target_width) / 2;

      PatBlt(device_context, 0, 0, gutter_width, target_height, BLACKNESS);
      PatBlt(device_context, client_width - gutter_width, 0, gutter_width, target_height, BLACKNESS);
   }
   else if(client_aspect_ratio < target_aspect_ratio)
   {
      // NOTE(law): The window is too tall, fill in the top and bottom with
      // black gutters.

      int target_width = client_width;
      int target_height = (int)((1.0f / target_aspect_ratio) * (float)client_width);
      int gutter_height = (client_height - target_height) / 2;

      PatBlt(device_context, 0, 0, target_width, gutter_height, BLACKNESS);
      PatBlt(device_context, 0, client_height - gutter_height, target_width, gutter_height, BLACKNESS);
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
         HMENU menu = CreateMenu();

         HMENU file_menu = CreatePopupMenu();
         AppendMenu(file_menu, MF_STRING, WIN32_MENU_FILE_OPEN, "&Open ROM\tCtrl+O");
         AppendMenu(file_menu, MF_SEPARATOR, 0, 0);
         AppendMenu(file_menu, MF_STRING, WIN32_MENU_FILE_EXIT, "E&xit\tAlt+F4");
         AppendMenu(menu, MF_STRING|MF_POPUP, (UINT_PTR)file_menu, "&File");

         HMENU view_menu = CreatePopupMenu();
         AppendMenu(view_menu, MF_STRING, WIN32_MENU_VIEW_RESOLUTION_1X, "&1x Resolution (160 x 144)\t1");
         AppendMenu(view_menu, MF_STRING, WIN32_MENU_VIEW_RESOLUTION_2X, "&2x Resolution (320 x 288)\t2");
         AppendMenu(view_menu, MF_STRING, WIN32_MENU_VIEW_RESOLUTION_4X, "&4x Resolution (640 x 576)\t4");
         AppendMenu(view_menu, MF_STRING, WIN32_MENU_VIEW_RESOLUTION_8X, "&8x Resolution (1280 x 1152)\t8");
         AppendMenu(view_menu, MF_SEPARATOR, 0, 0);
         AppendMenu(view_menu, MF_STRING, WIN32_MENU_VIEW_FULLSCREEN, "Toggle &Fullscreen\tAlt-Enter");
         AppendMenu(menu, MF_STRING|MF_POPUP, (UINT_PTR)view_menu, "&View");

         SetMenu(window, menu);

         // NOTE(law): Create window status bar.
         CreateWindowA(STATUSCLASSNAME, 0, WS_CHILD|WS_VISIBLE, 0, 0, 0, 0,
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
                  log("No cartridge is currently loaded.\n");
               }
            }
            else if(wparam == 'D')
            {
               // NOTE(law): Print the disassembly.
               if(win32_global_rom.memory)
               {
                  log("Parsing instruction stream...\n");
                  disassemble_stream(win32_global_rom.memory, 0, win32_global_rom.size);
               }
               else
               {
                  log("No cartridge is currently loaded.\n");
               }
            }
            else if(wparam == 'N')
            {
               // NOTE(law): Fetch and execute the next instruction.
               if(win32_global_rom.memory)
               {
                  log("Fetching and executing instruction... ");

                  handle_interrupts(win32_global_memory_map);
                  fetch_and_execute(win32_global_memory_map);

                  log("Done.\n");
               }
               else
               {
                  log("No cartridge is currently loaded.\n");
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

   WNDCLASSA window_class = {0};
   window_class.style = CS_HREDRAW|CS_VREDRAW;
   window_class.lpfnWndProc = win32_window_callback;
   window_class.hInstance = instance;
   window_class.hIcon = LoadIcon(0, IDI_APPLICATION);
   window_class.hCursor = LoadCursorA(0, IDC_ARROW);
   window_class.lpszClassName = "Game_Boy_Emulator_GEM";

   if(!RegisterClassA(&window_class))
   {
      log("ERROR: Failed to register a window class.\n");
      return(1);
   }

   HWND window = CreateWindowA(window_class.lpszClassName,
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
      log("ERROR: Failed to create a window.\n");
      return(1);
   }

   // NOTE(law): Set the window size after window creation (but before showing
   // the window) so the status bar height can be accounted for. Otherwise the
   // status bar will occlude some of the client area, unlike the menu bar.
   win32_set_resolution_scale(window, 1);

   ShowWindow(window, show_command);
   UpdateWindow(window);

   // NOTE(law): Attempt to load a ROM in case a path to one was provided as the
   // command line argument.
   win32_load_rom(window, command_line);

   win32_global_is_running = true;
   while(win32_global_is_running)
   {
      MSG message;
      while(PeekMessage(&message, 0, 0, 0, PM_REMOVE))
      {
         TranslateMessage(&message);
         DispatchMessage(&message);
      }
   }

   return(0);
}
