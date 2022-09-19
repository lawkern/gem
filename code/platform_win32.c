/* /////////////////////////////////////////////////////////////////////////// */
/* (c) copyright 2022 Lawrence D. Kern /////////////////////////////////////// */
/* /////////////////////////////////////////////////////////////////////////// */

#include <windows.h>
#include <stdarg.h>

#include "gem.c"

static bool win32_global_is_running = true;
static Platform_File win32_global_rom;
static unsigned char *win32_global_memory_map;

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
      fprintf(stderr, "ERROR: Failed to find file \"%s\".\n", file_path);
      return(result);
   }
   FindClose(find_file);

   size_t size = (file_data.nFileSizeHigh * (MAXDWORD + 1)) + file_data.nFileSizeLow;
   result.memory = malloc(size);
   if(!result.memory)
   {
      fprintf(stderr, "ERROR: Failed to allocate memory for file \"%s\".\n", file_path);
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
      fprintf(stderr, "Failed to read file \"%s.\"\n", file_path);
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
win32_load_rom(char *rom_path)
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

   Cartridge_Header *header = get_cartridge_header(rom.memory);

   // TODO(law): Implement Memory Bank Controllers.
   if(header->ram_size != 0);
   {
      log("ERROR: The Memory Bank Controller used by the loaded ROM is not supported.\n");
      free_file(&rom);
      return;
   }

   if(win32_global_rom.memory)
   {
      free_file(&win32_global_rom);
   }

   // TODO(law): Load boot ROM and start read from address 0x0000;
   register_pc = 0x100;

   // TODO(law): Additional memory size will be needed to support multiple Memory
   // Banks.
   unsigned char *memory_map = malloc(0xFFFF);
   memcpy(memory_map, rom.memory, rom.size);

   win32_global_rom = rom;
   win32_global_memory_map = memory_map;
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
      win32_load_rom(file_name);
   }
}

enum
{
   WIN32_MENU_FILE_OPEN = 9001,
   WIN32_MENU_FILE_EXIT,
};

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

         SetMenu(window, menu);
      } break;

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

   DWORD window_style = WS_OVERLAPPEDWINDOW;

   RECT window_rect = {0};
   window_rect.bottom = 144 << 1;
   window_rect.right  = 160 << 1;
   AdjustWindowRect(&window_rect, window_style, false);

   unsigned int window_width  = window_rect.right - window_rect.left;
   unsigned int window_height = window_rect.bottom - window_rect.top;

   HWND window = CreateWindowA(window_class.lpszClassName,
                               "Game Boy Emulator (GEM)",
                               window_style,
                               CW_USEDEFAULT,
                               CW_USEDEFAULT,
                               window_width,
                               window_height,
                               0,
                               0,
                               instance,
                               0);

   if(!window)
   {
      log("ERROR: Failed to create a window.\n");
      return(1);
   }

   ShowWindow(window, show_command);
   UpdateWindow(window);

   // NOTE(law): Attempt to load a ROM in case a path to one was provided as the
   // command line argument.
   win32_load_rom(command_line);

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
