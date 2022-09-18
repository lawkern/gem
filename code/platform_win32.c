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
   free(file.memory);
   file.size = 0;
   file.memory = 0;
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
      free_file(result);
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

LRESULT
win32_window_callback(HWND window, UINT message, WPARAM wparam, LPARAM lparam)
{
   LRESULT result = 0;

   switch(message)
   {
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
            if(wparam == VK_ESCAPE || (alt_key_pressed && wparam == VK_F4))
            {
               win32_global_is_running = false;
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
   char *rom_path = command_line;
   log("Loading ROM at \"%s\"...\n", rom_path);

   Platform_File rom = load_file(rom_path);
   if(rom.size == 0)
   {
      return(1);
   }
   win32_global_rom = rom;

   log("Validating cartridge header...\n");
   if(!validate_cartridge_header(rom.memory, rom.size))
   {
      return(1);
   }

   Cartridge_Header *header = get_cartridge_header(rom.memory);

   // TODO(law): Implement Memory Bank Controllers.
   assert(header->ram_size == 0);

   register_pc = 0x100;
   unsigned char *memory_map = malloc(0xFFFF);
   memcpy(memory_map, rom.memory, rom.size);
   win32_global_memory_map = memory_map;

   /////////////////////////////////////////////////////////////////////////////

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
