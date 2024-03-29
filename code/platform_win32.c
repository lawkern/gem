/* /////////////////////////////////////////////////////////////////////////// */
/* (c) copyright 2022 Lawrence D. Kern /////////////////////////////////////// */
/* /////////////////////////////////////////////////////////////////////////// */

// TODO(law): While most of the internal win32 functions defined in this file
// now support both ANSI and wide character strings, the PLATFORM_*
// implementations (and any functions that work on the same strings) currently
// only support 8-bit ANSI strings. Odds are any eventual unicode support in the
// main code will not want to use Windows-style UTF-16. Not sure how much we
// care in the end, since this really only amounts to file paths and log
// messages.

// #define UNICODE 0
// #define _UNICODE 0

#include <windows.h>
#include <commctrl.h>
#include <dsound.h>
#include <stdarg.h>

#include "platform_win32.h"

#include "gem.c"

#define WIN32_SECONDS_ELAPSED(start, end) ((float)((end).QuadPart - (start).QuadPart) \
      / (float)win32_global_counts_per_second.QuadPart)

global bool win32_global_is_running;
global bool win32_global_is_paused;
global LARGE_INTEGER win32_global_counts_per_second;

global struct memory_arena *win32_global_arena;
global struct pixel_bitmap *win32_global_bitmap;
global BITMAPINFO *win32_global_bitmap_info;

#define WIN32_DEFAULT_DPI 96
global int win32_global_dpi = WIN32_DEFAULT_DPI;
global HMENU win32_global_menu;
global HANDLE win32_global_small_icon16;
global HANDLE win32_global_small_icon24;

global WINDOWPLACEMENT win32_global_previous_window_placement =
{
   sizeof(win32_global_previous_window_placement)
};

global HIMAGELIST win32_global_toolbar_image_list24;
global HIMAGELIST win32_global_toolbar_image_list48;
global TBBUTTON win32_global_toolbar_buttons[] =
{
   {0, WIN32_MENU_FILE_OPEN, TBSTATE_ENABLED, TBSTYLE_BUTTON|TBSTYLE_AUTOSIZE, {0}, 0, (INT_PTR)TEXT("Open")},
   {1, WIN32_MENU_CONTROL_PLAY, 0, TBSTYLE_BUTTON|TBSTYLE_AUTOSIZE, {0}, 0, (INT_PTR)TEXT("Play")},
   {2, WIN32_MENU_CONTROL_PAUSE, TBSTATE_ENABLED, TBSTYLE_BUTTON|TBSTYLE_AUTOSIZE, {0}, 0, (INT_PTR)TEXT("Pause")},
   {3, WIN32_MENU_VIEW_FULLSCREEN, TBSTATE_ENABLED, TBSTYLE_BUTTON|TBSTYLE_AUTOSIZE, {0}, 0, (INT_PTR)TEXT("Fullscreen")},
};

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

   OutputDebugStringA(message);
}

function void *
win32_allocate(SIZE_T size)
{
   VOID *result = VirtualAlloc(0, size, MEM_COMMIT|MEM_RESERVE, PAGE_READWRITE);
   return(result);
}

function void
win32_free(VOID *memory)
{
   if(!VirtualFree(memory, 0, MEM_RELEASE))
   {
      platform_log("Failed to free virtual memory.\n");
   }
}

function
PLATFORM_FREE_FILE(platform_free_file)
{
   if(file->memory)
   {
      win32_free(file->memory);
   }

   ZeroMemory(file, sizeof(*file));
}

function
PLATFORM_LOAD_FILE(platform_load_file)
{
   struct platform_file result = {0};

   WIN32_FIND_DATAA file_data;
   HANDLE find_file = FindFirstFileA(file_path, &file_data);
   if(find_file == INVALID_HANDLE_VALUE)
   {
      platform_log("ERROR: Failed to find file \"%s\".\n", file_path);
      return(result);
   }
   FindClose(find_file);

   size_t size = (file_data.nFileSizeHigh * (MAXDWORD + 1)) + file_data.nFileSizeLow;
   result.memory = win32_allocate(size);
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

function void
win32_load_cartridge(struct memory_arena *arena, HWND window, char *file_path)
{
   load_cartridge(arena, file_path);

   if(map.load_complete)
   {
      HWND status_bar = GetDlgItem(window, WIN32_STATUS_BAR);
      SendMessageA(status_bar, WM_SETTEXT, 0, (LPARAM)file_path);

      EnableMenuItem(win32_global_menu, WIN32_MENU_FILE_CLOSE, MF_ENABLED);
   }
}

function void
win32_unload_cartridge(struct memory_arena *arena, HWND window)
{
   unload_cartridge(arena);

   HWND status_bar = GetDlgItem(window, WIN32_STATUS_BAR);
   SendMessage(status_bar, WM_SETTEXT, 0, (LPARAM)"");

   EnableMenuItem(win32_global_menu, WIN32_MENU_FILE_CLOSE, MF_GRAYED);
}

function void
win32_open_file_dialog(struct memory_arena *arena, HWND window)
{
   // TODO(law): We want something better than MAX_PATH here.
   char file_name[MAX_PATH] = "";

   OPENFILENAMEA open_file_name = {0};
   open_file_name.lStructSize = sizeof(open_file_name);
   open_file_name.hwndOwner = window;
   open_file_name.lpstrFilter = "(*.gb)\0*.gb\0All Files (*.*)\0*.*\0";
   open_file_name.lpstrFile = file_name;
   open_file_name.nMaxFile = MAX_PATH;
   open_file_name.Flags = OFN_EXPLORER|OFN_PATHMUSTEXIST;
   open_file_name.lpstrDefExt = "gb";

   if(GetOpenFileNameA(&open_file_name))
   {
      win32_load_cartridge(arena, window, file_name);
   }
}

function void
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
         s32 x = monitor_info.rcMonitor.left;
         s32 y = monitor_info.rcMonitor.top;
         s32 width = monitor_info.rcMonitor.right - monitor_info.rcMonitor.left;
         s32 height = monitor_info.rcMonitor.bottom - monitor_info.rcMonitor.top;

         SetWindowLong(window, GWL_STYLE, style & ~WS_OVERLAPPEDWINDOW);
         SetWindowPos(window, HWND_TOP, x, y, width, height, SWP_NOOWNERZORDER|SWP_FRAMECHANGED);
      }

      // NOTE(law): Hide Windows UI in fullscreen mode.
      SetMenu(window, 0);
      ShowWindow(GetDlgItem(window, WIN32_STATUS_BAR), SW_HIDE);
      ShowWindow(GetDlgItem(window, WIN32_REBAR), SW_HIDE);
   }
   else
   {
      SetWindowLong(window, GWL_STYLE, style|WS_OVERLAPPEDWINDOW);
      SetWindowPlacement(window, &win32_global_previous_window_placement);
      SetWindowPos(window, 0, 0, 0, 0, 0, SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER|SWP_NOOWNERZORDER|SWP_FRAMECHANGED);

      // NOTE(law): Show Windows UI after exiting fullscreen mode.
      SetMenu(window, win32_global_menu);
      ShowWindow(GetDlgItem(window, WIN32_STATUS_BAR), SW_SHOW);
      ShowWindow(GetDlgItem(window, WIN32_REBAR), SW_SHOW);
   }
}

function bool
win32_is_fullscreen(HWND window)
{
   DWORD style = GetWindowLong(window, GWL_STYLE);

   bool result = !(style & WS_OVERLAPPEDWINDOW);
   return(result);
}

function u32
win32_get_toolbar_height(HWND window)
{
   // TODO(law): Determine if this query is too slow to process every frame.

   u32 result = 0;

   if(!win32_is_fullscreen(window))
   {
      // NOTE(law): The rebar window contains the toolbar, and should be used to
      // calculate its height.

      RECT window_rect;
      GetWindowRect(GetDlgItem(window, WIN32_REBAR), &window_rect);

      result += window_rect.bottom - window_rect.top;
   }

   return(result);
}

function u32
win32_get_status_height(HWND window)
{
   // TODO(law): Determine if this query is too slow to process every frame.

   u32 result = 0;

   if(!win32_is_fullscreen(window))
   {
      RECT window_rect;
      GetWindowRect(GetDlgItem(window, WIN32_STATUS_BAR), &window_rect);

      result += window_rect.bottom - window_rect.top;
   }

   return(result);
}

function void
win32_adjust_window_rect(RECT *window_rect)
{
   bool dpi_supported = false;
   DWORD window_style = WS_OVERLAPPEDWINDOW;

   // NOTE(law): Try to use the Windows 10 API for a DPI-aware window adjustment.
   HMODULE library = LoadLibrary(TEXT("user32.dll"));
   if(library)
   {
      // TODO(law) Cache the function pointer so the library doesn't need to be
      // reloaded on every resolution update.

      typedef BOOL AWREFD(LPRECT, DWORD, BOOL, DWORD, UINT);
      AWREFD *AdjustWindowRectExForDpi = (AWREFD *)GetProcAddress(library, "AdjustWindowRectExForDpi");
      if(AdjustWindowRectExForDpi)
      {
         AdjustWindowRectExForDpi(window_rect, window_style, true, 0, win32_global_dpi);
         dpi_supported = true;
      }

      FreeLibrary(library);
   }

   if(!dpi_supported)
   {
      AdjustWindowRect(window_rect, window_style, true);
   }
}

function void
win32_set_resolution_scale(HWND window, u32 scale)
{
   // NOTE(law): Prevent updating the resolution if the window is currently in
   // fullscreen mode.
   if(!win32_is_fullscreen(window))
   {
      RECT window_rect = {0};
      window_rect.bottom = RESOLUTION_BASE_HEIGHT << scale;
      window_rect.right  = RESOLUTION_BASE_WIDTH  << scale;
      win32_adjust_window_rect(&window_rect);

      u32 window_width  = window_rect.right - window_rect.left;
      u32 window_height = window_rect.bottom - window_rect.top;

      u32 toolbar_height = win32_get_toolbar_height(window);
      window_height += toolbar_height;

      u32 status_height = win32_get_status_height(window);
      window_height += status_height;

      SetWindowPos(window, 0, 0, 0, window_width, window_height, SWP_NOMOVE);
   }
}

function void
win32_display_bitmap(struct pixel_bitmap bitmap, HWND window, HDC device_context)
{
   RECT client_rect;
   GetClientRect(window, &client_rect);

   s32 client_width = client_rect.right - client_rect.left;
   s32 client_height = client_rect.bottom - client_rect.top;

   u32 toolbar_height = win32_get_toolbar_height(window);
   client_height -= toolbar_height;

   u32 status_height = win32_get_status_height(window);
   client_height -= status_height;

   float client_aspect_ratio = (float)client_width / (float)client_height;
   float target_aspect_ratio = (float)RESOLUTION_BASE_WIDTH / (float)RESOLUTION_BASE_HEIGHT;

   float target_width  = (float)client_width;
   float target_height = (float)client_height;
   float gutter_width  = 0;
   float gutter_height = 0;

   if(client_aspect_ratio > target_aspect_ratio)
   {
      // NOTE(law): The window is too wide, fill in the left and right sides
      // with black gutters.
      target_width = target_aspect_ratio * client_height;
      gutter_width = (client_width - target_width) / 2;
   }
   else if(client_aspect_ratio < target_aspect_ratio)
   {
      // NOTE(law): The window is too tall, fill in the top and bottom with
      // black gutters.
      target_height = (1.0f / target_aspect_ratio) * client_width;
      gutter_height = (client_height - target_height) / 2;
   }

   if(client_aspect_ratio > target_aspect_ratio)
   {
      // NOTE(law): The window is too wide, fill in the left and right sides
      // with black gutters.
      PatBlt(device_context, 0, toolbar_height, (int)gutter_width, (int)target_height, BLACKNESS);
      PatBlt(device_context, (int)(client_width - gutter_width), toolbar_height, (int)gutter_width, (int)target_height, BLACKNESS);
   }
   else if(client_aspect_ratio < target_aspect_ratio)
   {
      // NOTE(law): The window is too tall, fill in the top and bottom with
      // black gutters.
      PatBlt(device_context, 0, toolbar_height, (int)target_width, (int)gutter_height, BLACKNESS);
      PatBlt(device_context, 0, toolbar_height + (int)(client_height - gutter_height), (int)target_width, (int)gutter_height, BLACKNESS);
   }

   int target_x = (int)gutter_width;
   int target_y = (int)(gutter_height + toolbar_height);

   StretchDIBits(device_context,
                 target_x, target_y, (int)target_width, (int)target_height, // Destination
                 0, 0, bitmap.width, bitmap.height, // Source
                 bitmap.memory, win32_global_bitmap_info, DIB_RGB_COLORS, SRCCOPY);
}

struct win32_sound_output
{
   DWORD index;
   DWORD buffer_size;
   LPDIRECTSOUNDBUFFER buffer;
};

function void
win32_clear_sound_buffer(struct win32_sound_output *output)
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

   u8 *destination = (u8 *)region1;
   for(DWORD index = 0; index < size1; ++index)
   {
      *destination++ = 0;
   }

   destination = (u8 *)region2;
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

function void
win32_initialize_sound(struct win32_sound_output *output, HWND window)
{
   ZeroMemory(output, sizeof(*output));

   HMODULE library = LoadLibrary(TEXT("dsound.dll"));
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
   wave_format.wBitsPerSample = (SOUND_OUTPUT_BYTES_PER_SAMPLE / SOUND_OUTPUT_CHANNEL_COUNT) * 8;
   wave_format.nBlockAlign = SOUND_OUTPUT_BYTES_PER_SAMPLE;
   wave_format.nAvgBytesPerSec = wave_format.nSamplesPerSec * wave_format.nBlockAlign;

   if(IDirectSoundBuffer_SetFormat(primary_buffer, &wave_format) != DS_OK)
   {
      platform_log("ERROR: DirectSound failed to set the primary sound buffer's format.\n");
      return;
   }

   DSBUFFERDESC secondary_description = {0};
   secondary_description.dwSize = sizeof(secondary_description);
   secondary_description.dwFlags = DSBCAPS_GLOBALFOCUS;
   secondary_description.dwBufferBytes = SOUND_OUTPUT_HZ * SOUND_OUTPUT_BYTES_PER_SAMPLE;
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

function DWORD
win32_get_sound_write_size(struct win32_sound_output *output)
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

function void
win32_output_sound_samples(struct win32_sound_output *output, s16 *samples, DWORD write_size)
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

   s16 *source = samples;
   s16 *destination = (s16 *)region1;

   DWORD sample_count = size1 / SOUND_OUTPUT_BYTES_PER_SAMPLE;
   for(DWORD sample_index = 0; sample_index < sample_count; ++sample_index)
   {
      *destination++ = *source++; // NOTE(law): Channel 0
      *destination++ = *source++; // NOTE(law): Channel 1

      output->index += SOUND_OUTPUT_BYTES_PER_SAMPLE;
   }

   destination = (s16 *)region2;

   sample_count = size2 / SOUND_OUTPUT_BYTES_PER_SAMPLE;
   for(DWORD sample_index = 0; sample_index < sample_count; ++sample_index)
   {
      *destination++ = *source++; // NOTE(law): Channel 0
      *destination++ = *source++; // NOTE(law): Channel 1

      output->index += SOUND_OUTPUT_BYTES_PER_SAMPLE;
   }

   if(IDirectSoundBuffer_Unlock(output->buffer, region1, size1, region2, size2) != DS_OK)
   {
      platform_log("ERROR: DirectSound failed to unlock the sound buffer.\n");
      return;
   }
}

function void
win32_set_pause_state(HWND window, bool paused)
{
   win32_global_is_paused = paused;

   HWND toolbar = GetDlgItem(GetDlgItem(window, WIN32_REBAR), WIN32_TOOLBAR);
   if(toolbar)
   {
      WPARAM play_button = (WPARAM)WIN32_MENU_CONTROL_PLAY;
      WPARAM pause_button = (WPARAM)WIN32_MENU_CONTROL_PAUSE;

      LPARAM enable = (LPARAM)MAKELONG(1, 0);
      LPARAM disable = (LPARAM)MAKELONG(0, 0);

      SendMessage(toolbar, TB_ENABLEBUTTON, pause_button, (paused) ? disable : enable);
      SendMessage(toolbar, TB_ENABLEBUTTON, play_button, (paused) ? enable : disable);
   }
}

function int
win32_get_window_dpi(HWND window)
{
   int result = 0;

   // NOTE(law): Try to use the Windows 8.1 API to get the monitor's DPI.
   HMODULE library = LoadLibrary(TEXT("shcore.lib"));
   if(library)
   {
      typedef HRESULT GDFM(HMONITOR, int, UINT *, UINT *);
      GDFM *GetDpiForMonitor = (GDFM *)GetProcAddress(library, "GetDpiForMonitor");

      if(GetDpiForMonitor)
      {
         HMONITOR monitor = MonitorFromWindow(window, MONITOR_DEFAULTTOPRIMARY);

         UINT dpi_x, dpi_y;
         if(SUCCEEDED(GetDpiForMonitor(monitor, 0, &dpi_x, &dpi_y)))
         {
            result = dpi_x;
         }
      }

      FreeLibrary(library);
   }

   if(!result)
   {
      // NOTE(law): If we don't have access to the Windows 8.1 API, just grab the
      // DPI off the primary monitor.
      HDC device_context = GetDC(0);
      result = GetDeviceCaps(device_context, LOGPIXELSX);
      ReleaseDC(0, device_context);
   }

   assert(result);

   return(result);
}

function void
win32_select_color_scheme(HWND window, enum monochrome_color_scheme scheme)
{
   if(scheme >= MONOCHROME_COLOR_SCHEME_COUNT)
   {
      scheme = 0;
   }

   gem_global_color_scheme = scheme;
   CheckMenuRadioItem(win32_global_menu,
                      WIN32_MENU_VIEW_COLOR_DMG,
                      WIN32_MENU_VIEW_COLOR_LIGHT,
                      WIN32_MENU_VIEW_COLOR_DMG + scheme,
                      MF_BYCOMMAND);
}

function void
win32_create_toolbar(HWND window)
{
   // TODO(law): Determine if there is a better way to repopulate toolbar
   // buttons without completely recreating the toolbar.

   HWND toolbar = 0;
   HWND rebar = GetDlgItem(window, WIN32_REBAR);
   if(rebar)
   {
      toolbar = GetDlgItem(rebar, WIN32_TOOLBAR);
      if(toolbar)
      {
         DestroyWindow(toolbar);
      }

      DestroyWindow(rebar);
   }

   toolbar = CreateWindow(TOOLBARCLASSNAME, 0, WS_CHILD|WS_VISIBLE|CCS_NODIVIDER|CCS_NOPARENTALIGN|TBSTYLE_FLAT|TBSTYLE_TOOLTIPS,
                          0, 0, 0, 0, window, (HMENU)WIN32_TOOLBAR, GetModuleHandle(0), 0);

   HIMAGELIST image_list = (win32_global_dpi > WIN32_DEFAULT_DPI) ? win32_global_toolbar_image_list48 : win32_global_toolbar_image_list24;

   SendMessage(toolbar, TB_BUTTONSTRUCTSIZE, (WPARAM)sizeof(TBBUTTON), 0);
   SendMessage(toolbar, TB_SETIMAGELIST, 0, (LPARAM)image_list);
   SendMessage(toolbar, TB_ADDBUTTONS, ARRAY_LENGTH(win32_global_toolbar_buttons), (LPARAM)&win32_global_toolbar_buttons);

   int padding = MulDiv(8, win32_global_dpi, WIN32_DEFAULT_DPI);
   SendMessage(toolbar, TB_SETPADDING, 0, MAKELONG(padding, 0));
   SendMessage(toolbar, TB_SETMAXTEXTROWS, 1, 0); // NOTE(law): Turn on labels.
   // SendMessage(toolbar, TB_SETMAXTEXTROWS, 0, 0); // NOTE(law): Turn off labels, only use text as tooltips.

   // NOTE(law): Create rebar container for toolbar.
   rebar = CreateWindow(REBARCLASSNAME, 0, WS_CHILD|WS_VISIBLE|CCS_NODIVIDER|RBS_BANDBORDERS,
                        0, 0, 0, 0, window, (HMENU)WIN32_REBAR, GetModuleHandle(0), 0);

   DWORD button_size = (DWORD)SendMessage(toolbar, TB_GETBUTTONSIZE, 0, 0);
   DWORD button_width = LOWORD(button_size);
   DWORD button_height = HIWORD(button_size);

   DWORD padding_size = (DWORD)SendMessage(toolbar, TB_GETPADDING, 0, 0);
   DWORD padding_width = LOWORD(padding_size);
   DWORD padding_height = HIWORD(padding_size);

   REBARBANDINFO band = {sizeof(band)};
   band.fMask = RBBIM_STYLE|RBBIM_CHILD|RBBIM_CHILDSIZE|RBBIM_SIZE;
   band.fStyle = RBBS_CHILDEDGE;
   band.hwndChild = toolbar;
   band.cxMinChild = ARRAY_LENGTH(win32_global_toolbar_buttons) * (button_width + padding_width);
   band.cyMinChild = button_height + (padding_height * 2);
   SendMessage(rebar, RB_INSERTBAND, (WPARAM)-1, (LPARAM)&band);

   win32_select_color_scheme(window, gem_global_color_scheme);
}

LRESULT
win32_window_callback(HWND window, UINT message, WPARAM wparam, LPARAM lparam)
{
   LRESULT result = 0;

   switch(message)
   {
      case WM_CREATE:
      {
         win32_global_dpi = win32_get_window_dpi(window);
         win32_global_menu = GetMenu(window);

         win32_global_small_icon16 = LoadImage(GetModuleHandle(0), MAKEINTRESOURCE(WIN32_ICON), IMAGE_ICON, 16, 16, 0);
         win32_global_small_icon24 = LoadImage(GetModuleHandle(0), MAKEINTRESOURCE(WIN32_ICON), IMAGE_ICON, 24, 24, 0);

         if(win32_global_dpi > WIN32_DEFAULT_DPI)
         {
            SendMessage(window, WM_SETICON, ICON_SMALL, (LPARAM)win32_global_small_icon24);
         }
         else
         {
            SendMessage(window, WM_SETICON, ICON_SMALL, (LPARAM)win32_global_small_icon16);
         }

         win32_global_toolbar_image_list24 = ImageList_Create(24, 24, ILC_MASK|ILC_COLORDDB, ARRAY_LENGTH(win32_global_toolbar_buttons), 1);
         win32_global_toolbar_image_list48 = ImageList_Create(48, 48, ILC_MASK|ILC_COLORDDB, ARRAY_LENGTH(win32_global_toolbar_buttons), 1);

         HBITMAP toolbar_bitmap24 = LoadBitmap(GetModuleHandle(0), MAKEINTRESOURCE(WIN32_TOOLBAR_BUTTONS_BITMAP_24));
         HBITMAP toolbar_bitmap48 = LoadBitmap(GetModuleHandle(0), MAKEINTRESOURCE(WIN32_TOOLBAR_BUTTONS_BITMAP_48));

         ImageList_AddMasked(win32_global_toolbar_image_list24, toolbar_bitmap24, RGB(0, 0, 0));
         ImageList_AddMasked(win32_global_toolbar_image_list48, toolbar_bitmap48, RGB(0, 0, 0));

         // NOTE(law): Create toolbar.
         win32_create_toolbar(window);

         // NOTE(law): Create window status bar.
         CreateWindow(STATUSCLASSNAME, 0, WS_CHILD|WS_VISIBLE, 0, 0, 0, 0, window, (HMENU)WIN32_STATUS_BAR, GetModuleHandle(0), 0);

         // NOTE(law): Set the initial window size based on the scale of the
         // client bitmap area once the various pieces of Win32 UI have been
         // created.
         win32_set_resolution_scale(window, (win32_global_dpi > WIN32_DEFAULT_DPI) ? 2 : 1);
      } break;

      case WM_SIZE:
      {
         HWND rebar = GetDlgItem(window, WIN32_REBAR);
         SendMessage(rebar, WM_SIZE, 0, 0);

         HWND toolbar = GetDlgItem(rebar, WIN32_TOOLBAR);
         SendMessage(toolbar, TB_AUTOSIZE, 0, 0);

         HWND status_bar = GetDlgItem(window, WIN32_STATUS_BAR);
         SendMessage(status_bar, WM_SIZE, 0, 0);
      } break;

      case WM_DPICHANGED:
      {
         win32_global_dpi = HIWORD(wparam);
         win32_create_toolbar(window);

         RECT *updated_window = (RECT *)lparam;
         int x = updated_window->left;
         int y = updated_window->top;
         int width = updated_window->right - updated_window->left;
         int height = updated_window->bottom - updated_window->top;

         SetWindowPos(window, 0, x, y, width, height, SWP_NOZORDER|SWP_NOACTIVATE);

         if(win32_global_dpi > WIN32_DEFAULT_DPI)
         {
            SendMessage(window, WM_SETICON, ICON_SMALL, (LPARAM)win32_global_small_icon24);
         }
         else
         {
            SendMessage(window, WM_SETICON, ICON_SMALL, (LPARAM)win32_global_small_icon16);
         }
      } break;

      case WM_COMMAND:
      {
         switch(LOWORD(wparam))
         {
            case WIN32_MENU_FILE_OPEN:
            {
               win32_open_file_dialog(win32_global_arena, window);
            } break;

            case WIN32_MENU_FILE_CLOSE:
            {
               win32_unload_cartridge(win32_global_arena, window);
            } break;

            case WIN32_MENU_FILE_EXIT:
            {
               win32_global_is_running = false;
               PostQuitMessage(0);
            } break;

            case WIN32_MENU_VIEW_COLOR_DMG:
            {
               win32_select_color_scheme(window, MONOCHROME_COLOR_SCHEME_DMG);
            } break;

            case WIN32_MENU_VIEW_COLOR_MGB:
            {
               win32_select_color_scheme(window, MONOCHROME_COLOR_SCHEME_MGB);
            } break;

            case WIN32_MENU_VIEW_COLOR_LIGHT:
            {
               win32_select_color_scheme(window, MONOCHROME_COLOR_SCHEME_LIGHT);
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

            case WIN32_MENU_CONTROL_PLAY:
            {
               win32_set_pause_state(window, false);
            } break;

            case WIN32_MENU_CONTROL_PAUSE:
            {
               win32_set_pause_state(window, true);
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
            else if(wparam == VK_ESCAPE && win32_is_fullscreen(window))
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
               win32_set_pause_state(window, !win32_global_is_paused);
            }
            else if(wparam == 'C')
            {
               win32_select_color_scheme(window, gem_global_color_scheme + 1);
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
                  disassemble_instruction(registers.pc);

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
         win32_display_bitmap(*win32_global_bitmap, window, device_context);
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
   (void)previous_instance;

   InitCommonControls();
   QueryPerformanceFrequency(&win32_global_counts_per_second);
   bool sleep_is_granular = (timeBeginPeriod(1) == TIMERR_NOERROR);

   WNDCLASSEX window_class = {0};
   window_class.cbSize = sizeof(window_class);
   window_class.style = CS_HREDRAW|CS_VREDRAW;
   window_class.lpfnWndProc = win32_window_callback;
   window_class.hInstance = instance;
   window_class.hIcon = LoadImage(instance, MAKEINTRESOURCE(WIN32_ICON), IMAGE_ICON, 0, 0, LR_DEFAULTSIZE);
   window_class.hCursor = LoadCursor(0, IDC_ARROW);
   window_class.lpszClassName = TEXT("Game_Boy_Emulator_GEM");
   window_class.lpszMenuName = MAKEINTRESOURCE(WIN32_MENU);

   if(!RegisterClassEx(&window_class))
   {
      platform_log("ERROR: Windows failed to register a window class.\n");
      return(1);
   }

   HWND window = CreateWindowEx(0,
                                window_class.lpszClassName,
                                TEXT("Game Boy Emulator (GEM)"),
                                WS_OVERLAPPEDWINDOW|WS_THICKFRAME,
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
   struct memory_arena arena = {0};
   arena.size = MEBIBYTES(64);
   arena.base_address = win32_allocate(arena.size);
   if(!arena.base_address)
   {
      platform_log("ERROR: Windows failed to allocate our memory arena.\n");
      return(1);
   }

   win32_global_arena = &arena;

   // NOTE(law) Set up the rendering bitmap.
   struct pixel_bitmap bitmap = {RESOLUTION_BASE_WIDTH, RESOLUTION_BASE_HEIGHT};

   SIZE_T bytes_per_pixel = sizeof(u32);
   SIZE_T bitmap_size = bitmap.width * bitmap.height * bytes_per_pixel;
   bitmap.memory = win32_allocate(bitmap_size);
   if(!bitmap.memory)
   {
      platform_log("ERROR: Windows failed to allocate our bitmap.\n");
      return(1);
   }

   clear(&bitmap, get_display_off_color(gem_global_color_scheme));

   BITMAPINFOHEADER bitmap_header = {0};
   bitmap_header.biSize = sizeof(BITMAPINFOHEADER);
   bitmap_header.biWidth = bitmap.width;
   bitmap_header.biHeight = -(s32)bitmap.height; // NOTE(law): Negative will indicate a top-down bitmap.
   bitmap_header.biPlanes = 1;
   bitmap_header.biBitCount = 32;
   bitmap_header.biCompression = BI_RGB;

   BITMAPINFO bitmap_info = {bitmap_header};

   win32_global_bitmap = &bitmap;
   win32_global_bitmap_info = &bitmap_info;

   // NOTE(law): Display the created window.
   ShowWindow(window, show_command);
   UpdateWindow(window);

   // NOTE(law): Initialize sound.
   struct win32_sound_output sound_output;
   win32_initialize_sound(&sound_output, window);

   struct sound_samples sound = {0};
   sound.size = sound_output.buffer_size;
   sound.samples = win32_allocate(sound.size);
   if(!sound.samples)
   {
      platform_log("ERROR: Windows failed to allocate our sound samples.\n");
      return(1);
   }

   // NOTE(law): Attempt to load a ROM in case a path to one was provided as the
   // command line argument.
   if(command_line[0])
   {
      win32_load_cartridge(&arena, window, command_line);
   }

   float frame_seconds_elapsed = 0;
   float target_seconds_per_frame = 1.0f / VERTICAL_SYNC_HZ;
   u32 target_cycles_per_frame = (u32)(CPU_HZ / VERTICAL_SYNC_HZ);

   struct cycle_clocks clocks = {0};

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

      if(!map.load_complete)
      {
         clear(&bitmap, get_display_off_color(gem_global_color_scheme));
      }

      if(!map.load_complete || win32_global_is_paused)
      {
         win32_clear_sound_buffer(&sound_output);
      }

      if(!win32_global_is_paused && map.load_complete)
      {
         clocks.cpu %= target_cycles_per_frame;
         while(clocks.cpu < target_cycles_per_frame)
         {
            cpu_tick(&clocks, &bitmap, &sound);
         }

         // NOTE(law): Output sound.
         DWORD sound_write_size = win32_get_sound_write_size(&sound_output);
         if(sound_write_size > (sound.sample_index * SOUND_OUTPUT_BYTES_PER_SAMPLE))
         {
            u32 remaining_samples = (sound_write_size / SOUND_OUTPUT_BYTES_PER_SAMPLE) - sound.sample_index;
#if DEBUG_SINE_WAVE
            generate_debug_samples(&sound, remaining_samples);
#else
            // TODO(law): This method probably won't work once longer sounds are
            // played - the counters will become out of sync with the CPU clock.
            while(remaining_samples--)
            {
               generate_sound_sample(&sound);
            }
#endif
            assert(sound_write_size == sound.sample_index * SOUND_OUTPUT_BYTES_PER_SAMPLE);
         }

         win32_output_sound_samples(&sound_output, sound.samples, sound.sample_index * SOUND_OUTPUT_BYTES_PER_SAMPLE);
         clear_sound_samples(&sound);
      }

      // NOTE(law): Blit bitmap to screen.
      HDC device_context = GetDC(window);
      win32_display_bitmap(bitmap, window, device_context);
      ReleaseDC(window, device_context);

      // NOTE(law): Calculate elapsed frame time.
      LARGE_INTEGER frame_end_count;
      QueryPerformanceCounter(&frame_end_count);
      frame_seconds_elapsed = WIN32_SECONDS_ELAPSED(frame_start_count, frame_end_count);

      // NOTE(law): If possible, sleep for some of the remaining frame time. The
      // sleep time calculation intentionally undershoots to prevent
      // oversleeping due to the lack of sub-millisecond granualarity.
      DWORD sleep_ms = 0;
      float sleep_fraction = 0.9f;
      if(sleep_is_granular && (frame_seconds_elapsed < target_seconds_per_frame))
      {
         sleep_ms = (DWORD)((target_seconds_per_frame - frame_seconds_elapsed) * 1000.0f * sleep_fraction);
         if(sleep_ms > 0)
         {
            Sleep(sleep_ms);
         }
      }

      // NOTE(law): Spin lock for the remaining frame time.
      while(frame_seconds_elapsed < target_seconds_per_frame)
      {
         QueryPerformanceCounter(&frame_end_count);
         frame_seconds_elapsed = WIN32_SECONDS_ELAPSED(frame_start_count, frame_end_count);
      }
      frame_start_count = frame_end_count;

      platform_log("Frame time: %0.03fms, ", frame_seconds_elapsed * 1000.0f);
      platform_log("Sleep: %ums, ", sleep_ms);
      platform_log("Cycles: %u, ", clocks.cpu);
      platform_log("Cycle time: %0.05fus\n", (frame_seconds_elapsed * 1000.0f * 1000.0f) / clocks.cpu);
   }

   return(0);
}
