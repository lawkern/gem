/* /////////////////////////////////////////////////////////////////////////// */
/* (c) copyright 2022 Lawrence D. Kern /////////////////////////////////////// */
/* /////////////////////////////////////////////////////////////////////////// */

#include <windows.h>

#include "gem.c"

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

int
main(int argument_count, char **arguments)
{
   char *program_name = arguments[0];

   if(argument_count < 2)
   {
      fprintf(stdout, "USAGE: %s [-h|-d] <path to ROM file>\n", program_name);
      return(0);
   }

   bool output_header = false;
   bool output_disassembly = false;

   if(argument_count > 2)
   {
      char *flag = arguments[1];
      if(flag[0] == '-' && flag[1] == 'h')
      {
         output_header = true;
      }
      else if(flag[0] == '-' && flag[1] == 'd')
      {
         output_disassembly = true;
      }
   }

   char *rom_path = arguments[argument_count - 1];
   printf("Loading ROM at \"%s\"...\n", rom_path);

   Platform_File rom = load_file(rom_path);
   if(rom.size == 0)
   {
      return(1);
   }

   printf("Validating cartridge header...\n");
   if(!validate_cartridge_header(rom.memory, rom.size))
   {
      return(1);
   }
   printf("Validated header.\n");

   Cartridge_Header *header = get_cartridge_header(rom.memory);
   if(output_header)
   {
      dump_cartridge_header(header);
   }

   if(output_disassembly)
   {
      printf("Parsing entry_point...\n");
      unsigned int offset = (unsigned char *)&header->entry_point - rom.memory;
      disassemble_stream(rom.memory, offset, sizeof(header->entry_point));

      printf("Parsing instruction stream...\n");
      disassemble_stream(rom.memory, 0x150, rom.size - 0x150);
   }

   // TODO(law): Implement Memory Bank Controllers.
   assert(header->ram_size == 0);

   unsigned char *memory_map = malloc(0xFFFF);
   memcpy(memory_map, rom.memory, rom.size);

   printf("Fetching and executing instructions...\n");
   register_pc = 0x100;
   fetch_and_execute(memory_map); // NOP
   fetch_and_execute(memory_map); // JP

   fetch_and_execute(memory_map);
   fetch_and_execute(memory_map);
   fetch_and_execute(memory_map);
   fetch_and_execute(memory_map);
   fetch_and_execute(memory_map);

   printf("Finished fetching and executing instructions.\n");

   return(0);
}
