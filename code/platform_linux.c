/* /////////////////////////////////////////////////////////////////////////// */
/* (c) copyright 2022 Lawrence D. Kern /////////////////////////////////////// */
/* /////////////////////////////////////////////////////////////////////////// */

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdarg.h>

#include "gem.c"

typedef struct
{
   size_t size;
   unsigned char *memory;
} Linux_File;

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

static void
linux_free_file(Linux_File *file)
{
   free(file->memory);
   file->size = 0;
   file->memory = 0;
}

static Linux_File
linux_load_file(char *file_path)
{
   // TODO(law): Better file I/O once file access is needed anywhere besides
   // program startup.

   Linux_File result = {0};

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

   Linux_File rom = linux_load_file(rom_path);
   if(rom.size == 0)
   {
      return(1);
   }

   printf("Validating cartridge header...\n");
   if(!validate_cartridge_header(rom.memory, rom.size))
   {
      return(1);
   }

   if(output_header)
   {
      dump_cartridge_header(rom.memory);
   }

   Cartridge_Header *header = get_cartridge_header(rom.memory);
   if(output_disassembly)
   {
      printf("Parsing entry_point...\n");
      unsigned int offset = (unsigned char *)&header->entry_point - rom.memory;
      disassemble_stream(rom.memory, offset, sizeof(header->entry_point));

      printf("Parsing instruction stream...\n");
      disassemble_stream(rom.memory, 0x150, rom.size - 0x150);
   }

   return(0);
}
