/* /////////////////////////////////////////////////////////////////////////// */
/* (c) copyright 2022 Lawrence D. Kern /////////////////////////////////////// */
/* /////////////////////////////////////////////////////////////////////////// */

#include "gem.c"

static
PLATFORM_LOAD_FILE(load_file)
{
   // TODO(law): Implement with OS-specfic primitives!

   Platform_File result = {0};

   FILE *file = fopen(file_path, "rb");
   if(!file)
   {
      fprintf(stderr, "ERROR: Failed to read file \"%s\"\n", file_path);
      return(result);
   }

   fseek(file, 0, SEEK_END);
   size_t size = ftell(file);
   fseek(file, 0, SEEK_SET);

   if(size <= 0)
   {
      fprintf(stderr, "ERROR: Failed to read file size of \"%s\"\n", file_path);
      fclose(file);

      return(result);
   }

   result.memory = malloc(size);
   if(!result.memory)
   {
      fprintf(stderr, "ERROR: Failed to allocate memory for \"%s\"\n", file_path);
      fclose(file);

      return(result);
   }

   size_t bytes_read = fread(result.memory, 1, size, file);
   if(bytes_read != size)
   {
      fprintf(stderr, "ERROR: Failed to read file \"%s\"\n", file_path);
      fclose(file);

      free(result.memory);
      result.memory = 0;

      return(result);
   }

   result.size = size;
   fclose(file);

   return(result);
}

static
PLATFORM_FREE_FILE(free_file)
{
   // TODO(law): Implement with OS-specfic primitives!

   free(file.memory);
   file.size = 0;
}

int
main(int argument_count, char **arguments)
{
   char *program_name = arguments[0];

   if(argument_count < 2)
   {
      fprintf(stdout, "USAGE: %s <path to ROM file>\n", program_name);
      return(0);
   }

   char *rom_path = arguments[1];
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

   Cartridge_Header *header = get_cartridge_header(rom.memory);
   dump_cartridge_header(header);

   printf("Parsing entry_point...\n");
   unsigned int offset = (unsigned char *)&header->entry_point - rom.memory;
   disassemble_stream(rom.memory, offset, sizeof(header->entry_point));

   printf("Parsing instruction stream...\n");
   disassemble_stream(rom.memory, 0x150, rom.size - 0x150);

   return(0);
}
