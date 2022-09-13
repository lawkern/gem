/* /////////////////////////////////////////////////////////////////////////// */
/* (c) copyright 2022 Lawrence D. Kern /////////////////////////////////////// */
/* /////////////////////////////////////////////////////////////////////////// */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define ARRAY_LENGTH(array) (sizeof(array) / sizeof((array)[0]))

#pragma pack(push, 1)
typedef struct
{
   // NOTE(law): The format of the cartidge header is referenced from:
   // https://gbdev.io/pandocs/The_Cartridge_Header.html

   unsigned int entry_point;
   unsigned char logo[48];

   union
   {
      char title[16];
      struct
      {
         char short_title[11];
         char manufacturer_code[4];
         unsigned char cgb_flag;
      };
   };

   char new_licensee_code[2];
   unsigned char sgb_flag;
   unsigned char cartridge_type;
   unsigned char rom_size;
   unsigned char ram_size;
   unsigned char destination_code;
   unsigned char old_licensee_code;
   unsigned char mask_rom_version_number;
   unsigned char header_checksum;
   unsigned short global_checksum;
} Cartridge_Header;
#pragma pack(pop)

static unsigned char global_logo[] =
{
   0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
   0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
   0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
};

static char *global_sram_sizes[] =
{
   "0 (no RAM)",
   "- (Unused)",
   "8 KiB (1 bank)",
   "32 KiB (4 banks of 8 KiB each)",
   "128 KiB (16 banks of 8 KiB each)",
   "64 KiB (8 banks of 8 KiB each)",
};

static char *global_destination_codes[] =
{
   "Japan (and possibly overseas)",
   "Overseas only",
};

static char *global_cartridge_types[] =
{
   [0x00] = "ROM ONLY",
   [0x01] = "MBC1",
   [0x02] = "MBC1+RAM",
   [0x03] = "MBC1+RAM+BATTERY",
   [0x05] = "MBC2",
   [0x06] = "MBC2+BATTERY",
   [0x08] = "ROM+RAM 1",
   [0x09] = "ROM+RAM+BATTERY 1",
   [0x0B] = "MMM01",
   [0x0C] = "MMM01+RAM",
   [0x0D] = "MMM01+RAM+BATTERY",
   [0x0F] = "MBC3+TIMER+BATTERY",
   [0x10] = "MBC3+TIMER+RAM+BATTERY 2",
   [0x11] = "MBC3",
   [0x12] = "MBC3+RAM 2",
   [0x13] = "MBC3+RAM+BATTERY 2",
   [0x19] = "MBC5",
   [0x1A] = "MBC5+RAM",
   [0x1B] = "MBC5+RAM+BATTERY",
   [0x1C] = "MBC5+RUMBLE",
   [0x1D] = "MBC5+RUMBLE+RAM",
   [0x1E] = "MBC5+RUMBLE+RAM+BATTERY",
   [0x20] = "MBC6",
   [0x22] = "MBC7+SENSOR+RUMBLE+RAM+BATTERY",
   [0xFC] = "POCKET CAMERA",
   [0xFD] = "BANDAI TAMA5",
   [0xFE] = "HuC3",
   [0xFF] = "HuC1+RAM+BATTERY",
};

typedef struct
{
   size_t size;
   unsigned char *memory;
} Platform_File;

static Platform_File
load_file(char *path)
{
   Platform_File result = {0};

   FILE *file = fopen(path, "rb");
   if(file)
   {
      fseek(file, 0, SEEK_END);
      size_t size = ftell(file);
      fseek(file, 0, SEEK_SET);

      if(size > 0)
      {
         result.memory = malloc(size);
         if(result.memory)
         {
            size_t bytes_read = fread(result.memory, 1, size, file);
            if(bytes_read == size)
            {
               result.size = size;
            }
            else
            {
               free(result.memory);
            }
         }
      }

      fclose(file);
   }

   return(result);
}

// TODO(law): For now, we're just assuming the host machine is little endian. In
// the future, it might be worthwhile to test endianess first and allow the
// values to pass through unmodified if this ever runs on a big endian machine
// (i.e. the same byte order of the Game Boy).

static unsigned short
endian_swap16(unsigned short value)
{
   unsigned short result = ((value << 8) & 0xFF00) | ((value >> 8) & 0x00FF);
   return(result);
}

static unsigned int
endian_swap32(unsigned int value)
{
   unsigned int result = (((value << 24) & 0xFF000000) |
                          ((value <<  8) & 0x00FF0000) |
                          ((value >>  8) & 0x0000FF00) |
                          ((value >> 24) & 0x000000FF));
   return(result);
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
      fprintf(stderr, "ERROR: Failed to load ROM at \"%s\"\n", rom_path);
      return(1);
   }

   printf("Parsing cartridge header...\n");
   Cartridge_Header *header = (Cartridge_Header *)(rom.memory + 0x100);

   // NOTE(law): Confirm that the cartridge stored the Game Boy logo correctly.
   assert(ARRAY_LENGTH(global_logo) == ARRAY_LENGTH(header->logo));
   for(unsigned int byte_index = 0; byte_index < ARRAY_LENGTH(global_logo); ++byte_index)
   {
      if(global_logo[byte_index] != header->logo[byte_index])
      {
         fprintf(stderr, "ERROR: Logo mismatch at byte index %d ", byte_index);
         fprintf(stderr, "(header: %#x, expected: %#x)\n", header->logo[byte_index], global_logo[byte_index]);

         return(1);
      }
   }

   printf("Verifying checksums...\n");

   unsigned char header_checksum = 0;
   for(unsigned short byte_offset = 0x0134; byte_offset <= 0x014C; ++byte_offset)
   {
      header_checksum = header_checksum - rom.memory[byte_offset] - 1;
   }

   if(header_checksum != header->header_checksum)
   {
      fprintf(stderr, "ERROR: Computed header checksum did not match value in header. ");
      fprintf(stderr, "(header: %#x, computed: %#x.\n", header_checksum, header->header_checksum);

      return(1);
   }

   unsigned short global_checksum = 0;
   for(unsigned int byte_offset = 0; byte_offset < rom.size; ++byte_offset)
   {
      global_checksum += (unsigned short)rom.memory[byte_offset];
   }

   global_checksum -= ((header->global_checksum >> 0) & 0x00FF);
   global_checksum -= ((header->global_checksum >> 8) & 0x00FF);

   header->global_checksum = endian_swap16(header->global_checksum);

   if(global_checksum != header->global_checksum)
   {
      fprintf(stderr, "WARNING: Computed global checksum did not match value in header. ");
      fprintf(stderr, "(header: %#06x, computed: %#06x)\n", header->global_checksum, global_checksum);
   }

   printf("CARTRIDGE HEADER:\n");

   header->entry_point = endian_swap32(header->entry_point);
   printf("  ENTRY POINT: %#010x\n", header->entry_point);

   printf("  TITLE: %s\n", header->title);
   printf("  SGB FLAG: %#x\n", header->sgb_flag);

   assert(header->cartridge_type < ARRAY_LENGTH(global_cartridge_types));
   printf("  CARTRIDGE TYPE: %#x (%s)\n", header->cartridge_type, global_cartridge_types[header->cartridge_type]);
   printf("  ROM SIZE: %u KiB\n", 32 * (1 << header->rom_size));

   assert(header->ram_size < ARRAY_LENGTH(global_sram_sizes));
   printf("  RAM SIZE: %s\n", global_sram_sizes[header->ram_size]);

   assert(header->destination_code < ARRAY_LENGTH(global_destination_codes));
   printf("  DESTINATION CODE: %#x (%s)\n", header->destination_code, global_destination_codes[header->destination_code]);

   if(header->old_licensee_code == 0x33)
   {
      printf("  OLD LICENSEE CODE: UNUSED\n");
      printf("  NEW LICENSEE CODE: %.*s\n", 2, header->new_licensee_code);
   }
   else
   {
      printf("  OLD LICENSEE CODE: %#x\n", header->old_licensee_code);
      printf("  NEW LICENSEE CODE: UNUSED\n");
   }

   printf("  MASK ROM VERSION NUMBER: %#x\n", header->mask_rom_version_number);
   printf("  HEADER CHECKSUM: %#04x\n", header->header_checksum);
   printf("  GLOBAL CHECKSUM: %#06x\n", header->global_checksum);

   return(0);
}
