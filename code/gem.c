/* /////////////////////////////////////////////////////////////////////////// */
/* (c) copyright 2022 Lawrence D. Kern /////////////////////////////////////// */
/* /////////////////////////////////////////////////////////////////////////// */

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define GEM_BASE_RESOLUTION_WIDTH 160
#define GEM_BASE_RESOLUTION_HEIGHT 144

#define TILE_PIXEL_DIM 8
#define TILES_PER_SCREEN_WIDTH  (GEM_BASE_RESOLUTION_WIDTH  / TILE_PIXEL_DIM)
#define TILES_PER_SCREEN_HEIGHT (GEM_BASE_RESOLUTION_HEIGHT / TILE_PIXEL_DIM)

typedef struct
{
   unsigned int width;
   unsigned int height;
   unsigned int *memory;
} Platform_Bitmap;

#define PLATFORM_LOG(name) void name(char *format, ...)
static PLATFORM_LOG(platform_log);

#define ARRAY_LENGTH(array) (sizeof(array) / sizeof((array)[0]))

unsigned int palette[] = {0xE0F8D0, 0x88C070, 0x346856, 0x081820};

static unsigned char boot_rom[] =
{
   0x31, 0xFE, 0xFF, 0xAF, 0x21, 0xFF, 0x9F, 0x32, 0xCB, 0x7C, 0x20, 0xFB, 0x21, 0x26, 0xFF, 0x0E,
   0x11, 0x3E, 0x80, 0x32, 0xE2, 0x0C, 0x3E, 0xF3, 0xE2, 0x32, 0x3E, 0x77, 0x77, 0x3E, 0xFC, 0xE0,
   0x47, 0x11, 0x04, 0x01, 0x21, 0x10, 0x80, 0x1A, 0xCD, 0x95, 0x00, 0xCD, 0x96, 0x00, 0x13, 0x7B,
   0xFE, 0x34, 0x20, 0xF3, 0x11, 0xD8, 0x00, 0x06, 0x08, 0x1A, 0x13, 0x22, 0x23, 0x05, 0x20, 0xF9,
   0x3E, 0x19, 0xEA, 0x10, 0x99, 0x21, 0x2F, 0x99, 0x0E, 0x0C, 0x3D, 0x28, 0x08, 0x32, 0x0D, 0x20,
   0xF9, 0x2E, 0x0F, 0x18, 0xF3, 0x67, 0x3E, 0x64, 0x57, 0xE0, 0x42, 0x3E, 0x91, 0xE0, 0x40, 0x04,
   0x1E, 0x02, 0x0E, 0x0C, 0xF0, 0x44, 0xFE, 0x90, 0x20, 0xFA, 0x0D, 0x20, 0xF7, 0x1D, 0x20, 0xF2,
   0x0E, 0x13, 0x24, 0x7C, 0x1E, 0x83, 0xFE, 0x62, 0x28, 0x06, 0x1E, 0xC1, 0xFE, 0x64, 0x20, 0x06,
   0x7B, 0xE2, 0x0C, 0x3E, 0x87, 0xE2, 0xF0, 0x42, 0x90, 0xE0, 0x42, 0x15, 0x20, 0xD2, 0x05, 0x20,
   0x4F, 0x16, 0x20, 0x18, 0xCB, 0x4F, 0x06, 0x04, 0xC5, 0xCB, 0x11, 0x17, 0xC1, 0xCB, 0x11, 0x17,
   0x05, 0x20, 0xF5, 0x22, 0x23, 0x22, 0x23, 0xC9, 0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B,
   0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D, 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E,
   0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC,
   0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E, 0x3C, 0x42, 0xB9, 0xA5, 0xB9, 0xA5, 0x42, 0x3C,
   0x21, 0x04, 0x01, 0x11, 0xA8, 0x00, 0x1A, 0x13, 0xBE, 0x20, 0xFE, 0x23, 0x7D, 0xFE, 0x34, 0x20,
   0xF5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20, 0xFB, 0x86, 0x20, 0xFE, 0x3E, 0x01, 0xE0, 0x50,
};

static unsigned short
endian_swap16(unsigned short value)
{
   // TODO(law): For now, we're just assuming the host machine is little
   // endian. In the future, it might be worthwhile to test endianess first and
   // allow the values to pass through unmodified if this ever runs on a big
   // endian machine (i.e. the same byte order of the Game Boy).

   unsigned short result = ((value << 8) & 0xFF00) | ((value >> 8) & 0x00FF);
   return(result);
}

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

static Cartridge_Header *
get_cartridge_header(unsigned char *rom_memory)
{
   Cartridge_Header *result = (Cartridge_Header *)(rom_memory + 0x100);
   return(result);
}

static bool
validate_cartridge_header(unsigned char *rom_memory, size_t rom_size)
{
   Cartridge_Header *header = get_cartridge_header(rom_memory);

   static unsigned char logo[] =
   {
      0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
      0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
      0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
   };

   // NOTE(law): Confirm that the cartridge stored the Game Boy logo correctly.
   platform_log("Verifying logo...\n");

   assert(ARRAY_LENGTH(logo) == ARRAY_LENGTH(header->logo));
   for(unsigned int byte_index = 0; byte_index < ARRAY_LENGTH(logo); ++byte_index)
   {
      // TODO(law): CGB and later models noly check the top half of the logo,
      // i.e. th first 0x18 bytes.

      if(logo[byte_index] != header->logo[byte_index])
      {
         platform_log("ERROR: Logo mismatch at byte index %d ", byte_index);
         platform_log("(header: 0x%02X, expected: 0x%02X)\n", header->logo[byte_index], logo[byte_index]);

         return(false);
      }
   }

   platform_log("Verifying checksums...\n");

   unsigned char header_checksum = 0;
   for(unsigned short byte_offset = 0x0134; byte_offset <= 0x014C; ++byte_offset)
   {
      header_checksum = header_checksum - rom_memory[byte_offset] - 1;
   }

   if(header_checksum != header->header_checksum)
   {
      platform_log("ERROR: Computed header checksum did not match value in header. ");
      platform_log("(header: 0x%02X, computed: 0x%02X.\n", header_checksum, header->header_checksum);

      return(false);
   }

   unsigned short global_checksum = 0;
   for(unsigned int byte_offset = 0; byte_offset < rom_size; ++byte_offset)
   {
      global_checksum += (unsigned short)rom_memory[byte_offset];
   }

   global_checksum -= ((header->global_checksum >> 0) & 0x00FF);
   global_checksum -= ((header->global_checksum >> 8) & 0x00FF);

   header->global_checksum = endian_swap16(header->global_checksum);

   if(global_checksum != header->global_checksum)
   {
      // NOTE(law): The global checksum is not verified by the majority of games
      // (Pokemon Stadium's GB Tower emulator is an exception).
      platform_log("WARNING: Computed global checksum did not match value in header. ");
      platform_log("(header: 0x%04X, computed: %0x04X)\n", header->global_checksum, global_checksum);
   }

   platform_log("SUCCESS: The cartridge header was validated.\n");

   return(true);
}

static
void dump_cartridge_header(unsigned char *stream)
{
   Cartridge_Header *header = get_cartridge_header(stream);

   platform_log("CARTRIDGE HEADER:\n");

   platform_log("  ENTRY POINT: 0x%08x\n", header->entry_point);

   platform_log("  TITLE: %s\n", header->title);
   platform_log("  SGB FLAG: 0x%02X\n", header->sgb_flag);

   static char *cartridge_types[] =
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

   // assert(header->cartridge_type < ARRAY_LENGTH(cartridge_types));
   platform_log("  CARTRIDGE TYPE: %#x (%s)\n", header->cartridge_type, cartridge_types[header->cartridge_type]);
   platform_log("  ROM SIZE: %u KiB\n", 32 * (1 << header->rom_size));

   static char *sram_sizes[] =
   {
      "0 (no RAM)",
      "- (Unused)",
      "8 KiB (1 bank)",
      "32 KiB (4 banks of 8 KiB each)",
      "128 KiB (16 banks of 8 KiB each)",
      "64 KiB (8 banks of 8 KiB each)",
   };

   assert(header->ram_size < ARRAY_LENGTH(sram_sizes));
   platform_log("  RAM SIZE: %s\n", sram_sizes[header->ram_size]);

   static char *destination_codes[] =
   {
      "Japan (and possibly overseas)",
      "Overseas only",
   };

   assert(header->destination_code < ARRAY_LENGTH(destination_codes));
   platform_log("  DESTINATION CODE: %#x (%s)\n", header->destination_code, destination_codes[header->destination_code]);

   if(header->old_licensee_code == 0x33)
   {
      platform_log("  OLD LICENSEE CODE: UNUSED\n");
      platform_log("  NEW LICENSEE CODE: %.*s\n", 2, header->new_licensee_code);
   }
   else
   {
      platform_log("  OLD LICENSEE CODE: %#x\n", header->old_licensee_code);
      platform_log("  NEW LICENSEE CODE: UNUSED\n");
   }

   platform_log("  MASK ROM VERSION NUMBER: %#x\n", header->mask_rom_version_number);
   platform_log("  HEADER CHECKSUM: 0x%02X\n", header->header_checksum);
   platform_log("  GLOBAL CHECKSUM: 0x%04X\n", header->global_checksum);
}

static unsigned int
disassemble_instruction(unsigned char *stream, unsigned int offset)
{
   unsigned int initial_offset = offset;

   // NOTE(law): Print the address of the current instruction.
   platform_log("0x%04X  ", offset);

   unsigned char opcode = stream[offset++];
   if(opcode == 0xCB)
   {
      opcode = stream[offset++];

      // NOTE(law): Parse prefix instructions.
      switch(opcode)
      {
         // NOTE(law): Rotate and Shift instructions
         case 0x00: {platform_log("RLC B               ");} break;
         case 0x01: {platform_log("RLC C               ");} break;
         case 0x02: {platform_log("RLC D               ");} break;
         case 0x03: {platform_log("RLC E               ");} break;
         case 0x04: {platform_log("RLC H               ");} break;
         case 0x05: {platform_log("RLC L               ");} break;
         case 0x06: {platform_log("RLC (HL)            ");} break;
         case 0x07: {platform_log("RLC A               ");} break;

         case 0x08: {platform_log("RRC B               ");} break;
         case 0x09: {platform_log("RRC C               ");} break;
         case 0x0A: {platform_log("RRC D               ");} break;
         case 0x0B: {platform_log("RRC E               ");} break;
         case 0x0C: {platform_log("RRC H               ");} break;
         case 0x0D: {platform_log("RRC L               ");} break;
         case 0x0E: {platform_log("RRC (HL)            ");} break;
         case 0x0F: {platform_log("RRC A               ");} break;

         case 0x10: {platform_log("RL B                ");} break;
         case 0x11: {platform_log("RL C                ");} break;
         case 0x12: {platform_log("RL D                ");} break;
         case 0x13: {platform_log("RL E                ");} break;
         case 0x14: {platform_log("RL H                ");} break;
         case 0x15: {platform_log("RL L                ");} break;
         case 0x16: {platform_log("RL (HL)             ");} break;
         case 0x17: {platform_log("RL A                ");} break;

         case 0x18: {platform_log("RR B                ");} break;
         case 0x19: {platform_log("RR C                ");} break;
         case 0x1A: {platform_log("RR D                ");} break;
         case 0x1B: {platform_log("RR E                ");} break;
         case 0x1C: {platform_log("RR H                ");} break;
         case 0x1D: {platform_log("RR L                ");} break;
         case 0x1E: {platform_log("RR (HL)             ");} break;
         case 0x1F: {platform_log("RR A                ");} break;

         case 0x20: {platform_log("SLA B               ");} break;
         case 0x21: {platform_log("SLA C               ");} break;
         case 0x22: {platform_log("SLA D               ");} break;
         case 0x23: {platform_log("SLA E               ");} break;
         case 0x24: {platform_log("SLA H               ");} break;
         case 0x25: {platform_log("SLA L               ");} break;
         case 0x26: {platform_log("SLA (HL)            ");} break;
         case 0x27: {platform_log("SLA A               ");} break;

         case 0x28: {platform_log("SRA B               ");} break;
         case 0x29: {platform_log("SRA C               ");} break;
         case 0x2A: {platform_log("SRA D               ");} break;
         case 0x2B: {platform_log("SRA E               ");} break;
         case 0x2C: {platform_log("SRA H               ");} break;
         case 0x2D: {platform_log("SRA L               ");} break;
         case 0x2E: {platform_log("SRA (HL)            ");} break;
         case 0x2F: {platform_log("SRA A               ");} break;

         case 0x30: {platform_log("SWAP B              ");} break;
         case 0x31: {platform_log("SWAP C              ");} break;
         case 0x32: {platform_log("SWAP D              ");} break;
         case 0x33: {platform_log("SWAP E              ");} break;
         case 0x34: {platform_log("SWAP H              ");} break;
         case 0x35: {platform_log("SWAP L              ");} break;
         case 0x36: {platform_log("SWAP (HL)           ");} break;
         case 0x37: {platform_log("SWAP A              ");} break;

         case 0x38: {platform_log("SRL B               ");} break;
         case 0x39: {platform_log("SRL C               ");} break;
         case 0x3A: {platform_log("SRL D               ");} break;
         case 0x3B: {platform_log("SRL E               ");} break;
         case 0x3C: {platform_log("SRL H               ");} break;
         case 0x3D: {platform_log("SRL L               ");} break;
         case 0x3E: {platform_log("SRL (HL)            ");} break;
         case 0x3F: {platform_log("SRL A               ");} break;


            // NOTE(law): Single-bit Operation instructions
         case 0x40: {platform_log("BIT 0, B            ");} break;
         case 0x41: {platform_log("BIT 0, C            ");} break;
         case 0x42: {platform_log("BIT 0, D            ");} break;
         case 0x43: {platform_log("BIT 0, E            ");} break;
         case 0x44: {platform_log("BIT 0, H            ");} break;
         case 0x45: {platform_log("BIT 0, L            ");} break;
         case 0x46: {platform_log("BIT 0, (HL)         ");} break;
         case 0x47: {platform_log("BIT 0, A            ");} break;

         case 0x48: {platform_log("BIT 1, B            ");} break;
         case 0x49: {platform_log("BIT 1, C            ");} break;
         case 0x4A: {platform_log("BIT 1, D            ");} break;
         case 0x4B: {platform_log("BIT 1, E            ");} break;
         case 0x4C: {platform_log("BIT 1, H            ");} break;
         case 0x4D: {platform_log("BIT 1, L            ");} break;
         case 0x4E: {platform_log("BIT 1, (HL)         ");} break;
         case 0x4F: {platform_log("BIT 1, A            ");} break;

         case 0x50: {platform_log("BIT 2, B            ");} break;
         case 0x51: {platform_log("BIT 2, C            ");} break;
         case 0x52: {platform_log("BIT 2, D            ");} break;
         case 0x53: {platform_log("BIT 2, E            ");} break;
         case 0x54: {platform_log("BIT 2, H            ");} break;
         case 0x55: {platform_log("BIT 2, L            ");} break;
         case 0x56: {platform_log("BIT 2, (HL)         ");} break;
         case 0x57: {platform_log("BIT 2, A            ");} break;

         case 0x58: {platform_log("BIT 3, B            ");} break;
         case 0x59: {platform_log("BIT 3, C            ");} break;
         case 0x5A: {platform_log("BIT 3, D            ");} break;
         case 0x5B: {platform_log("BIT 3, E            ");} break;
         case 0x5C: {platform_log("BIT 3, H            ");} break;
         case 0x5D: {platform_log("BIT 3, L            ");} break;
         case 0x5E: {platform_log("BIT 3, (HL)         ");} break;
         case 0x5F: {platform_log("BIT 3, A            ");} break;

         case 0x60: {platform_log("BIT 4, B            ");} break;
         case 0x61: {platform_log("BIT 4, C            ");} break;
         case 0x62: {platform_log("BIT 4, D            ");} break;
         case 0x63: {platform_log("BIT 4, E            ");} break;
         case 0x64: {platform_log("BIT 4, H            ");} break;
         case 0x65: {platform_log("BIT 4, L            ");} break;
         case 0x66: {platform_log("BIT 4, (HL)         ");} break;
         case 0x67: {platform_log("BIT 4, A            ");} break;

         case 0x68: {platform_log("BIT 5, B            ");} break;
         case 0x69: {platform_log("BIT 5, C            ");} break;
         case 0x6A: {platform_log("BIT 5, D            ");} break;
         case 0x6B: {platform_log("BIT 5, E            ");} break;
         case 0x6C: {platform_log("BIT 5, H            ");} break;
         case 0x6D: {platform_log("BIT 5, L            ");} break;
         case 0x6E: {platform_log("BIT 5, (HL)         ");} break;
         case 0x6F: {platform_log("BIT 5, A            ");} break;

         case 0x70: {platform_log("BIT 6, B            ");} break;
         case 0x71: {platform_log("BIT 6, C            ");} break;
         case 0x72: {platform_log("BIT 6, D            ");} break;
         case 0x73: {platform_log("BIT 6, E            ");} break;
         case 0x74: {platform_log("BIT 6, H            ");} break;
         case 0x75: {platform_log("BIT 6, L            ");} break;
         case 0x76: {platform_log("BIT 6, (HL)         ");} break;
         case 0x77: {platform_log("BIT 6, A            ");} break;

         case 0x78: {platform_log("BIT 7, B            ");} break;
         case 0x79: {platform_log("BIT 7, C            ");} break;
         case 0x7A: {platform_log("BIT 7, D            ");} break;
         case 0x7B: {platform_log("BIT 7, E            ");} break;
         case 0x7C: {platform_log("BIT 7, H            ");} break;
         case 0x7D: {platform_log("BIT 7, L            ");} break;
         case 0x7E: {platform_log("BIT 7, (HL)         ");} break;
         case 0x7F: {platform_log("BIT 7, A            ");} break;

         case 0x80: {platform_log("RES 0, B            ");} break;
         case 0x81: {platform_log("RES 0, C            ");} break;
         case 0x82: {platform_log("RES 0, D            ");} break;
         case 0x83: {platform_log("RES 0, E            ");} break;
         case 0x84: {platform_log("RES 0, H            ");} break;
         case 0x85: {platform_log("RES 0, L            ");} break;
         case 0x86: {platform_log("RES 0, (HL)         ");} break;
         case 0x87: {platform_log("RES 0, A            ");} break;

         case 0x88: {platform_log("RES 1, B            ");} break;
         case 0x89: {platform_log("RES 1, C            ");} break;
         case 0x8A: {platform_log("RES 1, D            ");} break;
         case 0x8B: {platform_log("RES 1, E            ");} break;
         case 0x8C: {platform_log("RES 1, H            ");} break;
         case 0x8D: {platform_log("RES 1, L            ");} break;
         case 0x8E: {platform_log("RES 1, (HL)         ");} break;
         case 0x8F: {platform_log("RES 1, A            ");} break;

         case 0x90: {platform_log("RES 2, B            ");} break;
         case 0x91: {platform_log("RES 2, C            ");} break;
         case 0x92: {platform_log("RES 2, D            ");} break;
         case 0x93: {platform_log("RES 2, E            ");} break;
         case 0x94: {platform_log("RES 2, H            ");} break;
         case 0x95: {platform_log("RES 2, L            ");} break;
         case 0x96: {platform_log("RES 2, (HL)         ");} break;
         case 0x97: {platform_log("RES 2, A            ");} break;

         case 0x98: {platform_log("RES 3, B            ");} break;
         case 0x99: {platform_log("RES 3, C            ");} break;
         case 0x9A: {platform_log("RES 3, D            ");} break;
         case 0x9B: {platform_log("RES 3, E            ");} break;
         case 0x9C: {platform_log("RES 3, H            ");} break;
         case 0x9D: {platform_log("RES 3, L            ");} break;
         case 0x9E: {platform_log("RES 3, (HL)         ");} break;
         case 0x9F: {platform_log("RES 3, A            ");} break;

         case 0xA0: {platform_log("RES 4, B            ");} break;
         case 0xA1: {platform_log("RES 4, C            ");} break;
         case 0xA2: {platform_log("RES 4, D            ");} break;
         case 0xA3: {platform_log("RES 4, E            ");} break;
         case 0xA4: {platform_log("RES 4, H            ");} break;
         case 0xA5: {platform_log("RES 4, L            ");} break;
         case 0xA6: {platform_log("RES 4, (HL)         ");} break;
         case 0xA7: {platform_log("RES 4, A            ");} break;

         case 0xA8: {platform_log("RES 5, B            ");} break;
         case 0xA9: {platform_log("RES 5, C            ");} break;
         case 0xAA: {platform_log("RES 5, D            ");} break;
         case 0xAB: {platform_log("RES 5, E            ");} break;
         case 0xAC: {platform_log("RES 5, H            ");} break;
         case 0xAD: {platform_log("RES 5, L            ");} break;
         case 0xAE: {platform_log("RES 5, (HL)         ");} break;
         case 0xAF: {platform_log("RES 5, A            ");} break;

         case 0xB0: {platform_log("RES 6, B            ");} break;
         case 0xB1: {platform_log("RES 6, C            ");} break;
         case 0xB2: {platform_log("RES 6, D            ");} break;
         case 0xB3: {platform_log("RES 6, E            ");} break;
         case 0xB4: {platform_log("RES 6, H            ");} break;
         case 0xB5: {platform_log("RES 6, L            ");} break;
         case 0xB6: {platform_log("RES 6, (HL)         ");} break;
         case 0xB7: {platform_log("RES 6, A            ");} break;

         case 0xB8: {platform_log("RES 7, B            ");} break;
         case 0xB9: {platform_log("RES 7, C            ");} break;
         case 0xBA: {platform_log("RES 7, D            ");} break;
         case 0xBB: {platform_log("RES 7, E            ");} break;
         case 0xBC: {platform_log("RES 7, H            ");} break;
         case 0xBD: {platform_log("RES 7, L            ");} break;
         case 0xBE: {platform_log("RES 7, (HL)         ");} break;
         case 0xBF: {platform_log("RES 7, A            ");} break;

         case 0xC0: {platform_log("SET 0, B            ");} break;
         case 0xC1: {platform_log("SET 0, C            ");} break;
         case 0xC2: {platform_log("SET 0, D            ");} break;
         case 0xC3: {platform_log("SET 0, E            ");} break;
         case 0xC4: {platform_log("SET 0, H            ");} break;
         case 0xC5: {platform_log("SET 0, L            ");} break;
         case 0xC6: {platform_log("SET 0, (HL)         ");} break;
         case 0xC7: {platform_log("SET 0, A            ");} break;

         case 0xC8: {platform_log("SET 1, B            ");} break;
         case 0xC9: {platform_log("SET 1, C            ");} break;
         case 0xCA: {platform_log("SET 1, D            ");} break;
         case 0xCB: {platform_log("SET 1, E            ");} break;
         case 0xCC: {platform_log("SET 1, H            ");} break;
         case 0xCD: {platform_log("SET 1, L            ");} break;
         case 0xCE: {platform_log("SET 1, (HL)         ");} break;
         case 0xCF: {platform_log("SET 1, A            ");} break;

         case 0xD0: {platform_log("SET 2, B            ");} break;
         case 0xD1: {platform_log("SET 2, C            ");} break;
         case 0xD2: {platform_log("SET 2, D            ");} break;
         case 0xD3: {platform_log("SET 2, E            ");} break;
         case 0xD4: {platform_log("SET 2, H            ");} break;
         case 0xD5: {platform_log("SET 2, L            ");} break;
         case 0xD6: {platform_log("SET 2, (HL)         ");} break;
         case 0xD7: {platform_log("SET 2, A            ");} break;

         case 0xD8: {platform_log("SET 3, B            ");} break;
         case 0xD9: {platform_log("SET 3, C            ");} break;
         case 0xDA: {platform_log("SET 3, D            ");} break;
         case 0xDB: {platform_log("SET 3, E            ");} break;
         case 0xDC: {platform_log("SET 3, H            ");} break;
         case 0xDD: {platform_log("SET 3, L            ");} break;
         case 0xDE: {platform_log("SET 3, (HL)         ");} break;
         case 0xDF: {platform_log("SET 3, A            ");} break;

         case 0xE0: {platform_log("SET 4, B            ");} break;
         case 0xE1: {platform_log("SET 4, C            ");} break;
         case 0xE2: {platform_log("SET 4, D            ");} break;
         case 0xE3: {platform_log("SET 4, E            ");} break;
         case 0xE4: {platform_log("SET 4, H            ");} break;
         case 0xE5: {platform_log("SET 4, L            ");} break;
         case 0xE6: {platform_log("SET 4, (HL)         ");} break;
         case 0xE7: {platform_log("SET 4, A            ");} break;

         case 0xE8: {platform_log("SET 5, B            ");} break;
         case 0xE9: {platform_log("SET 5, C            ");} break;
         case 0xEA: {platform_log("SET 5, D            ");} break;
         case 0xEB: {platform_log("SET 5, E            ");} break;
         case 0xEC: {platform_log("SET 5, H            ");} break;
         case 0xED: {platform_log("SET 5, L            ");} break;
         case 0xEE: {platform_log("SET 5, (HL)         ");} break;
         case 0xEF: {platform_log("SET 5, A            ");} break;

         case 0xF0: {platform_log("SET 6, B            ");} break;
         case 0xF1: {platform_log("SET 6, C            ");} break;
         case 0xF2: {platform_log("SET 6, D            ");} break;
         case 0xF3: {platform_log("SET 6, E            ");} break;
         case 0xF4: {platform_log("SET 6, H            ");} break;
         case 0xF5: {platform_log("SET 6, L            ");} break;
         case 0xF6: {platform_log("SET 6, (HL)         ");} break;
         case 0xF7: {platform_log("SET 6, A            ");} break;

         case 0xF8: {platform_log("SET 7, B            ");} break;
         case 0xF9: {platform_log("SET 7, C            ");} break;
         case 0xFA: {platform_log("SET 7, D            ");} break;
         case 0xFB: {platform_log("SET 7, E            ");} break;
         case 0xFC: {platform_log("SET 7, H            ");} break;
         case 0xFD: {platform_log("SET 7, L            ");} break;
         case 0xFE: {platform_log("SET 7, (HL)         ");} break;
         case 0xFF: {platform_log("SET 7, A            ");} break;

         default: {assert(!"UNHANDLED OPCODE");} break;
      }
   }
   else
   {
      // NOTE(law): Parse non-prefix opcode.
      switch(opcode)
      {
         // NOTE(law): 8-bit load instructions
         case 0x40: {platform_log("LD B, B             ");} break;
         case 0x41: {platform_log("LD B, C             ");} break;
         case 0x42: {platform_log("LD B, D             ");} break;
         case 0x43: {platform_log("LD B, E             ");} break;
         case 0x44: {platform_log("LD B, H             ");} break;
         case 0x45: {platform_log("LD B, L             ");} break;
         case 0x46: {platform_log("LD B, (HL)          ");} break;
         case 0x47: {platform_log("LD B, A             ");} break;

         case 0x48: {platform_log("LD C, B             ");} break;
         case 0x49: {platform_log("LD C, C             ");} break;
         case 0x4A: {platform_log("LD C, D             ");} break;
         case 0x4B: {platform_log("LD C, E             ");} break;
         case 0x4C: {platform_log("LD C, H             ");} break;
         case 0x4D: {platform_log("LD C, L             ");} break;
         case 0x4E: {platform_log("LD C, (HL)          ");} break;
         case 0x4F: {platform_log("LD C, A             ");} break;

         case 0x50: {platform_log("LD D, B             ");} break;
         case 0x51: {platform_log("LD D, C             ");} break;
         case 0x52: {platform_log("LD D, D             ");} break;
         case 0x53: {platform_log("LD D, E             ");} break;
         case 0x54: {platform_log("LD D, H             ");} break;
         case 0x55: {platform_log("LD D, L             ");} break;
         case 0x56: {platform_log("LD D, (HL)          ");} break;
         case 0x57: {platform_log("LD D, A             ");} break;

         case 0x58: {platform_log("LD E, B             ");} break;
         case 0x59: {platform_log("LD E, C             ");} break;
         case 0x5A: {platform_log("LD E, D             ");} break;
         case 0x5B: {platform_log("LD E, E             ");} break;
         case 0x5C: {platform_log("LD E, H             ");} break;
         case 0x5D: {platform_log("LD E, L             ");} break;
         case 0x5E: {platform_log("LD E, (HL)          ");} break;
         case 0x5F: {platform_log("LD E, A             ");} break;

         case 0x60: {platform_log("LD H, B             ");} break;
         case 0x61: {platform_log("LD H, C             ");} break;
         case 0x62: {platform_log("LD H, D             ");} break;
         case 0x63: {platform_log("LD H, E             ");} break;
         case 0x64: {platform_log("LD H, H             ");} break;
         case 0x65: {platform_log("LD H, L             ");} break;
         case 0x66: {platform_log("LD H, (HL)          ");} break;
         case 0x67: {platform_log("LD H, A             ");} break;

         case 0x68: {platform_log("LD L, B             ");} break;
         case 0x69: {platform_log("LD L, C             ");} break;
         case 0x6A: {platform_log("LD L, D             ");} break;
         case 0x6B: {platform_log("LD L, E             ");} break;
         case 0x6C: {platform_log("LD L, H             ");} break;
         case 0x6D: {platform_log("LD L, L             ");} break;
         case 0x6E: {platform_log("LD L, (HL)          ");} break;
         case 0x6F: {platform_log("LD L, A             ");} break;

         case 0x70: {platform_log("LD (HL), B          ");} break;
         case 0x71: {platform_log("LD (HL), C          ");} break;
         case 0x72: {platform_log("LD (HL), D          ");} break;
         case 0x73: {platform_log("LD (HL), E          ");} break;
         case 0x74: {platform_log("LD (HL), H          ");} break;
         case 0x75: {platform_log("LD (HL), L          ");} break;
         case 0x77: {platform_log("LD (HL), A          ");} break;

         case 0x78: {platform_log("LD A, B             ");} break;
         case 0x79: {platform_log("LD A, C             ");} break;
         case 0x7A: {platform_log("LD A, D             ");} break;
         case 0x7B: {platform_log("LD A, E             ");} break;
         case 0x7C: {platform_log("LD A, H             ");} break;
         case 0x7D: {platform_log("LD A, L             ");} break;
         case 0x7E: {platform_log("LD A, (HL)          ");} break;
         case 0x7F: {platform_log("LD A, A             ");} break;

         case 0x06: {platform_log("LD B, 0x%02X          ", stream[offset++]);} break;
         case 0x0E: {platform_log("LD C, 0x%02X          ", stream[offset++]);} break;
         case 0x16: {platform_log("LD D, 0x%02X          ", stream[offset++]);} break;
         case 0x1E: {platform_log("LD E, 0x%02X          ", stream[offset++]);} break;
         case 0x26: {platform_log("LD H, 0x%02X          ", stream[offset++]);} break;
         case 0x2E: {platform_log("LD L, 0x%02X          ", stream[offset++]);} break;
         case 0x36: {platform_log("LD (HL), 0x%02X       ", stream[offset++]);} break;
         case 0x3E: {platform_log("LD A, 0x%02X          ", stream[offset++]);} break;

         case 0x0A: {platform_log("LD A, (BC)          ");} break;
         case 0x1A: {platform_log("LD A, (DE)          ");} break;

         case 0xFA:
         {
            unsigned short operand = *((unsigned short *)(stream + offset));
            offset += 2;
            platform_log("LD A, (0x%04X)      ", operand);
         } break;

         case 0x02: {platform_log("LD (BC), A          ");} break;
         case 0x12: {platform_log("LD (DE), A          ");} break;

         case 0xEA:
         {
            unsigned short operand = *((unsigned short *)(stream + offset));
            offset += 2;
            platform_log("LD (0x%04X), A      ", operand);
         } break;

         case 0xF2: {platform_log("LD A, (FF00 + C)    ");} break;
         case 0xE2: {platform_log("LD (FF00 + C), A    ");} break;

         case 0xF0: {platform_log("LD A, (FF00 + 0x%02X) ", stream[offset++]);} break;
         case 0xE0: {platform_log("LD (FF00 + 0x%02X), A ", stream[offset++]);} break;

         case 0x22: {platform_log("LDI (HL), A         ");} break;
         case 0x32: {platform_log("LDD (HL), A         ");} break;
         case 0x2A: {platform_log("LDI A, (HL)         ");} break;
         case 0x3A: {platform_log("LDD A, (HL)         ");} break;

         case 0xF9: {platform_log("LD SP, HL           ");} break;

         case 0xC5: {platform_log("PUSH BC             ");} break;
         case 0xD5: {platform_log("PUSH DE             ");} break;
         case 0xE5: {platform_log("PUSH HL             ");} break;
         case 0xF5: {platform_log("PUSH AF             ");} break;

         case 0xC1: {platform_log("POP BC              ");} break;
         case 0xD1: {platform_log("POP DE              ");} break;
         case 0xE1: {platform_log("POP HL              ");} break;
         case 0xF1: {platform_log("POP AF              ");} break;


            // NOTE(law): 16-bit load instructions
         case 0x08:
         {
            unsigned short operand = *((unsigned short *)(stream + offset));
            offset += 2;
            platform_log("LD 0x%04X, SP       ", operand);
         } break;

         case 0x01:
         {
            unsigned short operand = *((unsigned short *)(stream + offset));
            offset += 2;
            platform_log("LD BC, 0x%04X       ", operand);
         } break;

         case 0x11:
         {
            unsigned short operand = *((unsigned short *)(stream + offset));
            offset += 2;
            platform_log("LD DE, 0x%04X       ", operand);
         } break;

         case 0x21:
         {
            unsigned short operand = *((unsigned short *)(stream + offset));
            offset += 2;
            platform_log("LD HL, 0x%04X       ", operand);
         } break;

         case 0x31:
         {
            unsigned short operand = *((unsigned short *)(stream + offset));
            offset += 2;
            platform_log("LD SP, 0x%04X       ", operand);
         } break;


         // NOTE(law): Rotate and Shift instructions
         case 0x07: {platform_log("RLCA                ");} break;
         case 0x17: {platform_log("RLA                 ");} break;
         case 0x0F: {platform_log("RRCA                ");} break;
         case 0x1F: {platform_log("RRA                 ");} break;


            // NOTE(law): 8-bit Arithmetic/Logic instructions
         case 0x80: {platform_log("ADD A, B            ");} break;
         case 0x81: {platform_log("ADD A, C            ");} break;
         case 0x82: {platform_log("ADD A, D            ");} break;
         case 0x83: {platform_log("ADD A, E            ");} break;
         case 0x84: {platform_log("ADD A, H            ");} break;
         case 0x85: {platform_log("ADD A, L            ");} break;
         case 0x86: {platform_log("ADD A, (HL)         ");} break;
         case 0x87: {platform_log("ADD A               ");} break;

         case 0xC6:
         {
            unsigned char operand = *(stream + offset++);
            platform_log("ADD A, 0x%02X         ", operand);
         } break;

         case 0x88: {platform_log("ADC A, B            ");} break;
         case 0x89: {platform_log("ADC A, C            ");} break;
         case 0x8A: {platform_log("ADC A, D            ");} break;
         case 0x8B: {platform_log("ADC A, E            ");} break;
         case 0x8C: {platform_log("ADC A, H            ");} break;
         case 0x8D: {platform_log("ADC A, L            ");} break;
         case 0x8E: {platform_log("ADC A, (HL)         ");} break;
         case 0x8F: {platform_log("ADC A, A            ");} break;

         case 0xCE:
         {
            unsigned char operand = *(stream + offset++);
            platform_log("ADC A, 0x%02X         ", operand);
         } break;

         case 0x90: {platform_log("SUB A, B            ");} break;
         case 0x91: {platform_log("SUB A, C            ");} break;
         case 0x92: {platform_log("SUB A, D            ");} break;
         case 0x93: {platform_log("SUB A, E            ");} break;
         case 0x94: {platform_log("SUB A, H            ");} break;
         case 0x95: {platform_log("SUB A, L            ");} break;
         case 0x96: {platform_log("SUB A, (HL)         ");} break;
         case 0x97: {platform_log("SUB A               ");} break;

         case 0xD6:
         {
            unsigned char operand = *(stream + offset++);
            platform_log("SUB 0x%02X            ", operand);
         } break;

         case 0x98: {platform_log("SBC A, B            ");} break;
         case 0x99: {platform_log("SBC A, C            ");} break;
         case 0x9A: {platform_log("SBC A, D            ");} break;
         case 0x9B: {platform_log("SBC A, E            ");} break;
         case 0x9C: {platform_log("SBC A, H            ");} break;
         case 0x9D: {platform_log("SBC A, L            ");} break;
         case 0x9E: {platform_log("SBC A, (HL)         ");} break;
         case 0x9F: {platform_log("SBC A, A            ");} break;

         case 0xDE: {platform_log("SBC A, 0x%02X         ", stream[offset++]);} break;

         case 0x27: {platform_log("DAA                 ");} break;
         case 0x2F: {platform_log("CPL                 ");} break;

         case 0xA8: {platform_log("XOR B               ");} break;
         case 0xA9: {platform_log("XOR C               ");} break;
         case 0xAA: {platform_log("XOR D               ");} break;
         case 0xAB: {platform_log("XOR E               ");} break;
         case 0xAC: {platform_log("XOR H               ");} break;
         case 0xAD: {platform_log("XOR L               ");} break;
         case 0xAE: {platform_log("XOR (HL)            ");} break;
         case 0xAF: {platform_log("XOR A               ");} break;

         case 0xEE: {platform_log("XOR 0x%02X            ", stream[offset++]);} break;

         case 0xB0: {platform_log("OR B                ");} break;
         case 0xB1: {platform_log("OR C                ");} break;
         case 0xB2: {platform_log("OR D                ");} break;
         case 0xB3: {platform_log("OR E                ");} break;
         case 0xB4: {platform_log("OR H                ");} break;
         case 0xB5: {platform_log("OR L                ");} break;
         case 0xB6: {platform_log("OR (HL)             ");} break;
         case 0xB7: {platform_log("OR A                ");} break;

         case 0xF6: {platform_log("OR 0x%02X             ", stream[offset++]);} break;

         case 0xA0: {platform_log("AND B               ");} break;
         case 0xA1: {platform_log("AND C               ");} break;
         case 0xA2: {platform_log("AND D               ");} break;
         case 0xA3: {platform_log("AND E               ");} break;
         case 0xA4: {platform_log("AND H               ");} break;
         case 0xA5: {platform_log("AND L               ");} break;
         case 0xA6: {platform_log("AND (HL)            ");} break;
         case 0xA7: {platform_log("AND A               ");} break;

         case 0xE6: {platform_log("AND 0x%02X            ", stream[offset++]);} break;

         case 0xB8: {platform_log("CP B                ");} break;
         case 0xB9: {platform_log("CP C                ");} break;
         case 0xBA: {platform_log("CP D                ");} break;
         case 0xBB: {platform_log("CP E                ");} break;
         case 0xBC: {platform_log("CP H                ");} break;
         case 0xBD: {platform_log("CP L                ");} break;
         case 0xBE: {platform_log("CP (HL)             ");} break;
         case 0xBF: {platform_log("CP A                ");} break;

         case 0xFE: {platform_log("CP 0x%02X             ", stream[offset++]);} break;

         case 0x04: {platform_log("INC B               ");} break;
         case 0x0C: {platform_log("INC C               ");} break;
         case 0x14: {platform_log("INC D               ");} break;
         case 0x1C: {platform_log("INC E               ");} break;
         case 0x24: {platform_log("INC H               ");} break;
         case 0x2C: {platform_log("INC L               ");} break;
         case 0x34: {platform_log("INC (HL)            ");} break;
         case 0x3C: {platform_log("INC A               ");} break;

         case 0x05: {platform_log("DEC B               ");} break;
         case 0x0D: {platform_log("DEC C               ");} break;
         case 0x15: {platform_log("DEC D               ");} break;
         case 0x1D: {platform_log("DEC E               ");} break;
         case 0x25: {platform_log("DEC H               ");} break;
         case 0x2D: {platform_log("DEC L               ");} break;
         case 0x35: {platform_log("DEC (HL)            ");} break;
         case 0x3D: {platform_log("DEC A               ");} break;


         // NOTE(law): 16-bit Arithmetic/Logic instructions
         case 0x09: {platform_log("ADD HL, BC          ");} break;
         case 0x19: {platform_log("ADD HL, DE          ");} break;
         case 0x29: {platform_log("ADD HL, HL          ");} break;
         case 0x39: {platform_log("ADD HL, SP          ");} break;

         case 0x03: {platform_log("INC BC              ");} break;
         case 0x13: {platform_log("INC DE              ");} break;
         case 0x23: {platform_log("INC HL              ");} break;
         case 0x33: {platform_log("INC SP              ");} break;

         case 0x0B: {platform_log("DEC BC              ");} break;
         case 0x1B: {platform_log("DEC DE              ");} break;
         case 0x2B: {platform_log("DEC HL              ");} break;
         case 0x3B: {platform_log("DEC SP              ");} break;

         case 0xE8: {platform_log("ADD SP, 0x%02X        ", stream[offset++]);} break;
         case 0xF8: {platform_log("LD HL, SP + 0x%02X    ", stream[offset++]);} break;


         // NOTE(law): CPU Control instructions
         case 0x3F: {platform_log("CCF                 ");} break;
         case 0x37: {platform_log("SCF                 ");} break;
         case 0x00: {platform_log("NOP                 ");} break;
         case 0x76: {platform_log("HALT                ");} break;
         case 0x10: {platform_log("STOP 0x%02X           ", stream[offset++]);} break;
         case 0xF3: {platform_log("DI                  ");} break;
         case 0xFB: {platform_log("EI                  ");} break;


         // NOTE(law): Jump instructions
         case 0xE9: {platform_log("JP HL               ");} break;

         case 0xC3:
         {
            unsigned short operand = *(unsigned short *)(stream + offset);
            offset += 2;
            platform_log("JP 0x%04X           ", operand);
         } break;

         case 0xC2:
         {
            unsigned short operand = *(unsigned short *)(stream + offset);
            offset += 2;
            platform_log("JP NZ, 0x%04X       ", operand);
         } break;

         case 0xCA:
         {
            unsigned short operand = *(unsigned short *)(stream + offset);
            offset += 2;
            platform_log("JP Z, 0x%04X        ", operand);
         } break;

         case 0xD2:
         {
            unsigned short operand = *(unsigned short *)(stream + offset);
            offset += 2;
            platform_log("JP NC, 0x%04X       ", operand);
         } break;

         case 0xDA:
         {
            unsigned short operand = *(unsigned short *)(stream + offset);
            offset += 2;
            platform_log("JP C, 0x%04X        ", operand);
         } break;

         case 0x18: {platform_log("JR PC + 0x%02X        ", stream[offset++]);} break;
         case 0x20: {platform_log("JR NZ, PC + 0x%02X    ", stream[offset++]);} break;
         case 0x28: {platform_log("JR Z, PC + 0x%02X     ", stream[offset++]);} break;
         case 0x30: {platform_log("JR NC, PC + 0x%02X    ", stream[offset++]);} break;
         case 0x38: {platform_log("JR C, PC + 0x%02X     ", stream[offset++]);} break;

         case 0xC4:
         {
            unsigned short operand = *((unsigned short *)(stream + offset));
            offset += 2;
            platform_log("CALL NZ, 0x%04X     ", operand);
         } break;

         case 0xCC:
         {
            unsigned short operand = *((unsigned short *)(stream + offset));
            offset += 2;
            platform_log("CALL Z, 0x%04X      ", operand);
         } break;

         case 0xCD:
         {
            unsigned short operand = *((unsigned short *)(stream + offset));
            offset += 2;
            platform_log("CALL 0x%04X         ", operand);
         } break;

         case 0xD4:
         {
            unsigned short operand = *((unsigned short *)(stream + offset));
            offset += 2;
            platform_log("CALL NC, 0x%04X     ", operand);
         } break;

         case 0xDC:
         {
            unsigned short operand = *((unsigned short *)(stream + offset));
            offset += 2;
            platform_log("CALL C, 0x%04X      ", operand);
         } break;

         case 0xC0: {platform_log("RET NZ              ");} break;
         case 0xC8: {platform_log("RET Z               ");} break;
         case 0xC9: {platform_log("RET                 ");} break;
         case 0xD0: {platform_log("RET NC              ");} break;
         case 0xD8: {platform_log("RET C               ");} break;

         case 0xD9: {platform_log("RETI                ");} break;

         case 0xC7: {platform_log("RST 00H             ");} break;
         case 0xCF: {platform_log("RST 08H             ");} break;
         case 0xD7: {platform_log("RST 10H             ");} break;
         case 0xDF: {platform_log("RST 18H             ");} break;
         case 0xE7: {platform_log("RST 20H             ");} break;
         case 0xEF: {platform_log("RST 28H             ");} break;
         case 0xF7: {platform_log("RST 30H             ");} break;
         case 0xFF: {platform_log("RST 38H             ");} break;

         case 0xD3: {platform_log("ILLEGAL_D3          ");} break;
         case 0xDB: {platform_log("ILLEGAL_DB          ");} break;
         case 0xDD: {platform_log("ILLEGAL_DD          ");} break;
         case 0xE3: {platform_log("ILLEGAL_E3          ");} break;
         case 0xE4: {platform_log("ILLEGAL_E4          ");} break;
         case 0xEB: {platform_log("ILLEGAL_EB          ");} break;
         case 0xEC: {platform_log("ILLEGAL_EC          ");} break;
         case 0xED: {platform_log("ILLEGAL_ED          ");} break;
         case 0xF4: {platform_log("ILLEGAL_F4          ");} break;
         case 0xFC: {platform_log("ILLEGAL_FC          ");} break;
         case 0xFD: {platform_log("ILLEGAL_FD          ");} break;

         default: {assert(!"UNHANDLED OPCODE");} break;
      }
   }

   platform_log("    ; ");
   for(unsigned int index = initial_offset; index < offset; ++index)
   {
      platform_log("%02X ", stream[index]);
   }

   platform_log("\n");

   unsigned int result = offset - initial_offset;
   return(result);
}

static void
disassemble_stream(unsigned char *stream, unsigned int offset, unsigned int byte_count)
{
   // TODO(law): 16-bit operations are not being endian swapped in this
   // function.

   unsigned int start = offset;
   while((offset - start) < byte_count)
   {
      offset += disassemble_instruction(stream, offset);
   }
}

static unsigned char register_a;
static unsigned char register_b;
static unsigned char register_c;
static unsigned char register_d;
static unsigned char register_e;
static unsigned char register_f;
static unsigned char register_h;
static unsigned char register_l;

static unsigned short register_pc;
static unsigned short register_sp;

static bool halt;
static bool stop;
static bool ime;

#define REGISTER_BC (((unsigned short)register_b << 8) | (unsigned short)register_c)
#define REGISTER_DE (((unsigned short)register_d << 8) | (unsigned short)register_e)
#define REGISTER_HL (((unsigned short)register_h << 8) | (unsigned short)register_l)
#define REGISTER_AF (((unsigned short)register_a << 8) | (unsigned short)register_f)

#define FLAG_Z_BIT 7
#define FLAG_N_BIT 6
#define FLAG_H_BIT 5
#define FLAG_C_BIT 4

#define FLAG_Z_MASK (1 << FLAG_Z_BIT)
#define FLAG_N_MASK (1 << FLAG_N_BIT)
#define FLAG_H_MASK (1 << FLAG_H_BIT)
#define FLAG_C_MASK (1 << FLAG_C_BIT)

#define FLAG_Z ((register_f >> FLAG_Z_BIT) & 0x1)
#define FLAG_N ((register_f >> FLAG_N_BIT) & 0x1)
#define FLAG_H ((register_f >> FLAG_H_BIT) & 0x1)
#define FLAG_C ((register_f >> FLAG_C_BIT) & 0x1)

#define REGISTER_IE stream[0xFFFF]
#define REGISTER_IF stream[0xFF0F]

#define INTERRUPT_VBLANK_BIT   0
#define INTERRUPT_LCD_STAT_BIT 1
#define INTERRUPT_TIMER_BIT    2
#define INTERRUPT_SERIAL_BIT   3
#define INTERRUPT_JOYPAD_BIT   4

#define INTERRUPT_VBLANK_MASK   (1 << INTERRUPT_VBLANK_BIT)
#define INTERRUPT_LCD_STAT_MASK (1 << INTERRUPT_LCD_STAT_BIT)
#define INTERRUPT_TIMER_MASK    (1 << INTERRUPT_TIMER_BIT)
#define INTERRUPT_SERIAL_MASK   (1 << INTERRUPT_SERIAL_BIT)
#define INTERRUPT_JOYPAD_MASK   (1 << INTERRUPT_JOYPAD_BIT)

#define ENABLE_VBLANK   ((REGISTER_IE >> INTERRUPT_VBLANK_BIT)   & 0x1)
#define ENABLE_LCD_STAT ((REGISTER_IE >> INTERRUPT_LCD_STAT_BIT) & 0x1)
#define ENABLE_TIMER    ((REGISTER_IE >> INTERRUPT_TIMER_BIT)    & 0x1)
#define ENABLE_SERIAL   ((REGISTER_IE >> INTERRUPT_SERIAL_BIT)   & 0x1)
#define ENABLE_JOYPAD   ((REGISTER_IE >> INTERRUPT_JOYPAD_BIT)   & 0x1)

#define REQUEST_VBLANK   ((REGISTER_IF >> INTERRUPT_VBLANK_BIT)   & 0x1)
#define REQUEST_LCD_STAT ((REGISTER_IF >> INTERRUPT_LCD_STAT_BIT) & 0x1)
#define REQUEST_TIMER    ((REGISTER_IF >> INTERRUPT_TIMER_BIT)    & 0x1)
#define REQUEST_SERIAL   ((REGISTER_IF >> INTERRUPT_SERIAL_BIT)   & 0x1)
#define REQUEST_JOYPAD   ((REGISTER_IF >> INTERRUPT_JOYPAD_BIT)   & 0x1)

#define JOYPAD_DOWN_START_BIT 3
#define JOYPAD_UP_SELECT_BIT  2
#define JOYPAD_LEFT_B_BIT     1
#define JOYPAD_RIGHT_A_BIT    0

#define JOYPAD_DOWN_START_MASK (1 << JOYPAD_DOWN_START_MASK)
#define JOYPAD_UP_SELECT_MASK  (1 << JOYPAD_UP_SELECT_MASK
#define JOYPAD_LEFT_B_MASK     (1 << JOYPAD_LEFT_B_MASK)
#define JOYPAD_RIGHT_A_MASK    (1 << JOYPAD_RIGHT_A_MASK)

static void
add(unsigned char value)
{
   // NOTE(law): Compute these values before updating register A for use in the
   // flag calculations.
   unsigned short extended_sum = (unsigned short)register_a + (unsigned short)value;
   unsigned char half_sum = (register_a & 0xF) + (value & 0xF);

   register_a = (unsigned char)extended_sum;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((register_a == 0) << FLAG_Z_BIT);

   // NOTE(law): Always reset the Subtraction flag.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag if a carry occurs from bit 3 to 4.
   register_f = (register_f & ~FLAG_H_MASK) | (((half_sum & 0x10) == 0x10) << FLAG_H_BIT);

   // NOTE(law): Set the Half Carry flag if a carry occurs from bit 7.
   register_f = (register_f & ~FLAG_C_MASK) | ((extended_sum > 0xFF) << FLAG_C_BIT);
}

static void
adc(unsigned char value)
{
   // NOTE(law): Compute these values before updating register A for use in the
   // flag calculations.
   unsigned short extended_sum = (unsigned short)register_a + (unsigned short)value + FLAG_C;
   unsigned char half_sum = (register_a & 0xF) + (value & 0xF) + FLAG_C;

   register_a = (unsigned char)extended_sum;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((register_a == 0) << FLAG_Z_BIT);

   // NOTE(law): Always reset the Subtraction flag.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag if a carry occurs from bit 3 to 4.
   register_f = (register_f & ~FLAG_H_MASK) | (((half_sum & 0x10) == 0x10) << FLAG_H_BIT);

   // NOTE(law): Set the Carry flag if adding the full 8 bits of register A and
   // the incoming value would create a sum greater than the maximum byte value
   // 0xFF (assuming the types used avoided an overflow).
   register_f = (register_f & ~FLAG_C_MASK) | ((extended_sum > 0xFF) << FLAG_C_BIT);
}

static void
sub(unsigned char value)
{
   // NOTE(law): Compute these values before updating register A for use in the
   // flag calculations.
   bool is_negative = (signed char)register_a < (signed char)value;
   bool is_half_negative = (signed char)(register_a & 0xF) < (signed char)(value & 0xF);

   register_a -= value;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((register_a == 0) << FLAG_Z_BIT);

   // NOTE(law): Always set the Subtraction flag.
   register_f |= FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag if subractring the low 4 bits of register A
   // and the incoming value sets bit 4 of the resulting sum.
   register_f = (register_f & ~FLAG_H_MASK) | (is_half_negative << FLAG_H_BIT);

   // NOTE(law): Set the Carry flag if the result is less than zero.
   register_f = (register_f & ~FLAG_C_MASK) | (is_negative << FLAG_C_BIT);
}

static void
sbc(unsigned char value)
{
   signed char value_and_carry = (signed char)value + FLAG_C;

   bool is_negative = (signed char)register_a < value_and_carry;
   bool is_half_negative = (signed char)(register_a & 0xF) < (value_and_carry & 0xF);

   register_a -= (value);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((register_a == 0) << FLAG_Z_BIT);

   // NOTE(law): Always set the Subtraction flag.
   register_f |= FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag if subractring the low 4 bits of register A
   // and the incoming value sets bit 4 of the resulting sum.
   register_f = (register_f & ~FLAG_H_MASK) | (is_half_negative << FLAG_H_BIT);

   // NOTE(law): Set the Carry flag if the result is less than zero.
   register_f = (register_f & ~FLAG_C_MASK) | (is_negative << FLAG_C_BIT);
}

static void
xor(unsigned char value)
{
   register_a ^= value;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((register_a == 0) << FLAG_Z_BIT);

   // NOTE(law): Always reset the Subtraction mask.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): Always reset the Half Carry mask.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Always reset the Carry mask.
   register_f &= ~FLAG_C_MASK;
}

static void
or(unsigned char value)
{
   register_a |= value;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((register_a == 0) << FLAG_Z_BIT);

   // NOTE(law): Always reset the Subtraction mask.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): Always reset the Half Carry mask.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Always reset the Carry mask.
   register_f &= ~FLAG_C_MASK;
}

static void
and(unsigned char value)
{
   register_a &= value;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((register_a == 0) << FLAG_Z_BIT);

   // NOTE(law): Always reset the Subtraction mask.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): Always set the Half Carry mask.
   register_f |= FLAG_H_MASK;

   // NOTE(law): Always reset the Carry mask.
   register_f &= ~FLAG_C_MASK;
}

static void
cp(unsigned char value)
{
   // NOTE(law): If the compared values are equivalent, set the Zero flag.
   register_f = (register_f & ~FLAG_Z_MASK) | ((register_a == value) << FLAG_Z_BIT);

   // NOTE(law): Always set the subtraction flag for comparisons.
   register_f |= FLAG_N_MASK;

   // NOTE(law): If the result of subtracting the first 4 bits of value from the
   // first 4 bits of A would produce a result that is less than 0, set the
   // Half Carry flag.
   bool is_half_negative = (signed char)(register_a & 0xF) < (signed char)(value & 0xF);
   register_f = (register_f & ~FLAG_H_MASK) | (is_half_negative << FLAG_H_BIT);

   // NOTE(law): If the result of subtracting value from A would produce a
   // result that is less than 0, set the Carry flag.
   bool is_negative = (signed char)register_a < (signed char)value;
   register_f = (register_f & ~FLAG_C_MASK) | (is_negative << FLAG_C_BIT);
}

static void
inc(unsigned char *value)
{
   // NOTE(law): Increment

   unsigned char half_sum = (*value & 0xF) + 1;

   *value += 1;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((*value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag when a carry from bit 3 occurs.
   register_f = (register_f & ~FLAG_H_MASK) | (((half_sum & 0x10) == 0x10) << FLAG_H_BIT);

   // NOTE(law): The Carry flag is not affected.
}

static void
dec(unsigned char *value)
{
   // NOTE(law): Decrement

   bool is_half_negative = (signed char)(*value & 0xF) < 1;

   *value -= 1;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((*value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always set.
   register_f |= FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag when a carry from bit 3 occurs.
   register_f = (register_f & ~FLAG_H_MASK) | (is_half_negative << FLAG_H_BIT);

   // NOTE(law): The Carry flag is not affected.
}

static void
add16(unsigned short value)
{
   unsigned int extended_sum = (unsigned int)REGISTER_HL + (unsigned int)value;
   unsigned char half_sum = (REGISTER_HL & 0xF) + (value & 0xF);

   unsigned short sum = REGISTER_HL + value;
   register_h = (sum >> 8);
   register_l = (sum & 0xFF);

   // NOTE(law): The Zero flag is not affected.

   // NOTE(law): The Subtraction flag is always unset.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag when a carry from bit 3 occurs.
   register_f = (register_f & ~FLAG_H_MASK) | (((half_sum & 0x10) == 0x10) << FLAG_H_BIT);

   register_f = (register_f & ~FLAG_C_MASK) | ((extended_sum > 0xFFFF) << FLAG_C_BIT);
}

static void
inc16_bytes(unsigned char *high, unsigned char *low)
{
   unsigned short value = ((unsigned short)*high << 8) | ((unsigned short)*low & 0xFF);
   value += 1;

   *high = (value >> 8);
   *low  = (value & 0xFF);


   // NOTE(law): The flags are not affected by 16-bit increments/decrements.
}

static void
dec16_bytes(unsigned char *high, unsigned char *low)
{
   unsigned short value = ((unsigned short)*high << 8) | (unsigned short)*low;
   value -= 1;

   *high = (value >> 8);
   *low  = (value & 0xFF);

   // NOTE(law): The flags are not affected by 16-bit increments/decrements.
}

static void
inc16(unsigned short *value)
{
   *value += 1;

   // NOTE(law): The flags are not affected by 16-bit increments/decrements.
}

static void
dec16(unsigned short *value)
{
   *value -= 1;

   // NOTE(law): The flags are not affected by 16-bit increments/decrements.
}

static void
jp(unsigned char *stream, bool should_jump)
{
   unsigned char address_low  = stream[register_pc++];
   unsigned char address_high = stream[register_pc++];

   if(should_jump)
   {
      unsigned short address = ((unsigned short)address_high << 8) | (unsigned short)address_low;
      register_pc = address;
   }
}

static void
jr(unsigned char *stream, bool should_jump)
{
   signed char offset = stream[register_pc++];

   if(should_jump)
   {
      signed short address = (signed short)register_pc + (signed short)offset;
      register_pc = (unsigned short)address;
   }
}

static void
call(unsigned char *stream, bool should_jump)
{
   unsigned char address_low  = stream[register_pc++];
   unsigned char address_high = stream[register_pc++];

   if(should_jump)
   {
      stream[--register_sp] = (register_pc >> 8);
      stream[--register_sp] = (register_pc & 0xFF);

      unsigned short address = ((unsigned short)address_high << 8) | (unsigned short)address_low;
      register_pc = address;
   }
}

static void
ret(unsigned char *stream, bool should_jump)
{
   unsigned char address_low  = stream[register_sp++];
   unsigned char address_high = stream[register_sp++];

   if(should_jump)
   {
      unsigned short address = ((unsigned short)address_high << 8) | (unsigned short)address_low;
      register_pc = address;
   }
}

static void
rst(unsigned char *stream, unsigned char address_low)
{
   stream[--register_sp] = (register_pc >> 8);
   stream[--register_sp] = (register_pc & 0xFF);

   unsigned short address = (unsigned short)address_low;
   register_pc = address;
}

static void
rl(unsigned char *value)
{
   // NOTE(law): Rotate Left

   unsigned char previous_bit7 = (*value >> 7);
   unsigned char previous_c = FLAG_C;

   *value <<= 1;

   // NOTE(law): Set bit 0 to the pre-shift value of the Carry flag (a value of zero
   // should have already been shifted into position zero).
   *value |= (previous_c << 0);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((*value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 7.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit7 << FLAG_C_BIT);
}

static void
rla()
{
   // NOTE(law): Rotate Left Accumulator

   unsigned char previous_bit7 = (register_a >> 7);
   unsigned char previous_c = FLAG_C;

   register_a <<= 1;

   // NOTE(law): Set bit 0 of value to the previous value of the Carry flag (a
   // value of zero should have already been shifted into position zero).
   register_a |= (previous_c << 0);

   // NOTE(law): The Zero flag is always reset.
   register_f &= ~FLAG_Z_MASK;

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 7.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit7 << FLAG_C_BIT);
}

static void
rlc(unsigned char *value)
{
   // NOTE(law): Rotate Left Circular

   unsigned char previous_bit7 = (*value >> 7);

   *value <<= 1;

   // NOTE(law): Set bit 0 to the pre-shift value of bit 7 (a value of zero
   // should have already been shifted into position zero).
   *value |= (previous_bit7 << 0);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((*value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 7.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit7 << FLAG_C_BIT);
}

static void
rlca()
{
   // NOTE(law): Rotate Left Circular Accumulator

   unsigned char previous_bit7 = (register_a >> 7);

   register_a <<= 1;

   // NOTE(law): Set bit 0 to the pre-shift value of bit 7 (a value of zero
   // should have already been shifted into position zero).
   register_a |= (previous_bit7 << 0);

   // NOTE(law): The Zero flag is always reset.
   register_f &= ~FLAG_Z_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 7.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit7 << FLAG_C_BIT);
}

static void
rr(unsigned char *value)
{
   // NOTE(law): Rotate Right

   unsigned char previous_bit0 = (*value & 0x01);
   unsigned char previous_c = FLAG_C;

   *value >>= 1;

   // NOTE(law): Set bit 7 to the pre-shift value of the Carry flag (a value of zero
   // should have already been shifted into position seven).
   *value |= (previous_c << 7);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((*value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);
}

static void
rra()
{
   // NOTE(law): Rotate Right Accumulator

   unsigned char previous_bit0 = (register_a & 0x01);
   unsigned char previous_c = FLAG_C;

   register_a >>= 1;

   // NOTE(law): Set bit 7 of value to the previous value of the Carry flag (a
   // value of zero should have already been shifted into position seven).
   register_a |= (previous_c << 7);

   // NOTE(law): The Zero flag is not affected.

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);
}

static void
rrc(unsigned char *value)
{
   // NOTE(law): Rotate Right Circular

   unsigned char previous_bit0 = (*value & 0x01);

   *value >>= 1;

   // NOTE(law): Set bit 7 to the pre-shift value of bit 0 (a value of zero
   // should have already been shifted into position seven).
   *value |= (previous_bit0 << 7);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((*value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);
}

static void
rrca()
{
   // NOTE(law): Rotate Right Circular Accumulator

   unsigned char previous_bit0 = (register_a & 0x01);

   register_a >>= 1;

   // NOTE(law): Set bit 7 to the pre-shift value of bit 0 (a value of zero
   // should have already been shifted into position seven).
   register_a |= (previous_bit0 << 7);

   // NOTE(law): The Zero flag is always reset.
   register_f &= ~FLAG_Z_MASK;

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);
}

static void
bit(unsigned int bit_index, unsigned char value)
{
   // NOTE(law) Update the Zero flag based on the value of specified bit index
   // of the value. If the bit is zero, set the flag, else reset it.

   unsigned char bit_value = ((value >> bit_index) & 0x01);
   register_f = (register_f & ~(FLAG_Z_MASK)) | ((bit_value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): The Half Carry flag is always set.
   register_f |= FLAG_H_MASK;

   // NOTE(law): The Carry flag is not affected.
}

static void
set(unsigned int bit_index, unsigned char *value)
{
   *value |= (1 << bit_index);
}

static void
res(unsigned int bit_index, unsigned char *value)
{
   *value &= ~(1 << bit_index);
}

static void
sla(unsigned char *value)
{
   // NOTE(law): Shift Left Arithmetic

   unsigned char previous_bit7 = (*value >> 7);

   *value <<= 1;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((*value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 7.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit7 << FLAG_C_BIT);
}

static void
sra(unsigned char *value)
{
   // NOTE(law): Shift Right Arithmetic

   unsigned char previous_bit0 = (*value & 0x01);

   // TODO(law): Confirm that casting to a signed value actually produces an
   // arithmetic shift in this case.
   *value = ((signed short)*value >> 1);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((*value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);
}

static void
swap(unsigned char *value)
{
   unsigned char high_nibble = (*value >> 4);
   unsigned char low_nibble = (*value & 0xF);

   *value = (low_nibble << 4) | high_nibble;
}

static void
srl(unsigned char *value)
{
   // NOTE(law): Shift Right Logical

   unsigned char previous_bit0 = (*value & 0x01);

   // TODO(law): Confirm that using an unsigned value actually produces a
   // logical shift in this case.
   *value >>= 1;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((*value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);
}

static void
fetch_and_execute(unsigned char *stream)
{
   unsigned char opcode = stream[register_pc++];

   if(opcode == 0xCB)
   {
      // NOTE(law): Parse prefix instructions.
      opcode = stream[register_pc++];
      switch(opcode)
      {
         // NOTE(law): Rotate and Shift instructions
         case 0x00: rlc(&register_b); break;
         case 0x01: rlc(&register_c); break;
         case 0x02: rlc(&register_d); break;
         case 0x03: rlc(&register_e); break;
         case 0x04: rlc(&register_h); break;
         case 0x05: rlc(&register_l); break;
         case 0x06: rlc(&stream[REGISTER_HL]); break;
         case 0x07: rlc(&register_a); break;

         case 0x08: rrc(&register_b); break;
         case 0x09: rrc(&register_c); break;
         case 0x0A: rrc(&register_d); break;
         case 0x0B: rrc(&register_e); break;
         case 0x0C: rrc(&register_h); break;
         case 0x0D: rrc(&register_l); break;
         case 0x0E: rrc(&stream[REGISTER_HL]); break;
         case 0x0F: rrc(&register_a); break;

         case 0x10: rl(&register_b); break;
         case 0x11: rl(&register_c); break;
         case 0x12: rl(&register_d); break;
         case 0x13: rl(&register_e); break;
         case 0x14: rl(&register_h); break;
         case 0x15: rl(&register_l); break;
         case 0x16: rl(&stream[REGISTER_HL]); break;
         case 0x17: rl(&register_a); break;

         case 0x18: rr(&register_b); break;
         case 0x19: rr(&register_c); break;
         case 0x1A: rr(&register_d); break;
         case 0x1B: rr(&register_e); break;
         case 0x1C: rr(&register_h); break;
         case 0x1D: rr(&register_l); break;
         case 0x1E: rr(&stream[REGISTER_HL]); break;
         case 0x1F: rr(&register_a); break;

         case 0x20: sla(&register_b); break;
         case 0x21: sla(&register_c); break;
         case 0x22: sla(&register_d); break;
         case 0x23: sla(&register_e); break;
         case 0x24: sla(&register_h); break;
         case 0x25: sla(&register_l); break;
         case 0x26: sla(&stream[REGISTER_HL]); break;
         case 0x27: sla(&register_a); break;

         case 0x28: sra(&register_b); break;
         case 0x29: sra(&register_c); break;
         case 0x2A: sra(&register_d); break;
         case 0x2B: sra(&register_e); break;
         case 0x2C: sra(&register_h); break;
         case 0x2D: sra(&register_l); break;
         case 0x2E: sra(&stream[REGISTER_HL]); break;
         case 0x2F: sra(&register_a); break;

         case 0x30: swap(&register_b); break;
         case 0x31: swap(&register_c); break;
         case 0x32: swap(&register_d); break;
         case 0x33: swap(&register_e); break;
         case 0x34: swap(&register_h); break;
         case 0x35: swap(&register_l); break;
         case 0x36: swap(&stream[REGISTER_HL]); break;
         case 0x37: swap(&register_a); break;

         case 0x38: srl(&register_b); break;
         case 0x39: srl(&register_c); break;
         case 0x3A: srl(&register_d); break;
         case 0x3B: srl(&register_e); break;
         case 0x3C: srl(&register_h); break;
         case 0x3D: srl(&register_l); break;
         case 0x3E: srl(&stream[REGISTER_HL]); break;
         case 0x3F: srl(&register_a); break;


         // NOTE(law): Single-bit Operation instructions
         case 0x40: bit(0, register_b); break;
         case 0x41: bit(0, register_c); break;
         case 0x42: bit(0, register_d); break;
         case 0x43: bit(0, register_e); break;
         case 0x44: bit(0, register_h); break;
         case 0x45: bit(0, register_l); break;
         case 0x46: bit(0, stream[REGISTER_HL]); break;
         case 0x47: bit(0, register_a); break;

         case 0x48: bit(1, register_b); break;
         case 0x49: bit(1, register_c); break;
         case 0x4A: bit(1, register_d); break;
         case 0x4B: bit(1, register_e); break;
         case 0x4C: bit(1, register_h); break;
         case 0x4D: bit(1, register_l); break;
         case 0x4E: bit(1, stream[REGISTER_HL]); break;
         case 0x4F: bit(1, register_a); break;

         case 0x50: bit(2, register_b); break;
         case 0x51: bit(2, register_c); break;
         case 0x52: bit(2, register_d); break;
         case 0x53: bit(2, register_e); break;
         case 0x54: bit(2, register_h); break;
         case 0x55: bit(2, register_l); break;
         case 0x56: bit(2, stream[REGISTER_HL]); break;
         case 0x57: bit(2, register_a); break;

         case 0x58: bit(3, register_b); break;
         case 0x59: bit(3, register_c); break;
         case 0x5A: bit(3, register_d); break;
         case 0x5B: bit(3, register_e); break;
         case 0x5C: bit(3, register_h); break;
         case 0x5D: bit(3, register_l); break;
         case 0x5E: bit(3, stream[REGISTER_HL]); break;
         case 0x5F: bit(3, register_a); break;

         case 0x60: bit(4, register_b); break;
         case 0x61: bit(4, register_c); break;
         case 0x62: bit(4, register_d); break;
         case 0x63: bit(4, register_e); break;
         case 0x64: bit(4, register_h); break;
         case 0x65: bit(4, register_l); break;
         case 0x66: bit(4, stream[REGISTER_HL]); break;
         case 0x67: bit(4, register_a); break;

         case 0x68: bit(5, register_b); break;
         case 0x69: bit(5, register_c); break;
         case 0x6A: bit(5, register_d); break;
         case 0x6B: bit(5, register_e); break;
         case 0x6C: bit(5, register_h); break;
         case 0x6D: bit(5, register_l); break;
         case 0x6E: bit(5, stream[REGISTER_HL]); break;
         case 0x6F: bit(5, register_a); break;

         case 0x70: bit(6, register_b); break;
         case 0x71: bit(6, register_c); break;
         case 0x72: bit(6, register_d); break;
         case 0x73: bit(6, register_e); break;
         case 0x74: bit(6, register_h); break;
         case 0x75: bit(6, register_l); break;
         case 0x76: bit(6, stream[REGISTER_HL]); break;
         case 0x77: bit(6, register_a); break;

         case 0x78: bit(7, register_b); break;
         case 0x79: bit(7, register_c); break;
         case 0x7A: bit(7, register_d); break;
         case 0x7B: bit(7, register_e); break;
         case 0x7C: bit(7, register_h); break;
         case 0x7D: bit(7, register_l); break;
         case 0x7E: bit(7, stream[REGISTER_HL]); break;
         case 0x7F: bit(7, register_a); break;

         case 0x80: res(0, &register_b); break;
         case 0x81: res(0, &register_c); break;
         case 0x82: res(0, &register_d); break;
         case 0x83: res(0, &register_e); break;
         case 0x84: res(0, &register_h); break;
         case 0x85: res(0, &register_l); break;
         case 0x86: res(0, &stream[REGISTER_HL]); break;
         case 0x87: res(0, &register_a); break;

         case 0x88: res(1, &register_b); break;
         case 0x89: res(1, &register_c); break;
         case 0x8A: res(1, &register_d); break;
         case 0x8B: res(1, &register_e); break;
         case 0x8C: res(1, &register_h); break;
         case 0x8D: res(1, &register_l); break;
         case 0x8E: res(1, &stream[REGISTER_HL]); break;
         case 0x8F: res(1, &register_a); break;

         case 0x90: res(2, &register_b); break;
         case 0x91: res(2, &register_c); break;
         case 0x92: res(2, &register_d); break;
         case 0x93: res(2, &register_e); break;
         case 0x94: res(2, &register_h); break;
         case 0x95: res(2, &register_l); break;
         case 0x96: res(2, &stream[REGISTER_HL]); break;
         case 0x97: res(2, &register_a); break;

         case 0x98: res(3, &register_b); break;
         case 0x99: res(3, &register_c); break;
         case 0x9A: res(3, &register_d); break;
         case 0x9B: res(3, &register_e); break;
         case 0x9C: res(3, &register_h); break;
         case 0x9D: res(3, &register_l); break;
         case 0x9E: res(3, &stream[REGISTER_HL]); break;
         case 0x9F: res(3, &register_a); break;

         case 0xA0: res(4, &register_b); break;
         case 0xA1: res(4, &register_c); break;
         case 0xA2: res(4, &register_d); break;
         case 0xA3: res(4, &register_e); break;
         case 0xA4: res(4, &register_h); break;
         case 0xA5: res(4, &register_l); break;
         case 0xA6: res(4, &stream[REGISTER_HL]); break;
         case 0xA7: res(4, &register_a); break;

         case 0xA8: res(5, &register_b); break;
         case 0xA9: res(5, &register_c); break;
         case 0xAA: res(5, &register_d); break;
         case 0xAB: res(5, &register_e); break;
         case 0xAC: res(5, &register_h); break;
         case 0xAD: res(5, &register_l); break;
         case 0xAE: res(5, &stream[REGISTER_HL]); break;
         case 0xAF: res(5, &register_a); break;

         case 0xB0: res(6, &register_b); break;
         case 0xB1: res(6, &register_c); break;
         case 0xB2: res(6, &register_d); break;
         case 0xB3: res(6, &register_e); break;
         case 0xB4: res(6, &register_h); break;
         case 0xB5: res(6, &register_l); break;
         case 0xB6: res(6, &stream[REGISTER_HL]); break;
         case 0xB7: res(6, &register_a); break;

         case 0xB8: res(7, &register_b); break;
         case 0xB9: res(7, &register_c); break;
         case 0xBA: res(7, &register_d); break;
         case 0xBB: res(7, &register_e); break;
         case 0xBC: res(7, &register_h); break;
         case 0xBD: res(7, &register_l); break;
         case 0xBE: res(7, &stream[REGISTER_HL]); break;
         case 0xBF: res(7, &register_a); break;

         case 0xC0: set(0, &register_b); break;
         case 0xC1: set(0, &register_c); break;
         case 0xC2: set(0, &register_d); break;
         case 0xC3: set(0, &register_e); break;
         case 0xC4: set(0, &register_h); break;
         case 0xC5: set(0, &register_l); break;
         case 0xC6: set(0, &stream[REGISTER_HL]); break;
         case 0xC7: set(0, &register_a); break;

         case 0xC8: set(1, &register_b); break;
         case 0xC9: set(1, &register_c); break;
         case 0xCA: set(1, &register_d); break;
         case 0xCB: set(1, &register_e); break;
         case 0xCC: set(1, &register_h); break;
         case 0xCD: set(1, &register_l); break;
         case 0xCE: set(1, &stream[REGISTER_HL]); break;
         case 0xCF: set(1, &register_a); break;

         case 0xD0: set(2, &register_b); break;
         case 0xD1: set(2, &register_c); break;
         case 0xD2: set(2, &register_d); break;
         case 0xD3: set(2, &register_e); break;
         case 0xD4: set(2, &register_h); break;
         case 0xD5: set(2, &register_l); break;
         case 0xD6: set(2, &stream[REGISTER_HL]); break;
         case 0xD7: set(2, &register_a); break;

         case 0xD8: set(3, &register_b); break;
         case 0xD9: set(3, &register_c); break;
         case 0xDA: set(3, &register_d); break;
         case 0xDB: set(3, &register_e); break;
         case 0xDC: set(3, &register_h); break;
         case 0xDD: set(3, &register_l); break;
         case 0xDE: set(3, &stream[REGISTER_HL]); break;
         case 0xDF: set(3, &register_a); break;

         case 0xE0: set(4, &register_b); break;
         case 0xE1: set(4, &register_c); break;
         case 0xE2: set(4, &register_d); break;
         case 0xE3: set(4, &register_e); break;
         case 0xE4: set(4, &register_h); break;
         case 0xE5: set(4, &register_l); break;
         case 0xE6: set(4, &stream[REGISTER_HL]); break;
         case 0xE7: set(4, &register_a); break;

         case 0xE8: set(5, &register_b); break;
         case 0xE9: set(5, &register_c); break;
         case 0xEA: set(5, &register_d); break;
         case 0xEB: set(5, &register_e); break;
         case 0xEC: set(5, &register_h); break;
         case 0xED: set(5, &register_l); break;
         case 0xEE: set(5, &stream[REGISTER_HL]); break;
         case 0xEF: set(5, &register_a); break;

         case 0xF0: set(6, &register_b); break;
         case 0xF1: set(6, &register_c); break;
         case 0xF2: set(6, &register_d); break;
         case 0xF3: set(6, &register_e); break;
         case 0xF4: set(6, &register_h); break;
         case 0xF5: set(6, &register_l); break;
         case 0xF6: set(6, &stream[REGISTER_HL]); break;
         case 0xF7: set(6, &register_a); break;

         case 0xF8: set(7, &register_b); break;
         case 0xF9: set(7, &register_c); break;
         case 0xFA: set(7, &register_d); break;
         case 0xFB: set(7, &register_e); break;
         case 0xFC: set(7, &register_h); break;
         case 0xFD: set(7, &register_l); break;
         case 0xFE: set(7, &stream[REGISTER_HL]); break;
         case 0xFF: set(7, &register_a); break;

         default:
         {
            platform_log("UNHANDLED OPCODE CB %x\n", opcode);
            assert(0);
         } break;
      }
   }
   else
   {
      switch(opcode)
      {
         // NOTE(law): 8-bit load instructions
         case 0x40: {/*register_b = register_b;*/} break;
         case 0x41: {register_b = register_c;} break;
         case 0x42: {register_b = register_d;} break;
         case 0x43: {register_b = register_e;} break;
         case 0x44: {register_b = register_h;} break;
         case 0x45: {register_b = register_l;} break;
         case 0x46: {register_b = stream[REGISTER_HL];} break;
         case 0x47: {register_b = register_a;} break;

         case 0x48: {register_c = register_b;} break;
         case 0x49: {/*register_c = register_c;*/} break;
         case 0x4A: {register_c = register_d;} break;
         case 0x4B: {register_c = register_e;} break;
         case 0x4C: {register_c = register_h;} break;
         case 0x4D: {register_c = register_l;} break;
         case 0x4E: {register_c = stream[REGISTER_HL];} break;
         case 0x4F: {register_c = register_a;} break;

         case 0x50: {register_d = register_b;} break;
         case 0x51: {register_d = register_c;} break;
         case 0x52: {/*register_d = register_d;*/} break;
         case 0x53: {register_d = register_e;} break;
         case 0x54: {register_d = register_h;} break;
         case 0x55: {register_d = register_l;} break;
         case 0x56: {register_d = stream[REGISTER_HL];} break;
         case 0x57: {register_d = register_a;} break;

         case 0x58: {register_e = register_b;} break;
         case 0x59: {register_e = register_c;} break;
         case 0x5A: {register_e = register_d;} break;
         case 0x5B: {/*register_e = register_e;*/} break;
         case 0x5C: {register_e = register_h;} break;
         case 0x5D: {register_e = register_l;} break;
         case 0x5E: {register_e = stream[REGISTER_HL];} break;
         case 0x5F: {register_e = register_a;} break;

         case 0x60: {register_h = register_b;} break;
         case 0x61: {register_h = register_c;} break;
         case 0x62: {register_h = register_d;} break;
         case 0x63: {register_h = register_e;} break;
         case 0x64: {/*register_h = register_h;*/} break;
         case 0x65: {register_h = register_l;} break;
         case 0x66: {register_h = stream[REGISTER_HL];} break;
         case 0x67: {register_h = register_a;} break;

         case 0x68: {register_l = register_b;} break;
         case 0x69: {register_l = register_c;} break;
         case 0x6A: {register_l = register_d;} break;
         case 0x6B: {register_l = register_e;} break;
         case 0x6C: {register_l = register_h;} break;
         case 0x6D: {/*register_l = register_l;*/} break;
         case 0x6E: {register_l = stream[REGISTER_HL];} break;
         case 0x6F: {register_l = register_a;} break;

         case 0x70: {stream[REGISTER_HL] = register_b;} break;
         case 0x71: {stream[REGISTER_HL] = register_c;} break;
         case 0x72: {stream[REGISTER_HL] = register_d;} break;
         case 0x73: {stream[REGISTER_HL] = register_e;} break;
         case 0x74: {stream[REGISTER_HL] = register_h;} break;
         case 0x75: {stream[REGISTER_HL] = register_l;} break;
         case 0x77: {stream[REGISTER_HL] = register_a;} break;

         case 0x78: {register_a = register_b;} break;
         case 0x79: {register_a = register_c;} break;
         case 0x7A: {register_a = register_d;} break;
         case 0x7B: {register_a = register_e;} break;
         case 0x7C: {register_a = register_h;} break;
         case 0x7D: {register_a = register_l;} break;
         case 0x7E: {register_a = stream[REGISTER_HL];} break;
         case 0x7F: {/*register_a = register_a;*/} break;

         case 0x06: {register_b = stream[register_pc++];} break; // LD B, n
         case 0x0E: {register_c = stream[register_pc++];} break; // LD C, n
         case 0x1E: {register_e = stream[register_pc++];} break; // LD E, n
         case 0x16: {register_d = stream[register_pc++];} break; // LD D, n
         case 0x26: {register_h = stream[register_pc++];} break; // LD H, n
         case 0x2E: {register_l = stream[register_pc++];} break; // LD L, n
         case 0x36: {stream[REGISTER_HL] = stream[register_pc++];} break; // LD (HL), n
         case 0x3E: {register_a = stream[register_pc++];} break; // LD A, n

         case 0x0A: {register_a = stream[REGISTER_BC];} break; // LD A, (BC)
         case 0x1A: {register_a = stream[REGISTER_DE];} break; // LD A, (DE)

         case 0xFA: // LD A, (nn)
         {
            unsigned char address_low  = stream[register_pc++];
            unsigned char address_high = stream[register_pc++];
            unsigned address = ((unsigned short)address_high << 8) | ((unsigned short)address_low);

            register_a = stream[address];
         } break;

         case 0x02: {stream[REGISTER_BC] = register_a;} break; // LD (BC), A
         case 0x12: {stream[REGISTER_DE] = register_a;} break; // LD (DE), A

         case 0xEA: // LD (nn), A
         {
            unsigned char address_low  = stream[register_pc++];
            unsigned char address_high = stream[register_pc++];
            unsigned address = ((unsigned short)address_high << 8) | ((unsigned short)address_low);

            stream[address] = register_a;
         } break;

         case 0xF2: {register_a = stream[0xFF00 + register_c];} break; // LDH A, (0xFF00 + C)
         case 0xE2: {stream[0xFF00 + register_c] = register_a;} break; // LDH (0xFF00 + C), A

         case 0xF0: {register_a = stream[0xFF00 + stream[register_pc++]];} break; // LDH A, (0xFF00 + n)
         case 0xE0: {stream[0xFF00 + stream[register_pc++]] = register_a;} break; // LDH (0xFF00 + n), A

         case 0x22: // LDI (HL), A
         {
            stream[REGISTER_HL] = register_a;
            unsigned short updated_value = REGISTER_HL + 1;

            register_h = updated_value >> 8;
            register_l = updated_value & 0xFF;
         } break;

         case 0x32: // LDD (HL), A
         {
            stream[REGISTER_HL] = register_a;
            unsigned short updated_value = REGISTER_HL - 1;

            register_h = updated_value >> 8;
            register_l = updated_value & 0xFF;
         } break;

         case 0x2A: // LDI A, (HL)
         {
            register_a = stream[REGISTER_HL];
            unsigned short updated_value = REGISTER_HL + 1;

            register_h = updated_value >> 8;
            register_l = updated_value & 0xFF;
         } break;

         case 0x3A: // LDD A, (HL)
         {
            register_a = stream[REGISTER_HL];
            unsigned short updated_value = REGISTER_HL - 1;

            register_h = updated_value >> 8;
            register_l = updated_value & 0xFF;
         } break;

         case 0xF9: {register_sp = REGISTER_HL;} break; // LD SP, HL

         case 0xC5: // PUSH BC
         {
            stream[--register_sp] = register_b;
            stream[--register_sp] = register_c;
         } break;

         case 0xD5: // PUSH DE
         {
            stream[--register_sp] = register_d;
            stream[--register_sp] = register_e;
         } break;

         case 0xE5: // PUSH HL
         {
            stream[--register_sp] = register_h;
            stream[--register_sp] = register_l;
         } break;

         case 0xF5: // PUSH AP
         {
            stream[--register_sp] = register_a;
            stream[--register_sp] = register_f;
         } break;

         case 0xC1: // POP BC
         {
            register_c = stream[register_sp++];
            register_b = stream[register_sp++];
         } break;

         case 0xD1: // POP DE
         {
            register_e = stream[register_sp++];
            register_d = stream[register_sp++];
         } break;

         case 0xE1: //POP HL
         {
            register_l = stream[register_sp++];
            register_h = stream[register_sp++];
         } break;

         case 0xF1: // POP AF
         {
            register_f = stream[register_sp++];
            register_a = stream[register_sp++];
         } break;


         // NOTE(law): 16-bit load instructions
         case 0x08: // LD (nn), SP
         {
            unsigned char address_low  = stream[register_pc++];
            unsigned char address_high = stream[register_pc++];
            unsigned short address = ((unsigned short)address_high << 8) | ((unsigned short)address_low);

            stream[address] = register_sp;
         } break;

         case 0x01: // LD BC, nn
         {
            register_c = stream[register_pc++];
            register_b = stream[register_pc++];
         } break;

         case 0x11: // LD DE, nn
         {
            register_e = stream[register_pc++];
            register_d = stream[register_pc++];
         } break;

         case 0x21: // LD HL, nn
         {
            register_l = stream[register_pc++];
            register_h = stream[register_pc++];
         } break;

         case 0x31: // LD SP, nn
         {
            unsigned char value_low  = stream[register_pc++];
            unsigned char value_high = stream[register_pc++];
            unsigned short value = ((unsigned short)value_high << 8) | ((unsigned short)value_low);

            register_sp = value;
         } break;


         // NOTE(law): Rotate and Shift instructions
         case 0x07: rlca(); break;
         case 0x17: rla(); break;
         case 0x0F: rrca(); break;
         case 0x1F: rra(); break;


         // NOTE(law): 8-bit Arithmetic/Logic instructions
         case 0x80: add(register_b); break; // ADD A, B
         case 0x81: add(register_c); break; // ADD A, C
         case 0x82: add(register_d); break; // ADD A, D
         case 0x83: add(register_e); break; // ADD A, E
         case 0x84: add(register_h); break; // ADD A, H
         case 0x85: add(register_l); break; // ADD A, L
         case 0x86: add(stream[REGISTER_HL]); break; // ADD A, (HL)
         case 0x87: add(register_a); break; // ADD A, A

         case 0xC6: add(stream[register_pc++]); break; // ADD A, n

         case 0x88: adc(register_b); break; // ADC A, B
         case 0x89: adc(register_c); break; // ADC A, C
         case 0x8A: adc(register_d); break; // ADC A, D
         case 0x8B: adc(register_e); break; // ADC A, E
         case 0x8C: adc(register_h); break; // ADC A, H
         case 0x8D: adc(register_l); break; // ADC A, L
         case 0x8E: adc(stream[REGISTER_HL]); break; // ADC A, (HL)
         case 0x8F: adc(register_a); break; // ADC A, A

         case 0xCE: {adc(stream[register_pc++]);} break; // ADC A, n

         case 0x90: sub(register_b); break; // SUB A, B
         case 0x91: sub(register_c); break; // SUB A, C
         case 0x92: sub(register_d); break; // SUB A, D
         case 0x93: sub(register_e); break; // SUB A, E
         case 0x94: sub(register_h); break; // SUB A, H
         case 0x95: sub(register_l); break; // SUB A, L
         case 0x96: sub(stream[REGISTER_HL]); break; // SUB A, (HL)
         case 0x97: sub(register_a); break; // SUB A, A

         case 0xD6: sub(stream[register_pc++]); break; // SUB A, n

         case 0x98: sbc(register_b); break; // SBC A, B
         case 0x99: sbc(register_c); break; // SBC A, C
         case 0x9A: sbc(register_d); break; // SBC A, D
         case 0x9B: sbc(register_e); break; // SBC A, E
         case 0x9C: sbc(register_h); break; // SBC A, H
         case 0x9D: sbc(register_l); break; // SBC A, L
         case 0x9E: sbc(stream[REGISTER_HL]); break; // SBC A, (HL)
         case 0x9F: sbc(register_a); break; // SBC A, A

         case 0xDE: sbc(stream[register_pc++]); break; // SBC A, n

         case 0x27: // DAA
         {
            // TODO(law): Look up how decimal adjustment actually works and
            // implement it!

            assert(!"DAA");
         } break;

         case 0x2F: // CPL
         {
            register_a = ~register_a;
            register_f |= FLAG_N_MASK;
            register_f |= FLAG_H_MASK;
         } break;

         case 0xA8: xor(register_b); break; // XOR A, B
         case 0xA9: xor(register_c); break; // XOR A, C
         case 0xAA: xor(register_d); break; // XOR A, D
         case 0xAB: xor(register_e); break; // XOR A, E
         case 0xAC: xor(register_h); break; // XOR A, H
         case 0xAD: xor(register_l); break; // XOR A, L
         case 0xAE: xor(stream[REGISTER_HL]); break; // XOR A, (HL)
         case 0xAF: xor(register_a); break; // XOR A, A

         case 0xEE: xor(stream[register_pc++]); break; // XOR A, n

         case 0xB0: or(register_b); break; // OR A, B
         case 0xB1: or(register_c); break; // OR A, C
         case 0xB2: or(register_d); break; // OR A, D
         case 0xB3: or(register_e); break; // OR A, E
         case 0xB4: or(register_h); break; // OR A, H
         case 0xB5: or(register_l); break; // OR A, L
         case 0xB6: or(stream[REGISTER_HL]); break; // OR A, (HL)
         case 0xB7: or(register_a); break; // OR A, A

         case 0xF6: or(stream[register_pc++]); break; // OR A, n

         case 0xA0: and(register_b); break; // AND A, B
         case 0xA1: and(register_c); break; // AND A, C
         case 0xA2: and(register_d); break; // AND A, D
         case 0xA3: and(register_e); break; // AND A, E
         case 0xA4: and(register_h); break; // AND A, H
         case 0xA5: and(register_l); break; // AND A, L
         case 0xA6: and(stream[REGISTER_HL]); break; // AND A, (HL)
         case 0xA7: and(register_a); break; // AND A, A

         case 0xE6: and(stream[register_pc++]); break; // AND A, n

         case 0xB8: cp(register_b); break; // CP A, B
         case 0xB9: cp(register_c); break; // CP A, C
         case 0xBA: cp(register_d); break; // CP A, D
         case 0xBB: cp(register_e); break; // CP A, E
         case 0xBC: cp(register_h); break; // CP A, H
         case 0xBD: cp(register_l); break; // CP A, L
         case 0xBE: cp(stream[REGISTER_HL]); break; // CP A, (HL)
         case 0xBF: cp(register_a); break; // CP A, A

         case 0xFE: cp(stream[register_pc++]); break; // CP A, n

         case 0x04: inc(&register_b); break; // INC B
         case 0x0C: inc(&register_c); break; // INC C
         case 0x14: inc(&register_d); break; // INC D
         case 0x1C: inc(&register_e); break; // INC E
         case 0x24: inc(&register_h); break; // INC H
         case 0x2C: inc(&register_l); break; // INC L
         case 0x34: inc(&stream[REGISTER_HL]); break; // INC (HL)
         case 0x3C: inc(&register_a); break; // INC A

         case 0x05: dec(&register_b); break; // DEC B
         case 0x0D: dec(&register_c); break; // DEC C
         case 0x15: dec(&register_d); break; // DEC D
         case 0x1D: dec(&register_e); break; // DEC E
         case 0x25: dec(&register_h); break; // DEC H
         case 0x2D: dec(&register_l); break; // DEC L
         case 0x35: dec(&stream[REGISTER_HL]); break; // DEC (HL)
         case 0x3D: dec(&register_a); break; // DEC A


         // NOTE(law): 16-bit Arithmetic/Logic instructions
         case 0x09: add16(REGISTER_BC); break; // ADD HL, BC
         case 0x19: add16(REGISTER_DE); break; // ADD HL, DE
         case 0x29: add16(REGISTER_HL); break; // ADD HL, HL
         case 0x39: add16(register_sp); break; // ADD HL, SP

         case 0x03: inc16_bytes(&register_b, &register_c); break; // INC BC
         case 0x13: inc16_bytes(&register_d, &register_e); break; // INC DE
         case 0x23: inc16_bytes(&register_h, &register_l); break; // INC HL
         case 0x33: inc16(&register_sp); break; // INC SP

         case 0x0B: dec16_bytes(&register_b, &register_c); break; // DEC BC
         case 0x1B: dec16_bytes(&register_d, &register_e); break; // DEC DE
         case 0x2B: dec16_bytes(&register_h, &register_l); break; // DEC HL
         case 0x3B: dec16(&register_sp); break; // DEC SP

         case 0xE8: // ADD SP, dd
         {
            signed char offset = stream[register_pc++];

            signed int extended_address = (signed int)register_sp + (signed int)offset;
            unsigned char half_sum = (register_sp & 0xF) + (offset & 0xF);

            register_sp = (unsigned short)extended_address;

            // NOTE(law): The Zero flag is always unset.
            register_f &= ~FLAG_Z_MASK;

            // NOTE(law): The Subtraction flag is always unset.
            register_f &= ~FLAG_N_MASK;

            // NOTE(law): Set the Half Carry flag when a carry from bit 3 occurs.
            register_f = (register_f & ~FLAG_H_MASK) | (((half_sum & 0x10) == 0x10) << FLAG_H_BIT);

            // NOTE(law): Set the Carry flag when a carry from bit 7 occurs.
            register_f = (register_f & ~FLAG_C_MASK) | ((extended_address > 0xFFFF) << FLAG_C_BIT);
         } break;

         case 0xF8: // LD HL, SP + dd
         {
            signed char offset = stream[register_pc++];

            signed int extended_address = (signed int)register_sp + (signed int)offset;
            unsigned char half_sum = (register_sp & 0xF) + (offset & 0xF);

            register_h = (unsigned char)((unsigned short)extended_address >> 8);
            register_l = (unsigned char)((unsigned short)extended_address & 0xFF);

            // NOTE(law): The Zero flag is always unset.
            register_f &= ~FLAG_Z_MASK;

            // NOTE(law): The Subtraction flag is always unset.
            register_f &= ~FLAG_N_MASK;

            // NOTE(law): Set the Half Carry flag when a carry from bit 3 occurs.
            register_f = (register_f & ~FLAG_H_MASK) | (((half_sum & 0x10) == 0x10) << FLAG_H_BIT);

            // NOTE(law): Set the Carry flag when a carry from bit 7 occurs.
            register_f = (register_f & ~FLAG_C_MASK) | ((extended_address > 0xFFFF) << FLAG_C_BIT);
         } break;


         // NOTE(law): CPU Control instructions
         case 0x3F: // CCF
         {
            register_f &= ~FLAG_N_MASK;
            register_f &= ~FLAG_H_MASK;

            unsigned char flipped_c = !FLAG_C;
            register_f = (register_f & ~FLAG_C_MASK) | (flipped_c << FLAG_C_BIT);
         } break;

         case 0x37: // SCF
         {
            register_f &= ~FLAG_N_MASK;
            register_f &= ~FLAG_H_MASK;
            register_f |= FLAG_C_MASK;
         } break;

         case 0x00: {} break; // NOP
         case 0x76: {halt = true;} break; // HALT
         case 0x10: {stop = true; register_pc++;} break; // STOP
         case 0xF3: {ime = false;} break; // DI
         case 0xFB: {ime = true;} break; // EI


         // NOTE(law): Jump instructions
         case 0xC3: jp(stream, true); break; // JP nn
         case 0xE9: {register_pc = REGISTER_HL;} break; // JP HL

         case 0xC2: jp(stream, !FLAG_Z); break; // JP NZ, nn
         case 0xCA: jp(stream, FLAG_Z); break;  // JP Z, nn
         case 0xD2: jp(stream, !FLAG_C); break; // JP NC, nn
         case 0xDA: jp(stream, FLAG_C); break;  // JP C, nn

         case 0x18: jr(stream, true); break;    // JR PC + n
         case 0x20: jr(stream, !FLAG_Z); break; // JR NZ, PC + n
         case 0x28: jr(stream, FLAG_Z); break;  // JR Z, PC + n
         case 0x30: jr(stream, !FLAG_C); break; // JR NC, PC + n
         case 0x38: jr(stream, FLAG_C); break;  // JR C, PC + n

         case 0xC4: call(stream, !FLAG_Z); break; // CALL NZ, nn
         case 0xCC: call(stream, FLAG_Z); break;  // CALL Z, nn
         case 0xCD: call(stream, true); break;    // CALL nn
         case 0xD4: call(stream, !FLAG_C); break; // CALL NC, nn
         case 0xDC: call(stream, FLAG_C); break;  // CALL C, nn

         case 0xC0: ret(stream, !FLAG_Z); break; // RET NZ
         case 0xC8: ret(stream, FLAG_Z); break;  // RET Z
         case 0xC9: ret(stream, true); break;    // RET
         case 0xD0: ret(stream, !FLAG_C); break; // RET NC
         case 0xD8: ret(stream, FLAG_C); break;  // RET C

         case 0xD9: {ret(stream, true); ime = true;} break; // RETI

         case 0xC7: rst(stream, 0x00); break;
         case 0xCF: rst(stream, 0x08); break;
         case 0xD7: rst(stream, 0x10); break;
         case 0xDF: rst(stream, 0x18); break;
         case 0xE7: rst(stream, 0x20); break;
         case 0xEF: rst(stream, 0x28); break;
         case 0xF7: rst(stream, 0x30); break;
         case 0xFF: rst(stream, 0x38); break;

         case 0xD3: {assert(!"ILLEGAL_D3");} break;
         case 0xDB: {assert(!"ILLEGAL_DB");} break;
         case 0xDD: {assert(!"ILLEGAL_DD");} break;
         case 0xE3: {assert(!"ILLEGAL_E3");} break;
         case 0xE4: {assert(!"ILLEGAL_E4");} break;
         case 0xEB: {assert(!"ILLEGAL_EB");} break;
         case 0xEC: {assert(!"ILLEGAL_EC");} break;
         case 0xED: {assert(!"ILLEGAL_ED");} break;
         case 0xF4: {assert(!"ILLEGAL_F4");} break;
         case 0xFC: {assert(!"ILLEGAL_FC");} break;
         case 0xFD: {assert(!"ILLEGAL_FD");} break;

         default:
         {
            platform_log("UNHANDLED OPCODE %X\n", opcode);
            assert(0);
         } break;
      }
   }
}

static void
handle_interrupts(unsigned char *stream)
{
   if(ime && (REGISTER_IE & REGISTER_IF))
   {
      // NOTE(law): Disable interrupts for the duration of the handler.
      ime = false;

      // NOTE(law): The priority of interrupts are ordered by increasing bit
      // index in register_if (i.e. VBlank with bit index 0 has the highest
      // priority).
      unsigned int bit_index = 0;
      for(; bit_index <= 4; ++bit_index)
      {
         if((REGISTER_IF >> bit_index) & 0x1)
         {
            break;
         }
      }
      assert(bit_index <= 4);

      // NOTE(law): Reset the bit of the interrupt we plan to handle.
      REGISTER_IF &= ~(1 << bit_index);

      // TODO(law): Wait for two cycles using NOPs.

      stream[--register_sp] = (register_pc >> 8);
      stream[--register_sp] = (register_pc & 0xFF);

      unsigned short isr_addresses[] = {0x40, 0x48, 0x50, 0x58, 0x60};
      register_pc = isr_addresses[bit_index];
   }
}

static void
clear(Platform_Bitmap *bitmap)
{
   for(unsigned int y = 0; y < bitmap->height; ++y)
   {
      for(unsigned int x = 0; x < bitmap->width; ++x)
      {
         bitmap->memory[(bitmap->width * y) + x] = palette[0];
      }
   }
}

static void
render_tiles(Platform_Bitmap *bitmap, unsigned char *stream, int tile_offset)
{
   unsigned char *tiles = stream + tile_offset;
   for(unsigned int tile_y = 0; tile_y < TILES_PER_SCREEN_HEIGHT; ++tile_y)
   {
      for(unsigned int tile_x = 0; tile_x < TILES_PER_SCREEN_WIDTH; ++tile_x)
      {
         unsigned int tile_index = (TILES_PER_SCREEN_WIDTH * tile_y) + tile_x;
         unsigned char *tile = tiles + (2 * tile_index * TILE_PIXEL_DIM);

         for(unsigned int pixel_y = 0; pixel_y < TILE_PIXEL_DIM; ++pixel_y)
         {
            unsigned char byte0 = tile[(2 * pixel_y) + 0];
            unsigned char byte1 = tile[(2 * pixel_y) + 1];

            for(unsigned int pixel_x = 0; pixel_x < TILE_PIXEL_DIM; ++pixel_x)
            {
               unsigned int bit_offset = TILE_PIXEL_DIM - 1 - pixel_x;
               unsigned char low_bit  = (byte0 >> bit_offset) & 0x1;
               unsigned char high_bit = (byte1 >> bit_offset) & 0x1;

               unsigned int color_index = (high_bit << 1) | low_bit;

               unsigned int bitmap_y = (tile_y * TILE_PIXEL_DIM) + pixel_y;
               unsigned int bitmap_x = (tile_x * TILE_PIXEL_DIM) + pixel_x;
               bitmap->memory[(bitmap->width * bitmap_y) + bitmap_x] = palette[color_index];
            }
         }
      }
   }
}
