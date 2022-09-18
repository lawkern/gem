/* /////////////////////////////////////////////////////////////////////////// */
/* (c) copyright 2022 Lawrence D. Kern /////////////////////////////////////// */
/* /////////////////////////////////////////////////////////////////////////// */

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct
{
   size_t size;
   unsigned char *memory;
} Platform_File;

#define PLATFORM_FREE_FILE(name) void name(Platform_File file)
#define PLATFORM_LOAD_FILE(name) Platform_File name(char *file_path)

static PLATFORM_FREE_FILE(free_file);
static PLATFORM_LOAD_FILE(load_file);

#define ARRAY_LENGTH(array) (sizeof(array) / sizeof((array)[0]))

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
   printf("Verifying logo...\n");

   assert(ARRAY_LENGTH(logo) == ARRAY_LENGTH(header->logo));
   for(unsigned int byte_index = 0; byte_index < ARRAY_LENGTH(logo); ++byte_index)
   {
      // TODO(law): CGB and later models noly check the top half of the logo,
      // i.e. th first 0x18 bytes.

      if(logo[byte_index] != header->logo[byte_index])
      {
         fprintf(stderr, "ERROR: Logo mismatch at byte index %d ", byte_index);
         fprintf(stderr, "(header: 0x%02x, expected: 0x%02x)\n", header->logo[byte_index], logo[byte_index]);

         return(false);
      }
   }

   printf("Verifying checksums...\n");

   unsigned char header_checksum = 0;
   for(unsigned short byte_offset = 0x0134; byte_offset <= 0x014C; ++byte_offset)
   {
      header_checksum = header_checksum - rom_memory[byte_offset] - 1;
   }

   if(header_checksum != header->header_checksum)
   {
      fprintf(stderr, "ERROR: Computed header checksum did not match value in header. ");
      fprintf(stderr, "(header: 0x%02x, computed: 0x%02x.\n", header_checksum, header->header_checksum);

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
      fprintf(stderr, "WARNING: Computed global checksum did not match value in header. ");
      fprintf(stderr, "(header: 0x%04x, computed: %0x04x)\n", header->global_checksum, global_checksum);
   }

   return(true);
}

static
void dump_cartridge_header(Cartridge_Header *header)
{
   printf("CARTRIDGE HEADER:\n");

   printf("  ENTRY POINT: 0x%08x\n", header->entry_point);

   printf("  TITLE: %s\n", header->title);
   printf("  SGB FLAG: 0x%02x\n", header->sgb_flag);

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
   printf("  CARTRIDGE TYPE: %#x (%s)\n", header->cartridge_type, cartridge_types[header->cartridge_type]);
   printf("  ROM SIZE: %u KiB\n", 32 * (1 << header->rom_size));

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
   printf("  RAM SIZE: %s\n", sram_sizes[header->ram_size]);

   static char *destination_codes[] =
   {
      "Japan (and possibly overseas)",
      "Overseas only",
   };

   assert(header->destination_code < ARRAY_LENGTH(destination_codes));
   printf("  DESTINATION CODE: %#x (%s)\n", header->destination_code, destination_codes[header->destination_code]);

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
   printf("  HEADER CHECKSUM: 0x%02x\n", header->header_checksum);
   printf("  GLOBAL CHECKSUM: 0x%04x\n", header->global_checksum);
}

static void
disassemble_stream(unsigned char *stream, unsigned int offset, unsigned int byte_count)
{
   // TODO(law): 16-bit operations are not being endian swapped in this
   // function.

   unsigned int start = offset;

   while((offset - start) < byte_count)
   {
      // NOTE(law): Print the address of the current instruction.
      printf("  0x%06x  ", offset);

      unsigned char opcode = stream[offset++];
      if(opcode == 0xCB)
      {
         // NOTE(law): Parse prefix instructions.
         printf("(%02x ", opcode);

         opcode = stream[offset++];
         printf("%02x) ", opcode);

         switch(opcode)
         {
            // NOTE(law): Rotate and Shift instructions
            case 0x00: {printf("RLC\tB");} break;
            case 0x01: {printf("RLC\tC");} break;
            case 0x02: {printf("RLC\tD");} break;
            case 0x03: {printf("RLC\tE");} break;
            case 0x04: {printf("RLC\tH");} break;
            case 0x05: {printf("RLC\tL");} break;
            case 0x06: {printf("RLC\t(HL)");} break;
            case 0x07: {printf("RLC\tA");} break;

            case 0x08: {printf("RRC\tB");} break;
            case 0x09: {printf("RRC\tC");} break;
            case 0x0A: {printf("RRC\tD");} break;
            case 0x0B: {printf("RRC\tE");} break;
            case 0x0C: {printf("RRC\tH");} break;
            case 0x0D: {printf("RRC\tL");} break;
            case 0x0E: {printf("RRC\t(HL)");} break;
            case 0x0F: {printf("RRC\tA");} break;

            case 0x10: {printf("RL\tB");} break;
            case 0x11: {printf("RL\tC");} break;
            case 0x12: {printf("RL\tD");} break;
            case 0x13: {printf("RL\tE");} break;
            case 0x14: {printf("RL\tH");} break;
            case 0x15: {printf("RL\tL");} break;
            case 0x16: {printf("RL\t(HL)");} break;
            case 0x17: {printf("RL\tA");} break;

            case 0x18: {printf("RR\tB");} break;
            case 0x19: {printf("RR\tC");} break;
            case 0x1A: {printf("RR\tD");} break;
            case 0x1B: {printf("RR\tE");} break;
            case 0x1C: {printf("RR\tH");} break;
            case 0x1D: {printf("RR\tL");} break;
            case 0x1E: {printf("RR\t(HL)");} break;
            case 0x1F: {printf("RR\tA");} break;

            case 0x20: {printf("SLA\tB");} break;
            case 0x21: {printf("SLA\tC");} break;
            case 0x22: {printf("SLA\tD");} break;
            case 0x23: {printf("SLA\tE");} break;
            case 0x24: {printf("SLA\tH");} break;
            case 0x25: {printf("SLA\tL");} break;
            case 0x26: {printf("SLA\t(HL)");} break;
            case 0x27: {printf("SLA\tA");} break;

            case 0x28: {printf("SRA\tB");} break;
            case 0x29: {printf("SRA\tC");} break;
            case 0x2A: {printf("SRA\tD");} break;
            case 0x2B: {printf("SRA\tE");} break;
            case 0x2C: {printf("SRA\tH");} break;
            case 0x2D: {printf("SRA\tL");} break;
            case 0x2E: {printf("SRA\t(HL)");} break;
            case 0x2F: {printf("SRA\tA");} break;

            case 0x30: {printf("SWAP\tB");} break;
            case 0x31: {printf("SWAP\tC");} break;
            case 0x32: {printf("SWAP\tD");} break;
            case 0x33: {printf("SWAP\tE");} break;
            case 0x34: {printf("SWAP\tH");} break;
            case 0x35: {printf("SWAP\tL");} break;
            case 0x36: {printf("SWAP\t(HL)");} break;
            case 0x37: {printf("SWAP\tA");} break;

            case 0x38: {printf("SRL\tB");} break;
            case 0x39: {printf("SRL\tC");} break;
            case 0x3A: {printf("SRL\tD");} break;
            case 0x3B: {printf("SRL\tE");} break;
            case 0x3C: {printf("SRL\tH");} break;
            case 0x3D: {printf("SRL\tL");} break;
            case 0x3E: {printf("SRL\t(HL)");} break;
            case 0x3F: {printf("SRL\tA");} break;


            // NOTE(law): Single-bit Operation instructions
            case 0x40: {printf("BIT\t0, B");} break;
            case 0x41: {printf("BIT\t0, C");} break;
            case 0x42: {printf("BIT\t0, D");} break;
            case 0x43: {printf("BIT\t0, E");} break;
            case 0x44: {printf("BIT\t0, H");} break;
            case 0x45: {printf("BIT\t0, L");} break;
            case 0x46: {printf("BIT\t0, (HL)");} break;
            case 0x47: {printf("BIT\t0, A");} break;

            case 0x48: {printf("BIT\t1, B");} break;
            case 0x49: {printf("BIT\t1, C");} break;
            case 0x4A: {printf("BIT\t1, D");} break;
            case 0x4B: {printf("BIT\t1, E");} break;
            case 0x4C: {printf("BIT\t1, H");} break;
            case 0x4D: {printf("BIT\t1, L");} break;
            case 0x4E: {printf("BIT\t1, (HL)");} break;
            case 0x4F: {printf("BIT\t1, A");} break;

            case 0x50: {printf("BIT\t2, B");} break;
            case 0x51: {printf("BIT\t2, C");} break;
            case 0x52: {printf("BIT\t2, D");} break;
            case 0x53: {printf("BIT\t2, E");} break;
            case 0x54: {printf("BIT\t2, H");} break;
            case 0x55: {printf("BIT\t2, L");} break;
            case 0x56: {printf("BIT\t2, (HL)");} break;
            case 0x57: {printf("BIT\t2, A");} break;

            case 0x58: {printf("BIT\t3, B");} break;
            case 0x59: {printf("BIT\t3, C");} break;
            case 0x5A: {printf("BIT\t3, D");} break;
            case 0x5B: {printf("BIT\t3, E");} break;
            case 0x5C: {printf("BIT\t3, H");} break;
            case 0x5D: {printf("BIT\t3, L");} break;
            case 0x5E: {printf("BIT\t3, (HL)");} break;
            case 0x5F: {printf("BIT\t3, A");} break;

            case 0x60: {printf("BIT\t4, B");} break;
            case 0x61: {printf("BIT\t4, C");} break;
            case 0x62: {printf("BIT\t4, D");} break;
            case 0x63: {printf("BIT\t4, E");} break;
            case 0x64: {printf("BIT\t4, H");} break;
            case 0x65: {printf("BIT\t4, L");} break;
            case 0x66: {printf("BIT\t4, (HL)");} break;
            case 0x67: {printf("BIT\t4, A");} break;

            case 0x68: {printf("BIT\t5, B");} break;
            case 0x69: {printf("BIT\t5, C");} break;
            case 0x6A: {printf("BIT\t5, D");} break;
            case 0x6B: {printf("BIT\t5, E");} break;
            case 0x6C: {printf("BIT\t5, H");} break;
            case 0x6D: {printf("BIT\t5, L");} break;
            case 0x6E: {printf("BIT\t5, (HL)");} break;
            case 0x6F: {printf("BIT\t5, A");} break;

            case 0x70: {printf("BIT\t6, B");} break;
            case 0x71: {printf("BIT\t6, C");} break;
            case 0x72: {printf("BIT\t6, D");} break;
            case 0x73: {printf("BIT\t6, E");} break;
            case 0x74: {printf("BIT\t6, H");} break;
            case 0x75: {printf("BIT\t6, L");} break;
            case 0x76: {printf("BIT\t6, (HL)");} break;
            case 0x77: {printf("BIT\t6, A");} break;

            case 0x78: {printf("BIT\t7, B");} break;
            case 0x79: {printf("BIT\t7, C");} break;
            case 0x7A: {printf("BIT\t7, D");} break;
            case 0x7B: {printf("BIT\t7, E");} break;
            case 0x7C: {printf("BIT\t7, H");} break;
            case 0x7D: {printf("BIT\t7, L");} break;
            case 0x7E: {printf("BIT\t7, (HL)");} break;
            case 0x7F: {printf("BIT\t7, A");} break;

            case 0x80: {printf("RES\t0, B");} break;
            case 0x81: {printf("RES\t0, C");} break;
            case 0x82: {printf("RES\t0, D");} break;
            case 0x83: {printf("RES\t0, E");} break;
            case 0x84: {printf("RES\t0, H");} break;
            case 0x85: {printf("RES\t0, L");} break;
            case 0x86: {printf("RES\t0, (HL)");} break;
            case 0x87: {printf("RES\t0, A");} break;

            case 0x88: {printf("RES\t1, B");} break;
            case 0x89: {printf("RES\t1, C");} break;
            case 0x8A: {printf("RES\t1, D");} break;
            case 0x8B: {printf("RES\t1, E");} break;
            case 0x8C: {printf("RES\t1, H");} break;
            case 0x8D: {printf("RES\t1, L");} break;
            case 0x8E: {printf("RES\t1, (HL)");} break;
            case 0x8F: {printf("RES\t1, A");} break;

            case 0x90: {printf("RES\t2, B");} break;
            case 0x91: {printf("RES\t2, C");} break;
            case 0x92: {printf("RES\t2, D");} break;
            case 0x93: {printf("RES\t2, E");} break;
            case 0x94: {printf("RES\t2, H");} break;
            case 0x95: {printf("RES\t2, L");} break;
            case 0x96: {printf("RES\t2, (HL)");} break;
            case 0x97: {printf("RES\t2, A");} break;

            case 0x98: {printf("RES\t3, B");} break;
            case 0x99: {printf("RES\t3, C");} break;
            case 0x9A: {printf("RES\t3, D");} break;
            case 0x9B: {printf("RES\t3, E");} break;
            case 0x9C: {printf("RES\t3, H");} break;
            case 0x9D: {printf("RES\t3, L");} break;
            case 0x9E: {printf("RES\t3, (HL)");} break;
            case 0x9F: {printf("RES\t3, A");} break;

            case 0xA0: {printf("RES\t4, B");} break;
            case 0xA1: {printf("RES\t4, C");} break;
            case 0xA2: {printf("RES\t4, D");} break;
            case 0xA3: {printf("RES\t4, E");} break;
            case 0xA4: {printf("RES\t4, H");} break;
            case 0xA5: {printf("RES\t4, L");} break;
            case 0xA6: {printf("RES\t4, (HL)");} break;
            case 0xA7: {printf("RES\t4, A");} break;

            case 0xA8: {printf("RES\t5, B");} break;
            case 0xA9: {printf("RES\t5, C");} break;
            case 0xAA: {printf("RES\t5, D");} break;
            case 0xAB: {printf("RES\t5, E");} break;
            case 0xAC: {printf("RES\t5, H");} break;
            case 0xAD: {printf("RES\t5, L");} break;
            case 0xAE: {printf("RES\t5, (HL)");} break;
            case 0xAF: {printf("RES\t5, A");} break;

            case 0xB0: {printf("RES\t6, B");} break;
            case 0xB1: {printf("RES\t6, C");} break;
            case 0xB2: {printf("RES\t6, D");} break;
            case 0xB3: {printf("RES\t6, E");} break;
            case 0xB4: {printf("RES\t6, H");} break;
            case 0xB5: {printf("RES\t6, L");} break;
            case 0xB6: {printf("RES\t6, (HL)");} break;
            case 0xB7: {printf("RES\t6, A");} break;

            case 0xB8: {printf("RES\t7, B");} break;
            case 0xB9: {printf("RES\t7, C");} break;
            case 0xBA: {printf("RES\t7, D");} break;
            case 0xBB: {printf("RES\t7, E");} break;
            case 0xBC: {printf("RES\t7, H");} break;
            case 0xBD: {printf("RES\t7, L");} break;
            case 0xBE: {printf("RES\t7, (HL)");} break;
            case 0xBF: {printf("RES\t7, A");} break;

            case 0xC0: {printf("SET\t0, B");} break;
            case 0xC1: {printf("SET\t0, C");} break;
            case 0xC2: {printf("SET\t0, D");} break;
            case 0xC3: {printf("SET\t0, E");} break;
            case 0xC4: {printf("SET\t0, H");} break;
            case 0xC5: {printf("SET\t0, L");} break;
            case 0xC6: {printf("SET\t0, (HL)");} break;
            case 0xC7: {printf("SET\t0, A");} break;

            case 0xC8: {printf("SET\t1, B");} break;
            case 0xC9: {printf("SET\t1, C");} break;
            case 0xCA: {printf("SET\t1, D");} break;
            case 0xCB: {printf("SET\t1, E");} break;
            case 0xCC: {printf("SET\t1, H");} break;
            case 0xCD: {printf("SET\t1, L");} break;
            case 0xCE: {printf("SET\t1, (HL)");} break;
            case 0xCF: {printf("SET\t1, A");} break;

            case 0xD0: {printf("SET\t2, B");} break;
            case 0xD1: {printf("SET\t2, C");} break;
            case 0xD2: {printf("SET\t2, D");} break;
            case 0xD3: {printf("SET\t2, E");} break;
            case 0xD4: {printf("SET\t2, H");} break;
            case 0xD5: {printf("SET\t2, L");} break;
            case 0xD6: {printf("SET\t2, (HL)");} break;
            case 0xD7: {printf("SET\t2, A");} break;

            case 0xD8: {printf("SET\t3, B");} break;
            case 0xD9: {printf("SET\t3, C");} break;
            case 0xDA: {printf("SET\t3, D");} break;
            case 0xDB: {printf("SET\t3, E");} break;
            case 0xDC: {printf("SET\t3, H");} break;
            case 0xDD: {printf("SET\t3, L");} break;
            case 0xDE: {printf("SET\t3, (HL)");} break;
            case 0xDF: {printf("SET\t3, A");} break;

            case 0xE0: {printf("SET\t4, B");} break;
            case 0xE1: {printf("SET\t4, C");} break;
            case 0xE2: {printf("SET\t4, D");} break;
            case 0xE3: {printf("SET\t4, E");} break;
            case 0xE4: {printf("SET\t4, H");} break;
            case 0xE5: {printf("SET\t4, L");} break;
            case 0xE6: {printf("SET\t4, (HL)");} break;
            case 0xE7: {printf("SET\t4, A");} break;

            case 0xE8: {printf("SET\t5, B");} break;
            case 0xE9: {printf("SET\t5, C");} break;
            case 0xEA: {printf("SET\t5, D");} break;
            case 0xEB: {printf("SET\t5, E");} break;
            case 0xEC: {printf("SET\t5, H");} break;
            case 0xED: {printf("SET\t5, L");} break;
            case 0xEE: {printf("SET\t5, (HL)");} break;
            case 0xEF: {printf("SET\t5, A");} break;

            case 0xF0: {printf("SET\t6, B");} break;
            case 0xF1: {printf("SET\t6, C");} break;
            case 0xF2: {printf("SET\t6, D");} break;
            case 0xF3: {printf("SET\t6, E");} break;
            case 0xF4: {printf("SET\t6, H");} break;
            case 0xF5: {printf("SET\t6, L");} break;
            case 0xF6: {printf("SET\t6, (HL)");} break;
            case 0xF7: {printf("SET\t6, A");} break;

            case 0xF8: {printf("SET\t7, B");} break;
            case 0xF9: {printf("SET\t7, C");} break;
            case 0xFA: {printf("SET\t7, D");} break;
            case 0xFB: {printf("SET\t7, E");} break;
            case 0xFC: {printf("SET\t7, H");} break;
            case 0xFD: {printf("SET\t7, L");} break;
            case 0xFE: {printf("SET\t7, (HL)");} break;
            case 0xFF: {printf("SET\t7, A");} break;

            default: {assert(!"UNHANDLED OPCODE");} break;
         }
      }
      else
      {
         // NOTE(law): Parse non-prefix instructions.
         printf("(%02x) ", opcode);

         switch(opcode)
         {
            // NOTE(law): 8-bit load instructions
            case 0x40: {printf("LD\tB, B");} break;
            case 0x41: {printf("LD\tB, C");} break;
            case 0x42: {printf("LD\tB, D");} break;
            case 0x43: {printf("LD\tB, E");} break;
            case 0x44: {printf("LD\tB, H");} break;
            case 0x45: {printf("LD\tB, L");} break;
            case 0x46: {printf("LD\tB, (HL)");} break;
            case 0x47: {printf("LD\tB, A");} break;

            case 0x48: {printf("LD\tC, B");} break;
            case 0x49: {printf("LD\tC, C");} break;
            case 0x4A: {printf("LD\tC, D");} break;
            case 0x4B: {printf("LD\tC, E");} break;
            case 0x4C: {printf("LD\tC, H");} break;
            case 0x4D: {printf("LD\tC, L");} break;
            case 0x4E: {printf("LD\tC, (HL)");} break;
            case 0x4F: {printf("LD\tC, A");} break;

            case 0x50: {printf("LD\tD, B");} break;
            case 0x51: {printf("LD\tD, C");} break;
            case 0x52: {printf("LD\tD, D");} break;
            case 0x53: {printf("LD\tD, E");} break;
            case 0x54: {printf("LD\tD, H");} break;
            case 0x55: {printf("LD\tD, L");} break;
            case 0x56: {printf("LD\tD, (HL)");} break;
            case 0x57: {printf("LD\tD, A");} break;

            case 0x58: {printf("LD\tE, B");} break;
            case 0x59: {printf("LD\tE, C");} break;
            case 0x5A: {printf("LD\tE, D");} break;
            case 0x5B: {printf("LD\tE, E");} break;
            case 0x5C: {printf("LD\tE, H");} break;
            case 0x5D: {printf("LD\tE, L");} break;
            case 0x5E: {printf("LD\tE, (HL)");} break;
            case 0x5F: {printf("LD\tE, A");} break;

            case 0x60: {printf("LD\tH, B");} break;
            case 0x61: {printf("LD\tH, C");} break;
            case 0x62: {printf("LD\tH, D");} break;
            case 0x63: {printf("LD\tH, E");} break;
            case 0x64: {printf("LD\tH, H");} break;
            case 0x65: {printf("LD\tH, L");} break;
            case 0x66: {printf("LD\tH, (HL)");} break;
            case 0x67: {printf("LD\tH, A");} break;

            case 0x68: {printf("LD\tL, B");} break;
            case 0x69: {printf("LD\tL, C");} break;
            case 0x6A: {printf("LD\tL, D");} break;
            case 0x6B: {printf("LD\tL, E");} break;
            case 0x6C: {printf("LD\tL, H");} break;
            case 0x6D: {printf("LD\tL, L");} break;
            case 0x6E: {printf("LD\tL, (HL)");} break;
            case 0x6F: {printf("LD\tL, A");} break;

            case 0x70: {printf("LD\t(HL), B");} break;
            case 0x71: {printf("LD\t(HL), C");} break;
            case 0x72: {printf("LD\t(HL), D");} break;
            case 0x73: {printf("LD\t(HL), E");} break;
            case 0x74: {printf("LD\t(HL), H");} break;
            case 0x75: {printf("LD\t(HL), L");} break;
            case 0x77: {printf("LD\t(HL), A");} break;

            case 0x78: {printf("LD\tA, B");} break;
            case 0x79: {printf("LD\tA, C");} break;
            case 0x7A: {printf("LD\tA, D");} break;
            case 0x7B: {printf("LD\tA, E");} break;
            case 0x7C: {printf("LD\tA, H");} break;
            case 0x7D: {printf("LD\tA, L");} break;
            case 0x7E: {printf("LD\tA, (HL)");} break;
            case 0x7F: {printf("LD\tA, A");} break;

            case 0x06: {printf("LD\tB, 0x%02x", stream[offset++]);} break;
            case 0x0E: {printf("LD\tC, 0x%02x", stream[offset++]);} break;
            case 0x16: {printf("LD\tD, 0x%02x", stream[offset++]);} break;
            case 0x1E: {printf("LD\tE, 0x%02x", stream[offset++]);} break;
            case 0x26: {printf("LD\tH, 0x%02x", stream[offset++]);} break;
            case 0x2E: {printf("LD\tL, 0x%02x", stream[offset++]);} break;
            case 0x36: {printf("LD\t(HL), 0x%02x", stream[offset++]);} break;
            case 0x3E: {printf("LD\tA, 0x%02x", stream[offset++]);} break;

            case 0x0A: {printf("LD\tA, (BC)");} break;
            case 0x1A: {printf("LD\tA, (DE)");} break;

            case 0xFA:
            {
               unsigned short operand = *((unsigned short *)(stream + offset));
               offset += 2;
               printf("LD\tA, (0x%04x)", operand);
            } break;

            case 0x02: {printf("LD\t(BC), A");} break;
            case 0x12: {printf("LD\t(DE), A");} break;

            case 0xEA:
            {
               unsigned short operand = *((unsigned short *)(stream + offset));
               offset += 2;
               printf("LD\t(0x%04x), A", operand);
            } break;

            case 0xF2: {printf("LDH\tA, (FF00 + C)");} break;
            case 0xE2: {printf("LDH\t(FF00 + C), A");} break;

            case 0xF0: {printf("LDH\tA, (FF00 + 0x%02x)", stream[offset++]);} break;
            case 0xE0: {printf("LDH\t(FF00 + 0x%02x), A", stream[offset++]);} break;

            case 0x22: {printf("LDI\t(HL), A");} break;
            case 0x32: {printf("LDD\t(HL), A");} break;
            case 0x2A: {printf("LDI\tA, (HL)");} break;
            case 0x3A: {printf("LDD\tA, (HL)");} break;

            case 0xF9: {printf("LD\tSP, HL");} break;

            case 0xC5: {printf("PUSH\tBC");} break;
            case 0xD5: {printf("PUSH\tDE");} break;
            case 0xE5: {printf("PUSH\tHL");} break;
            case 0xF5: {printf("PUSH\tAF");} break;

            case 0xC1: {printf("POP\tBC");} break;
            case 0xD1: {printf("POP\tDE");} break;
            case 0xE1: {printf("POP\tHL");} break;
            case 0xF1: {printf("POP\tAF");} break;


            // NOTE(law): 16-bit load instructions
            case 0x08:
            {
               unsigned short operand = *((unsigned short *)(stream + offset));
               offset += 2;
               printf("LD\t0x%04x, SP", operand);
            } break;

            case 0x01:
            {
               unsigned short operand = *((unsigned short *)(stream + offset));
               offset += 2;
               printf("LD\tBC, 0x%04x", operand);
            } break;

            case 0x11:
            {
               unsigned short operand = *((unsigned short *)(stream + offset));
               offset += 2;
               printf("LD\tDE, 0x%04x", operand);
            } break;

            case 0x21:
            {
               unsigned short operand = *((unsigned short *)(stream + offset));
               offset += 2;
               printf("LD\tHL, 0x%04x", operand);
            } break;

            case 0x31:
            {
               unsigned short operand = *((unsigned short *)(stream + offset));
               offset += 2;
               printf("LD\tSP, 0x%04x", operand);
            } break;


            // NOTE(law): Rotate and Shift instructions
            case 0x07: {printf("RLCA");} break;
            case 0x17: {printf("RLA");} break;
            case 0x0F: {printf("RRCA");} break;
            case 0x1F: {printf("RRA");} break;


            // NOTE(law): 8-bit Arithmetic/Logic instructions
            case 0x80: {printf("ADD\tA, B");} break;
            case 0x81: {printf("ADD\tA, C");} break;
            case 0x82: {printf("ADD\tA, D");} break;
            case 0x83: {printf("ADD\tA, E");} break;
            case 0x84: {printf("ADD\tA, H");} break;
            case 0x85: {printf("ADD\tA, L");} break;
            case 0x86: {printf("ADD\tA, (HL)");} break;
            case 0x87: {printf("ADD\tA");} break;

            case 0xC6:
            {
               unsigned char operand = *(stream + offset++);
               printf("ADD\tA, 0x%02x", operand);
            } break;

            case 0x88: {printf("ADC\tA, B");} break;
            case 0x89: {printf("ADC\tA, C");} break;
            case 0x8A: {printf("ADC\tA, D");} break;
            case 0x8B: {printf("ADC\tA, E");} break;
            case 0x8C: {printf("ADC\tA, H");} break;
            case 0x8D: {printf("ADC\tA, L");} break;
            case 0x8E: {printf("ADC\tA, (HL)");} break;
            case 0x8F: {printf("ADC\tA, A");} break;

            case 0xCE:
            {
               unsigned char operand = *(stream + offset++);
               printf("ADC\tA, 0x%02x", operand);
            } break;

            case 0x90: {printf("SUB\tA, B");} break;
            case 0x91: {printf("SUB\tA, C");} break;
            case 0x92: {printf("SUB\tA, D");} break;
            case 0x93: {printf("SUB\tA, E");} break;
            case 0x94: {printf("SUB\tA, H");} break;
            case 0x95: {printf("SUB\tA, L");} break;
            case 0x96: {printf("SUB\tA, (HL)");} break;
            case 0x97: {printf("SUB\tA");} break;

            case 0xD6:
            {
               unsigned char operand = *(stream + offset++);
               printf("SUB\t0x%02x", operand);
            } break;

            case 0x98: {printf("SBC\tA, B");} break;
            case 0x99: {printf("SBC\tA, C");} break;
            case 0x9A: {printf("SBC\tA, D");} break;
            case 0x9B: {printf("SBC\tA, E");} break;
            case 0x9C: {printf("SBC\tA, H");} break;
            case 0x9D: {printf("SBC\tA, L");} break;
            case 0x9E: {printf("SBC\tA, (HL)");} break;
            case 0x9F: {printf("SBC\tA, A");} break;

            case 0xDE: {printf("SBC\tA, 0x%02x", stream[offset++]);} break;

            case 0x27: {printf("DAA");} break;
            case 0x2F: {printf("CPL");} break;

            case 0xA8: {printf("XOR\tB");} break;
            case 0xA9: {printf("XOR\tC");} break;
            case 0xAA: {printf("XOR\tD");} break;
            case 0xAB: {printf("XOR\tE");} break;
            case 0xAC: {printf("XOR\tH");} break;
            case 0xAD: {printf("XOR\tL");} break;
            case 0xAE: {printf("XOR\t(HL)");} break;
            case 0xAF: {printf("XOR\tA");} break;

            case 0xEE: {printf("XOR\t0x%02x", stream[offset++]);} break;

            case 0xB0: {printf("OR\tB");} break;
            case 0xB1: {printf("OR\tC");} break;
            case 0xB2: {printf("OR\tD");} break;
            case 0xB3: {printf("OR\tE");} break;
            case 0xB4: {printf("OR\tH");} break;
            case 0xB5: {printf("OR\tL");} break;
            case 0xB6: {printf("OR\t(HL)");} break;
            case 0xB7: {printf("OR\tA");} break;

            case 0xF6: {printf("OR\t0x%02x", stream[offset++]);} break;

            case 0xA0: {printf("AND\tB");} break;
            case 0xA1: {printf("AND\tC");} break;
            case 0xA2: {printf("AND\tD");} break;
            case 0xA3: {printf("AND\tE");} break;
            case 0xA4: {printf("AND\tH");} break;
            case 0xA5: {printf("AND\tL");} break;
            case 0xA6: {printf("AND\t(HL)");} break;
            case 0xA7: {printf("AND\tA");} break;

            case 0xE6: {printf("AND\t0x%02x", stream[offset++]);} break;

            case 0xB8: {printf("CP\tB");} break;
            case 0xB9: {printf("CP\tC");} break;
            case 0xBA: {printf("CP\tD");} break;
            case 0xBB: {printf("CP\tE");} break;
            case 0xBC: {printf("CP\tH");} break;
            case 0xBD: {printf("CP\tL");} break;
            case 0xBE: {printf("CP\t(HL)");} break;
            case 0xBF: {printf("CP\tA");} break;

            case 0xFE: {printf("CP\t0x%02x", stream[offset++]);} break;

            case 0x04: {printf("INC\tB");} break;
            case 0x0C: {printf("INC\tC");} break;
            case 0x14: {printf("INC\tD");} break;
            case 0x1C: {printf("INC\tE");} break;
            case 0x24: {printf("INC\tH");} break;
            case 0x2C: {printf("INC\tL");} break;
            case 0x34: {printf("INC\t(HL)");} break;
            case 0x3C: {printf("INC\tA");} break;

            case 0x05: {printf("DEC\tB");} break;
            case 0x0D: {printf("DEC\tC");} break;
            case 0x15: {printf("DEC\tD");} break;
            case 0x1D: {printf("DEC\tE");} break;
            case 0x25: {printf("DEC\tH");} break;
            case 0x2D: {printf("DEC\tL");} break;
            case 0x35: {printf("DEC\t(HL)");} break;
            case 0x3D: {printf("DEC\tA");} break;

            // NOTE(law): 16-bit Arithmetic/Logic instructions
            case 0x09: {printf("ADD\tHL, BC");} break;
            case 0x19: {printf("ADD\tHL, DE");} break;
            case 0x29: {printf("ADD\tHL, HL");} break;
            case 0x39: {printf("ADD\tHL, SP");} break;

            case 0x03: {printf("INC\tBC");} break;
            case 0x13: {printf("INC\tDE");} break;
            case 0x23: {printf("INC\tHL");} break;
            case 0x33: {printf("INC\tSP");} break;

            case 0x0B: {printf("DEC\tBC");} break;
            case 0x1B: {printf("DEC\tDE");} break;
            case 0x2B: {printf("DEC\tHL");} break;
            case 0x3B: {printf("DEC\tSP");} break;

            case 0xE8: {printf("ADD\tSP, 0x%02x", stream[offset++]);} break;
            case 0xF8: {printf("LD\tHL, SP + 0x%02x", stream[offset++]);} break;

            // NOTE(law): CPU Control instructions
            case 0x3F: {printf("CCF");} break;
            case 0x37: {printf("SCF");} break;
            case 0x00: {printf("NOP");} break;
            case 0x76: {printf("HALT");} break;
            case 0x10: {printf("STOP\t0x%02x", stream[offset++]);} break;
            case 0xF3: {printf("DI");} break;
            case 0xFB: {printf("EI");} break;


            // NOTE(law): Jump instructions
            case 0xE9: {printf("JP\tHL");} break;

            case 0xC3:
            {
               unsigned short operand = *(unsigned short *)(stream + offset);
               offset += 2;
               printf("JP\t0x%04x", operand);
            } break;

            case 0xC2:
            {
               unsigned short operand = *(unsigned short *)(stream + offset);
               offset += 2;
               printf("JP\tNZ, 0x%04x", operand);
            } break;

            case 0xCA:
            {
               unsigned short operand = *(unsigned short *)(stream + offset);
               offset += 2;
               printf("JP\tZ, 0x%04x", operand);
            } break;

            case 0xD2:
            {
               unsigned short operand = *(unsigned short *)(stream + offset);
               offset += 2;
               printf("JP\tNC, 0x%04x", operand);
            } break;

            case 0xDA:
            {
               unsigned short operand = *(unsigned short *)(stream + offset);
               offset += 2;
               printf("JP\tC, 0x%04x", operand);
            } break;

            case 0x18: {printf("JR\tPC + 0x%02x", stream[offset++]);} break;
            case 0x20: {printf("JR\tNZ, PC + 0x%02x", stream[offset++]);} break;
            case 0x28: {printf("JR\tZ, PC + 0x%02x", stream[offset++]);} break;
            case 0x30: {printf("JR\tNC, PC + 0x%02x", stream[offset++]);} break;
            case 0x38: {printf("JR\tC, PC + 0x%02x", stream[offset++]);} break;

            case 0xC4:
            {
               unsigned short operand = *((unsigned short *)(stream + offset));
               offset += 2;
               printf("CALL\tNZ, 0x%04x", operand);
            } break;

            case 0xCC:
            {
               unsigned short operand = *((unsigned short *)(stream + offset));
               offset += 2;
               printf("CALL\tZ, 0x%04x", operand);
            } break;

            case 0xCD:
            {
               unsigned short operand = *((unsigned short *)(stream + offset));
               offset += 2;
               printf("CALL\t0x%04x", operand);
            } break;

            case 0xD4:
            {
               unsigned short operand = *((unsigned short *)(stream + offset));
               offset += 2;
               printf("CALL\tNC, 0x%04x", operand);
            } break;

            case 0xDC:
            {
               unsigned short operand = *((unsigned short *)(stream + offset));
               offset += 2;
               printf("CALL\tC, 0x%04x", operand);
            } break;

            case 0xC0: {printf("RET\tNZ");} break;
            case 0xC8: {printf("RET\tZ");} break;
            case 0xC9: {printf("RET");} break;
            case 0xD0: {printf("RET\tNC");} break;
            case 0xD8: {printf("RET\tC");} break;

            case 0xD9: {printf("RETI");} break;

            case 0xC7: {printf("RST\t00H");} break;
            case 0xCF: {printf("RST\t08H");} break;
            case 0xD7: {printf("RST\t10H");} break;
            case 0xDF: {printf("RST\t18H");} break;
            case 0xE7: {printf("RST\t20H");} break;
            case 0xEF: {printf("RST\t28H");} break;
            case 0xF7: {printf("RST\t30H");} break;
            case 0xFF: {printf("RST\t38H");} break;

            case 0xD3: {printf("ILLEGAL_D3");} break;
            case 0xDB: {printf("ILLEGAL_DB");} break;
            case 0xDD: {printf("ILLEGAL_DD");} break;
            case 0xE3: {printf("ILLEGAL_E3");} break;
            case 0xE4: {printf("ILLEGAL_E4");} break;
            case 0xEB: {printf("ILLEGAL_EB");} break;
            case 0xEC: {printf("ILLEGAL_EC");} break;
            case 0xED: {printf("ILLEGAL_ED");} break;
            case 0xF4: {printf("ILLEGAL_F4");} break;
            case 0xFC: {printf("ILLEGAL_FC");} break;
            case 0xFD: {printf("ILLEGAL_FD");} break;

            default: {assert(!"UNHANDLED OPCODE");} break;
         }
      }

      printf("\n");
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

#define FLAG_Z_BIT 7
#define FLAG_N_BIT 6
#define FLAG_H_BIT 5
#define FLAG_C_BIT 4

#define FLAG_Z ((register_f >> FLAG_Z_BIT) & 0x1)
#define FLAG_N ((register_f >> FLAG_N_BIT) & 0x1)
#define FLAG_H ((register_f >> FLAG_H_BIT) & 0x1)
#define FLAG_C ((register_f >> FLAG_C_BIT) & 0x1)

#define FLAG_Z_MASK (1 << FLAG_Z_BIT)
#define FLAG_N_MASK (1 << FLAG_N_BIT)
#define FLAG_H_MASK (1 << FLAG_H_BIT)
#define FLAG_C_MASK (1 << FLAG_C_BIT)

#define REGISTER_BC (((unsigned short)register_b << 8) | (unsigned short)register_c)
#define REGISTER_DE (((unsigned short)register_d << 8) | (unsigned short)register_e)
#define REGISTER_HL (((unsigned short)register_h << 8) | (unsigned short)register_l)
#define REGISTER_AF (((unsigned short)register_a << 8) | (unsigned short)register_f)

static void
add(unsigned char value)
{
   // NOTE(law): Compute these values before updating register A for use in the
   // flag calculations.
   unsigned short extended_sum = (unsigned short)register_a + (unsigned short)value;
   unsigned char half_sum = (register_a & 0xF) + (value & 0xF);

   register_a = (unsigned char)extended_sum;

   // NOTE(law): Clear the flags to zero.
   register_f = 0;

   // NOTE(law): Set the Zero flag bit if the result of the computation produced
   // a zero.
   if(register_a == 0)
   {
      register_f |= FLAG_Z_MASK;
   }

   // NOTE(law): Always unset the Subtraction flag during an ADD operation (This
   // is actually a no-op, since the flag is cleared up top).
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag if adding the low 4 bits of register A
   // and the incoming value sets bit 5 of the resulting sum.
   if((half_sum & 0x10) == 0x10)
   {
      register_f |= FLAG_H_MASK;
   }

   // NOTE(law): Set the Carry flag if adding the full 8 bits of register A and
   // the incoming value would create a sum greater than the maximum byte value
   // 0xFF (assuming the types used avoided an overflow).
   if(extended_sum > 0xFF)
   {
      register_f |= FLAG_C_MASK;
   }
}

static void
adc(unsigned char value)
{
   unsigned short extended_sum = (unsigned short)register_a + (unsigned short)value + FLAG_C;
   unsigned char half_sum = (register_a & 0xF) + ((value + FLAG_C) & 0xF);

   register_a = (unsigned char)extended_sum;

   register_f = 0;

   if(register_a == 0)
   {
      register_f |= FLAG_Z_MASK;
   }

   register_f &= ~FLAG_N_MASK;

   if((half_sum & 0x10) == 0x10)
   {
      register_f |= FLAG_H_MASK;
   }

   if(extended_sum > 0xFF)
   {
      register_f |= FLAG_C_MASK;
   }
}

static void
sub(unsigned char value)
{
   // NOTE(law): Compute these values before updating register A for use in the
   // flag calculations.
   bool is_negative = (signed char)register_a < (signed char)value;
   bool is_half_negative = (signed char)(register_a & 0xF) < (signed char)(value & 0xF);

   register_a -= value;

   // NOTE(law): Clear the flags to zero.
   register_f = 0;

   // NOTE(law): Set the Zero flag bit if the result of the computation produced
   // a zero.
   if(register_a == 0)
   {
      register_f |= FLAG_Z_MASK;
   }

   // NOTE(law): Always the Subtraction flag during a SUB operation.
   register_f |= FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag if subtracting the low 4 bits of the
   // incoming value from the low 4 bits of register A results in a negative
   // number.
   if(is_half_negative)
   {
      register_f |= FLAG_H_MASK;
   }

   // NOTE(law): Set the Carry flag if subtracting the the incoming value from
   // register A results in a negative number.
   if(is_negative)
   {
      register_f |= FLAG_C_MASK;
   }
}

static void
sbc(unsigned char value)
{
   signed char value_and_carry = (signed char)value + FLAG_C;

   bool is_negative = (signed char)register_a < value_and_carry;
   bool is_half_negative = (signed char)(register_a & 0xF) < (value_and_carry & 0xF);

   register_a -= (value);

   register_f = 0;

   if(register_a == 0)
   {
      register_f |= FLAG_Z_MASK;
   }

   register_f |= FLAG_N_MASK;

   if(is_half_negative)
   {
      register_f |= FLAG_H_MASK;
   }

   if(is_negative)
   {
      register_f |= FLAG_C_MASK;
   }
}

static void
xor(unsigned char value)
{
   register_a ^= value;

   register_f = 0;

   if(register_a == 0)
   {
      register_f |= FLAG_Z_MASK;
   }
}

static void
or(unsigned char value)
{
   register_a |= value;

   register_f = 0;

   if(register_a == 0)
   {
      register_f |= FLAG_Z_MASK;
   }
}

static void
and(unsigned char value)
{
   register_a &= value;

   register_f = 0;

   if(register_a == 0)
   {
      register_f |= FLAG_Z_MASK;
   }

   register_f |= FLAG_H_MASK;
}

static void
cp(unsigned char value)
{
   bool is_negative = (signed char)register_a < (signed char)value;
   bool is_half_negative = (signed char)(register_a & 0xF) < (signed char)(value & 0xF);

   register_f = 0;

   if(register_a == value)
   {
      register_f |= FLAG_Z_MASK;
   }

   register_f |= FLAG_N_MASK;

   if(is_half_negative)
   {
      register_f |= FLAG_H_MASK;
   }

   if(is_negative)
   {
      register_f |= FLAG_C_MASK;
   }
}

static void
inc(unsigned char *value)
{
   unsigned char half_sum = (*value & 0xF) + 1;

   *value += 1;

   if(register_a == 0)
   {
      register_f |= FLAG_Z_MASK;
   }

   register_f &= ~FLAG_N_MASK;

   if((half_sum & 0x10) == 0x10)
   {
      register_f |= FLAG_H_MASK;
   }
}

static void
dec(unsigned char *value)
{
   bool is_half_negative = (signed char)(*value & 0xF) < 1;

   *value -= 1;

   if(register_a == 0)
   {
      register_f |= FLAG_Z_MASK;
   }

   register_f |= FLAG_N_MASK;

   if(is_half_negative)
   {
      register_f |= FLAG_H_MASK;
   }
}

static void
add16(unsigned short value)
{
   unsigned int extended_sum = (unsigned int)REGISTER_HL + (unsigned int)value;
   unsigned char half_sum = (REGISTER_HL & 0xF) + (value & 0xF);

   unsigned short sum = REGISTER_HL + value;
   register_h = (sum >> 8);
   register_l = (sum & 0xFF);

   register_f &= ~FLAG_N_MASK;

   if((half_sum & 0x10) == 0x10)
   {
      register_f |= FLAG_H_MASK;
   }

   if(extended_sum > 0xFFFF)
   {
      register_f |= FLAG_C_MASK;
   }
}

static void
inc16_bytes(unsigned char *high, unsigned char *low)
{
   unsigned short value = ((unsigned short)high << 8) | ((unsigned short)low & 0xFF);
   value += 1;

   *high = (value >> 8);
   *low  = (value & 0xFF);
}

static void
dec16_bytes(unsigned char *high, unsigned char *low)
{
   unsigned short value = ((unsigned short)high << 8) | ((unsigned short)low & 0xFF);
   value -= 1;

   *high = (value >> 8);
   *low  = (value & 0xFF);
}

static void
inc16(unsigned short *value)
{
   *value += 1;
}

static void
dec16(unsigned short *value)
{
   *value -= 1;
}

static void
jp(unsigned char *stream, bool should_jump)
{
   unsigned char address_low  = stream[register_pc++];
   unsigned char address_high = stream[register_pc++];

   if(should_jump)
   {
      unsigned short address = ((unsigned short)address_high << 8) | ((unsigned short)address_low);
      register_pc = address;
   }
}

static void
jr(unsigned char *stream, bool should_jump)
{
   unsigned char offset = stream[register_pc++];

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
      stream[--register_sp] = address_high;
      stream[--register_sp] = address_low;

      unsigned short address = ((unsigned short)address_high << 8) | ((unsigned short)address_low);
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
      unsigned short address = ((unsigned short)address_high << 8) | ((unsigned short)address_low);
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
rla()
{
   unsigned char previous_bit7 = (register_a >> 7);
   unsigned char previous_c = FLAG_C;

   register_a <<= 1;

   // NOTE(law): Set bit 0 of value to the previous value of the Carry flag
   register_a = (register_a & ~0x01) | (previous_c);

   register_f = 0;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 7.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit7 << FLAG_C_BIT);
}

static void
rlca()
{
   register_a <<= 1;

   register_f = 0;

   // NOTE(law): Set the Carry flag to the value of bit 0 (i.e. pre-shift value
   // of bit 7).
   register_f |= ((register_a & 0x01) << FLAG_C_BIT);
}

static void
rra()
{
   unsigned char previous_bit0 = (register_a & 0x01);
   unsigned char previous_c = FLAG_C;

   register_a >>= 1;

   // NOTE(law): Set bit 7 of value to the previous value of the Carry flag
   register_a = (register_a & ~(1 << 7)) | (previous_c << 7);

   register_f = 0;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 7.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);
}

static void
rrca()
{
   register_a >>= 1;

   register_f = 0;

   // NOTE(law): Set the Carry flag to the value of bit 7 (i.e. pre-shift value
   // of bit 0).
   register_f |= ((register_a >> 7) << FLAG_C_BIT);
}

static void
rl(unsigned char *value)
{
   unsigned char previous_bit7 = (*value >> 7);
   unsigned char previous_c = FLAG_C;

   *value <<= 1;

   // NOTE(law): Set bit 0 of value to the previous value of the Carry flag
   *value = (*value & ~0x01) | (previous_c);

   register_f = 0;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 7.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit7 << FLAG_C_BIT);

   if(*value == 0)
   {
      register_f |= FLAG_Z_MASK;
   }
}

static void
rlc(unsigned char *value)
{
   *value <<= 1;

   register_f = 0;

   // NOTE(law): Set the Carry flag to the value of bit 0 (i.e. pre-shift value
   // of bit 7).
   register_f |= ((*value & 0x01) << FLAG_C_BIT);

   if(*value == 0)
   {
      register_f |= FLAG_Z_MASK;
   }
}

static void
rr(unsigned char *value)
{
   unsigned char previous_bit0 = (*value & 0x01);
   unsigned char previous_c = FLAG_C;

   *value >>= 1;

   // NOTE(law): Set bit 7 of value to the previous value of the Carry flag
   *value = (*value & ~(1 << 7)) | (previous_c << 7);

   register_f = 0;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);

   if(*value == 0)
   {
      register_f |= FLAG_Z_MASK;
   }
}

static void
rrc(unsigned char *value)
{
   *value >>= 1;

   register_f = 0;

   // NOTE(law): Set the Carry flag to the value of bit 7 (i.e. pre-shift value
   // of bit 0).
   register_f |= ((*value >> 7) << FLAG_C_BIT);

   if(*value == 0)
   {
      register_f |= FLAG_Z_MASK;
   }
}

static void
bit(unsigned int bit_index, unsigned char value)
{
   // NOTE(law) Update the Zero flag based on the value of specified bit index
   // of the value.

   unsigned char bit_value = ((value >> bit_index) & 0x01);
   register_f = (register_f & ~(FLAG_Z_MASK)) | (bit_value << FLAG_Z_BIT);

   register_f &= ~FLAG_N_MASK;
   register_f |= FLAG_H_MASK;
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
   unsigned char previous_bit7 = (*value >> 7);

   *value <<= 1;

   register_f = 0;
   if(*value == 0)
   {
      register_f |= FLAG_Z_MASK;
   }

   register_f |= (previous_bit7 << FLAG_C_BIT);
}

static void
sra(unsigned char *value)
{
   unsigned char previous_bit0 = (*value & 0x01);

   // TODO(law): Confirm that casting to a signed value actually produces an
   // arithmetic shift in this case.
   *value = ((signed short)value >> 1);

   register_f = 0;
   if(*value == 0)
   {
      register_f |= FLAG_Z_MASK;
   }

   register_f |= (previous_bit0 << FLAG_C_BIT);
}

static void
swap(unsigned char *value)
{
   unsigned char high_nibble = (*value >> 4);
   unsigned char low_nibble = (*value & 0xF);

   *value = (low_nibble << 4) | (high_nibble & 0xF);
}

static void
srl(unsigned char *value)
{
   unsigned char previous_bit0 = (*value & 0x01);

   // TODO(law): Confirm that using an unsigned value actually produces a
   // logical shift in this case.
   *value >>= 1;

   register_f = 0;
   if(*value == 0)
   {
      register_f |= FLAG_Z_MASK;
   }

   register_f |= (previous_bit0 << FLAG_C_BIT);
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
            printf("UNHANDLED OPCODE CB %x\n", opcode);
            assert(0);
         } break;
      }
   }
   else
   {
      switch(opcode)
      {
         // NOTE(law): 8-bit load instructions
         case 0x40: {register_b = register_b;} break;
         case 0x41: {register_b = register_c;} break;
         case 0x42: {register_b = register_d;} break;
         case 0x43: {register_b = register_e;} break;
         case 0x44: {register_b = register_h;} break;
         case 0x45: {register_b = register_l;} break;
         case 0x46: {register_b = stream[REGISTER_HL];} break;
         case 0x47: {register_b = register_a;} break;

         case 0x48: {register_c = register_b;} break;
         case 0x49: {register_c = register_c;} break;
         case 0x4A: {register_c = register_d;} break;
         case 0x4B: {register_c = register_e;} break;
         case 0x4C: {register_c = register_h;} break;
         case 0x4D: {register_c = register_l;} break;
         case 0x4E: {register_c = stream[REGISTER_HL];} break;
         case 0x4F: {register_c = register_a;} break;

         case 0x50: {register_d = register_b;} break;
         case 0x51: {register_d = register_c;} break;
         case 0x52: {register_d = register_d;} break;
         case 0x53: {register_d = register_e;} break;
         case 0x54: {register_d = register_h;} break;
         case 0x55: {register_d = register_l;} break;
         case 0x56: {register_d = stream[REGISTER_HL];} break;
         case 0x57: {register_d = register_a;} break;

         case 0x58: {register_e = register_b;} break;
         case 0x59: {register_e = register_c;} break;
         case 0x5A: {register_e = register_d;} break;
         case 0x5B: {register_e = register_e;} break;
         case 0x5C: {register_e = register_h;} break;
         case 0x5D: {register_e = register_l;} break;
         case 0x5E: {register_e = stream[REGISTER_HL];} break;
         case 0x5F: {register_e = register_a;} break;

         case 0x60: {register_h = register_b;} break;
         case 0x61: {register_h = register_c;} break;
         case 0x62: {register_h = register_d;} break;
         case 0x63: {register_h = register_e;} break;
         case 0x64: {register_h = register_h;} break;
         case 0x65: {register_h = register_l;} break;
         case 0x66: {register_h = stream[REGISTER_HL];} break;
         case 0x67: {register_h = register_a;} break;

         case 0x68: {register_l = register_b;} break;
         case 0x69: {register_l = register_c;} break;
         case 0x6A: {register_l = register_d;} break;
         case 0x6B: {register_l = register_e;} break;
         case 0x6C: {register_l = register_h;} break;
         case 0x6D: {register_l = register_l;} break;
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
         case 0x7F: {register_a = register_a;} break;

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

         case 0xF0: {register_a = stream[0xFF00 + stream[register_pc++]];} break; // LDH A, (0xFF + n)
         case 0xE0: {stream[0xFF00 + stream[register_pc++]] = register_a;} break; // LDH (0xFF + n), A

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
            signed short address = (signed short)register_sp + offset;
            unsigned char half_sum = (register_sp & 0xF) + (offset & 0xF);

            register_sp = (unsigned short)address;

            register_f = 0;

            if((half_sum & 0x10) == 0x10)
            {
               register_f |= FLAG_H_MASK;
            }

            if(address > 0xFF)
            {
               register_f |= FLAG_C_MASK;
            }
         } break;

         case 0xF8: // LD HL, SP + dd
         {
            signed char offset = stream[register_pc++];
            signed short address = (signed short)register_sp + offset;
            unsigned char half_sum = (register_sp & 0xF) + (offset & 0xF);

            register_h = address >> 8;
            register_l = address & 0xFF;

            register_f = 0;

            if((half_sum & 0x10) == 0x10)
            {
               register_f |= FLAG_H_MASK;
            }

            if(address > 0xFF)
            {
               register_f |= FLAG_C_MASK;
            }
         } break;


         // NOTE(law): CPU Control instructions
         case 0x3F: // CCF
         {
            register_f &= ~FLAG_N_MASK;
            register_f &= ~FLAG_H_MASK;
            if(FLAG_C)
            {
               register_f &= ~FLAG_C_MASK;
            }
            else
            {
               register_f |= FLAG_C_MASK;
            }
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
         case 0xF3: {ime = false;} break;
         case 0xFB: {ime = true;} break;


         // NOTE(law): Jump instructions
         case 0xC3: jp(stream, true); // JP nn
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
            printf("UNHANDLED OPCODE %x\n", opcode);
            assert(0);
         } break;
      }
   }
}
