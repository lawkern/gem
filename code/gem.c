/* /////////////////////////////////////////////////////////////////////////// */
/* (c) copyright 2022 Lawrence D. Kern /////////////////////////////////////// */
/* /////////////////////////////////////////////////////////////////////////// */

#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

typedef  uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef  int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

#define ARRAY_LENGTH(array) (sizeof(array) / sizeof((array)[0]))
#define MAXIMUM(a, b) ((a) > (b) ? (a) : (b))

#define KIBIBYTES(value) (1024LL * (value))
#define MEBIBYTES(value) (1024LL * KIBIBYTES(value))

// NOTE(law): Anything prepended with PLATFORM_ is implemented on a per-platform
// basis - that is, Win32 and Linux implement there own versions of file
// loading, logging, etc with the API defined below.

struct platform_file
{
   size_t size;
   u8 *memory;
};

#define PLATFORM_LOG(name) void name(char *format, ...)
#define PLATFORM_FREE_FILE(name) void name(struct platform_file *file)
#define PLATFORM_LOAD_FILE(name) struct platform_file name(char *file_path)

static PLATFORM_LOG(platform_log);
static PLATFORM_FREE_FILE(platform_free_file);
static PLATFORM_LOAD_FILE(platform_load_file);

#define TAU 6.2831853071f

#define CPU_HZ 4194304
#define VERTICAL_SYNC_HZ 59.73f
#define HORIZONTAL_SYNC_HZ 9198

struct memory_arena
{
   u8 *base_address;
   size_t size;
   size_t used;
};

struct memory_bank
{
   size_t size;
   u8 *memory;
};

enum memory_banking_mode
{
   MEMORY_BANKING_MODE_SIMPLE   = 0x00,
   MEMORY_BANKING_MODE_ADVANCED = 0x01,
};

enum memory_bank_controller
{
   MEMORY_BANK_CONTROLLER_NONE,
   MEMORY_BANK_CONTROLLER_MBC1,
   MEMORY_BANK_CONTROLLER_MBC2,
};

static struct
{
   struct memory_bank rom_banks[512];
   struct memory_bank ram_banks[16];

   u32 rom_bank_count;
   u32 ram_bank_count;

   u8 rom_bank_mask;

   u32 rom_selected_index;
   u32 ram_selected_index;

   enum memory_banking_mode banking_mode;
   u8 upper_rom_bank_bits;

   struct memory_bank vram;
   struct memory_bank wram;
   struct memory_bank oam;
   struct memory_bank io;
   struct memory_bank hram;

   u8 register_ie;

   enum memory_bank_controller mbc;

   bool load_complete;
   bool boot_complete;
   bool ram_enabled;
} map;

static struct
{
   u8 a;
   u8 b;
   u8 c;
   u8 d;
   u8 e;
   u8 f;
   u8 h;
   u8 l;

   u16 pc;
   u16 sp;
} registers;

static bool halt;
static bool stop;
static bool ime;

#define REGISTER_BC (((u16)registers.b << 8) | (u16)registers.c)
#define REGISTER_DE (((u16)registers.d << 8) | (u16)registers.e)
#define REGISTER_HL (((u16)registers.h << 8) | (u16)registers.l)
#define REGISTER_AF (((u16)registers.a << 8) | (u16)registers.f)

#define REGISTER_IE_ADDRESS 0xFFFF
#define REGISTER_IF_ADDRESS 0xFF0F

#define FLAG_Z_BIT 7
#define FLAG_N_BIT 6
#define FLAG_H_BIT 5
#define FLAG_C_BIT 4

#define FLAG_Z_MASK (1 << FLAG_Z_BIT)
#define FLAG_N_MASK (1 << FLAG_N_BIT)
#define FLAG_H_MASK (1 << FLAG_H_BIT)
#define FLAG_C_MASK (1 << FLAG_C_BIT)

#define FLAG_Z ((registers.f >> FLAG_Z_BIT) & 0x1)
#define FLAG_N ((registers.f >> FLAG_N_BIT) & 0x1)
#define FLAG_H ((registers.f >> FLAG_H_BIT) & 0x1)
#define FLAG_C ((registers.f >> FLAG_C_BIT) & 0x1)

static u8 boot_rom[] =
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

static void *
allocate(struct memory_arena *arena, size_t size)
{
   assert(size <= (arena->size - arena->used));

   void *result = arena->base_address + arena->used;
   arena->used += size;

   return(result);
}

static void
reset_arena(struct memory_arena *arena)
{
   arena->used = 0;
}

static void
zero_memory(void *memory, size_t size)
{
   // TODO(law): Speed this up!

   u8 *byte = memory;
   while(size--)
   {
      *byte++ = 0;
   }
}

static void
copy_memory(void *destination, void *source, size_t size)
{
   // TODO(law): Speed this up!

   u8 *destination_byte = destination;
   u8 *source_byte = source;
   while(size--)
   {
      *destination_byte++ = *source_byte++;
   }
}

static u16
endian_swap16(u16 value)
{
   // TODO(law): For now, we're just assuming the host machine is little
   // endian. In the future, it might be worthwhile to test endianess first and
   // allow the values to pass through unmodified if this ever runs on a big
   // endian machine (i.e. the same byte order of the Game Boy).

   u16 result = ((value << 8) & 0xFF00) | ((value >> 8) & 0x00FF);
   return(result);
}

#pragma pack(push, 1)
struct cartridge_header
{
   // NOTE(law): The format of the cartidge header is referenced from:
   // https://gbdev.io/pandocs/The_Cartridge_Header.html

   u32 entry_point;
   u8 logo[48];

   union
   {
      char title[16];
      struct
      {
         char short_title[11];
         char manufacturer_code[4];
         u8 cgb_flag;
      };
   };

   char new_licensee_code[2];
   u8 sgb_flag;
   u8 cartridge_type;
   u8 rom_size;
   u8 ram_size;
   u8 destination_code;
   u8 old_licensee_code;
   u8 mask_rom_version_number;
   u8 header_checksum;
   u16 global_checksum;
};
#pragma pack(pop)

static u8
read_memory(u16 address)
{
   // TODO(law): Condense this down once everything is working.

   u8 result = 0x00;

   if(address <= 0x3FFF)
   {
      // NOTE(law): ROM bank 00 (0000 - 3FFF).
      if(map.boot_complete || address >= 0x100)
      {
         u8 bank_index = 0;
         if(map.banking_mode == MEMORY_BANKING_MODE_ADVANCED)
         {
            if(map.rom_bank_count > map.upper_rom_bank_bits)
            {
               map.rom_selected_index = map.upper_rom_bank_bits;
            }
         }

         result = map.rom_banks[bank_index].memory[address];
      }
      else
      {
         result = boot_rom[address];
      }
   }
   else if(address <= 0x7FFF)
   {
      // NOTE(law): ROM bank 01-NN (4000 - 7FFF).
      u32 selected_rom_index = MAXIMUM(map.rom_selected_index, 1);
      result = map.rom_banks[selected_rom_index].memory[address - 0x4000];
   }
   else if(address <= 0x9FFF)
   {
      // NOTE(law): VRAM (8000 - 9FFF).
      result = map.vram.memory[address - 0x8000];
   }
   else if(address <= 0xBFFF)
   {
      // NOTE(law): External RAM banks (A000 - BFFF).
      if(map.ram_enabled)
      {
         if(map.mbc == MEMORY_BANK_CONTROLLER_MBC2 && address >= 0xA200)
         {
            // NOTE(law): A200 - BFFF mirrors 0xA000-A1FF on MBC2 cartridges.
            address -= 0x200;
         }

         result = map.rom_banks[map.ram_selected_index].memory[address - 0xA000];
      }
      else
      {
         result = 0xFF;
      }
   }
   else if(address <= 0xDFFF)
   {
      // NOTE(law): WRAM (C000 - CFFF, D000 - DFFF).
      result = map.wram.memory[address - 0xC000];
   }
   else if(address <= 0xFDFF)
   {
      // NOTE(law): Mirror of C000-DDFF (E000 - FDFF).
      result = map.wram.memory[address - 0xC000 - 0x2000];
   }
   else if(address <= 0xFE9F)
   {
      // NOTE(law): OAM sprite attribute table (FE00 - FE9F).
      result = map.oam.memory[address - 0xFE00];
   }
   else if(address <= 0xFEFF)
   {
      // NOTE(law): Not usable (FFA0 - FEFF)

      // TODO(law): Accessing this address range is not permitted. Implement the
      // weird hardware-specific behavior, like OAM corruption.
      result = 0xFF;
   }
   else if(address <= 0xFF7F)
   {
      // NOTE(law): I/O registers (FF00 - FF7F).
      result = map.io.memory[address - 0xFF00];
   }
   else if(address <= 0xFFFE)
   {
      // NOTE(law): HRAM (FF80 - FFFE)
      result = map.hram.memory[address - 0xFF80];
   }
   else if(address == 0xFFFF)
   {
      result = map.register_ie;
   }

   return(result);
}

static void
write_memory(u16 address, u8 value)
{
   // TODO(law): Condense this down once everything is working.

   if(map.mbc == MEMORY_BANK_CONTROLLER_MBC2 && address <= 0x3999)
   {
      if((address >> 8) & 0x1)
      {
         // NOTE(law): Select MBC2 ROM bank number.
         map.rom_selected_index = MAXIMUM(value & 0x0F, 1);
      }
      else
      {
         // NOTE(law): Enable MBC2 RAM.
         map.ram_enabled = ((value >> 4) == 0xA);
      }
   }
   else if(address <= 0x1FFF)
   {
      // NOTE(law): Enable RAM (0 - 1FFF).
      map.ram_enabled = ((value >> 4) == 0xA);
   }
   else if(address <= 0x3FFF)
   {
      // NOTE(law): Select ROM bank (2000 - 3FFF).
      map.rom_selected_index = MAXIMUM(value & map.rom_bank_mask, 1);
      if(map.banking_mode == MEMORY_BANKING_MODE_ADVANCED)
      {
         u32 extended_index = map.rom_selected_index + map.upper_rom_bank_bits;
         if(map.rom_bank_count > extended_index)
         {
            map.rom_selected_index = extended_index;
         }
      }
   }
   else if(address <= 0x5FFF)
   {
      // NOTE(law): Select RAM bank/upper ROM bank bits (4000 - 5FFF).
      value &= 0x3;
      if(map.banking_mode == MEMORY_BANKING_MODE_SIMPLE)
      {
         if((u32)value < map.ram_bank_count)
         {
            map.ram_selected_index = (u32)value;
         }
      }
      else
      {
         if((u32)value < map.rom_bank_count)
         {
            // TODO(law): This calculation is handled differently for MBC1M
            // cartridges - it only shifts up by 4.
            map.upper_rom_bank_bits = (u32)value << 5;
            map.rom_selected_index = value;
         }
      }
   }
   else if(address <= 0x7FFF)
   {
      // NOTE(law): Select banking mode (6000 - 7FFF).

      // TODO(law): Determine if the value MUST be 0 or 1, or if it can be
      // truncated.

      map.banking_mode = (value & 0x1);
   }
   else if(address <= 0x9FFF)
   {
      // NOTE(law): VRAM (8000 - 9FFF).
      map.vram.memory[address - 0x8000] = value;
   }
   else if(address <= 0xBFFF)
   {
      // NOTE(law): External RAM banks (A000 - BFFF).

      // TODO(law): Does writing to disabled RAM have an effect?
      if(map.ram_enabled)
      {
         if((map.mbc == MEMORY_BANK_CONTROLLER_MBC2) && (address >= 0xA200))
         {
            // NOTE(law): 0xA200-BFFF mirrors 0xA000-A1FF on MBC2 cartridges.
            address -= 0x200;
         }

         map.ram_banks[map.ram_selected_index].memory[address - 0xA000] = value;
      }
   }
   else if(address <= 0xDFFF)
   {
      // NOTE(law): WRAM (C000 - CFFF, D000 - DFFF).
      map.wram.memory[address - 0xC000] = value;
   }
   else if(address <= 0xFDFF)
   {
      // NOTE(law): Mirror of C000-DDFF (E000 - FDFF).
      map.wram.memory[address -0xC000 - 0x2000] = value;
   }
   else if(address <= 0xFE9F)
   {
      // NOTE(law): OAM sprite attribute table (FE00 - FE9F).
      map.oam.memory[address - 0xFE00] = value;
   }
   else if(address <= 0xFEFF)
   {
      // NOTE(law): Not usable (FFA0 - FEFF)

      // TODO(law): Confirm what writes to this area are supposed to do.
      assert(!"WRITE TO UNUSABLE MEMORY LOCATION.");
   }
   else if(address <= 0xFF7F)
   {
      // NOTE(law): I/O registers (FF00 - FF7F).
      map.io.memory[address - 0xFF00] = value;
   }
   else if(address <= 0xFFFE)
   {
      // NOTE(law): HRAM (FF80 - FFFE)
      map.hram.memory[address - 0xFF80] = value;
   }
   else if(address == 0xFFFF)
   {
      map.register_ie = value;
   }
}

static u16
read_memory16(u16 address)
{
   // TODO(law): Confirm the endian-ness here.
   u8 low  = read_memory(address + 0);
   u8 high = read_memory(address + 1);

   u16 result = ((u16)high << 8) | low;
   return(result);
}

static void
write_memory16(u16 address, u16 value)
{
   // TODO(law): Confirm the endian-ness here.
   u8 low  = (value & 0xF);
   u8 high = (value >> 8);

   write_memory(address + 0, low);
   write_memory(address + 1, high);
}

static struct cartridge_header *
get_cartridge_header(u8 *rom_memory)
{
   struct cartridge_header *result = (struct cartridge_header *)(rom_memory + 0x100);
   return(result);
}

static bool
validate_cartridge_header(u8 *rom_memory, size_t rom_size)
{
   struct cartridge_header *header = get_cartridge_header(rom_memory);

   static u8 logo[] =
   {
      0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
      0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
      0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
   };

   // NOTE(law): Confirm that the cartridge stored the Game Boy logo correctly.
   platform_log("Verifying logo...\n");

   assert(ARRAY_LENGTH(logo) == ARRAY_LENGTH(header->logo));
   for(u32 byte_index = 0; byte_index < ARRAY_LENGTH(logo); ++byte_index)
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

   u8 header_checksum = 0;
   for(u16 byte_offset = 0x0134; byte_offset <= 0x014C; ++byte_offset)
   {
      header_checksum = header_checksum - rom_memory[byte_offset] - 1;
   }

   if(header_checksum != header->header_checksum)
   {
      platform_log("ERROR: Computed header checksum did not match value in header. ");
      platform_log("(header: 0x%02X, computed: 0x%02X.\n", header_checksum, header->header_checksum);

      return(false);
   }

   u16 global_checksum = 0;
   for(u32 byte_offset = 0; byte_offset < rom_size; ++byte_offset)
   {
      global_checksum += (u16)rom_memory[byte_offset];
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
void dump_cartridge_header(u8 *stream)
{
   struct cartridge_header *header = get_cartridge_header(stream);

   platform_log("CARTRIDGE HEADER:\n");

   platform_log("  ENTRY POINT: 0x%08x\n", header->entry_point);

   platform_log("  TITLE: %s\n", header->title);
   platform_log("  SGB FLAG: 0x%02X\n", header->sgb_flag);

   static char *cartridge_type_names[] =
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

   // assert(header->cartridge_type < ARRAY_LENGTH(cartridge_type_names));
   platform_log("  CARTRIDGE TYPE: %#x (%s)\n", header->cartridge_type, cartridge_type_names[header->cartridge_type]);
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

static void
allocate_memory_bank(struct memory_arena *arena, struct memory_bank *bank, size_t size)
{
   bank->size = size;
   bank->memory = allocate(arena, size);
}

static void
unload_cartridge(struct memory_arena *arena)
{
   reset_arena(arena);
   zero_memory(&map, sizeof(map));
   zero_memory(&registers, sizeof(registers));
}

static void
load_cartridge(struct memory_arena *arena, char *file_path)
{
   unload_cartridge(arena);

   platform_log("Loading ROM at \"%s\"...\n", file_path);
   struct platform_file rom = platform_load_file(file_path);

   if(!rom.memory)
   {
      platform_log("ERROR: The ROM \"%s\" was not loaded.\n", file_path);
      return;
   }

   if(rom.size < (sizeof(boot_rom) + sizeof(struct cartridge_header)))
   {
      platform_log("ERROR: The file was too small to contain a cartridge header - the ROM was not loaded.\n");
      platform_free_file(&rom);
      return;
   }

   if(!validate_cartridge_header(rom.memory, rom.size))
   {
      platform_log("ERROR: Invalid cartridge header - the ROM was not loaded.\n");
      platform_free_file(&rom);
      return;
   }

   if(rom.size > (arena->size + sizeof(boot_rom)))
   {
      platform_log("ERROR: Oversized ROM file - the ROM was not loaded.\n");
      platform_free_file(&rom);
      return;
   }

   // NOTE(law): Once it seems like we have a valid cartridge, allocate the
   // various memory banks.

   struct cartridge_header *header = get_cartridge_header(rom.memory);

   map.rom_bank_mask = ~(0xFF << (header->rom_size + 1));
   map.rom_bank_count = (2 << header->rom_size);
   for(u32 bank_index = 0; bank_index < map.rom_bank_count; ++bank_index)
   {
      struct memory_bank *bank = map.rom_banks + bank_index;
      allocate_memory_bank(arena, bank, KIBIBYTES(16));
   }

   u32 ram_bank_counts[] = {0, 0, 1, 4, 16, 8};
   map.ram_bank_count = ram_bank_counts[header->ram_size];
   for(u32 bank_index = 0; bank_index < map.ram_bank_count; ++bank_index)
   {
      struct memory_bank *bank = map.ram_banks + bank_index;
      allocate_memory_bank(arena, bank, KIBIBYTES(8));
   }

   allocate_memory_bank(arena, &map.vram, KIBIBYTES(8));
   allocate_memory_bank(arena, &map.wram, KIBIBYTES(8));
   allocate_memory_bank(arena, &map.oam, 0xA0);
   allocate_memory_bank(arena, &map.io, 0x80);
   allocate_memory_bank(arena, &map.hram, 0x7E);

   // TODO(law): Support the remaining cartridge types!
   switch(header->cartridge_type)
   {
      case 0x00: // ROM ONLY
      {
         map.mbc = MEMORY_BANK_CONTROLLER_NONE;
      } break;

      case 0x01: // MCB1
      case 0x02: // MBC1+RAM
      case 0x03: // MCB1+RAM+BATTERY
      {
         map.mbc = MEMORY_BANK_CONTROLLER_MBC1;
      } break;

      case 0x05: // MBC2
      case 0x06: // MBC2+BATTERY
      {
         map.mbc = MEMORY_BANK_CONTROLLER_MBC2;
      } break;

      default:
      {
         assert(!"UNHANDLED CARTRIDGE TYPE");
      } break;
   }

   u8 *source = rom.memory;
   for(u32 bank_index = 0; bank_index < map.rom_bank_count; ++bank_index)
   {
      size_t size = map.rom_banks[bank_index].size;
      copy_memory(map.rom_banks[bank_index].memory, source, size);
      source += size;
   }

   // NOTE(law): Ensure that this byte is set to zero on cartridge load. Setting
   // it to a non-zero value is what unmaps the boot code.
   write_memory(0xFF50, 0x0);

   platform_free_file(&rom);

   map.load_complete = true;
}

static u16
disassemble_instruction(u16 address)
{
   u16 initial_address = address;

   // NOTE(law): Print the address of the current instruction.
   platform_log("0x%04X  ", address);

   u8 opcode = read_memory(address++);
   if(opcode == 0xCB)
   {
      opcode = read_memory(address++);

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

         case 0x06: {platform_log("LD B, 0x%02X          ", read_memory(address++));} break;
         case 0x0E: {platform_log("LD C, 0x%02X          ", read_memory(address++));} break;
         case 0x16: {platform_log("LD D, 0x%02X          ", read_memory(address++));} break;
         case 0x1E: {platform_log("LD E, 0x%02X          ", read_memory(address++));} break;
         case 0x26: {platform_log("LD H, 0x%02X          ", read_memory(address++));} break;
         case 0x2E: {platform_log("LD L, 0x%02X          ", read_memory(address++));} break;
         case 0x36: {platform_log("LD (HL), 0x%02X       ", read_memory(address++));} break;
         case 0x3E: {platform_log("LD A, 0x%02X          ", read_memory(address++));} break;

         case 0x0A: {platform_log("LD A, (BC)          ");} break;
         case 0x1A: {platform_log("LD A, (DE)          ");} break;

         case 0xFA:
         {
            u16 operand = read_memory16(address);
            address += 2;
            platform_log("LD A, (0x%04X)      ", operand);
         } break;

         case 0x02: {platform_log("LD (BC), A          ");} break;
         case 0x12: {platform_log("LD (DE), A          ");} break;

         case 0xEA:
         {
            u16 operand = read_memory16(address);
            address += 2;
            platform_log("LD (0x%04X), A      ", operand);
         } break;

         case 0xF2: {platform_log("LD A, (FF00 + C)    ");} break;
         case 0xE2: {platform_log("LD (FF00 + C), A    ");} break;

         case 0xF0: {platform_log("LD A, (FF00 + 0x%02X) ", read_memory(address++));} break;
         case 0xE0: {platform_log("LD (FF00 + 0x%02X), A ", read_memory(address++));} break;

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
            u16 operand = read_memory16(address);
            address += 2;
            platform_log("LD 0x%04X, SP       ", operand);
         } break;

         case 0x01:
         {
            u16 operand = read_memory16(address);
            address += 2;
            platform_log("LD BC, 0x%04X       ", operand);
         } break;

         case 0x11:
         {
            u16 operand = read_memory16(address);
            address += 2;
            platform_log("LD DE, 0x%04X       ", operand);
         } break;

         case 0x21:
         {
            u16 operand = read_memory16(address);
            address += 2;
            platform_log("LD HL, 0x%04X       ", operand);
         } break;

         case 0x31:
         {
            u16 operand = read_memory16(address);
            address += 2;
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

         case 0xC6: {platform_log("ADD A, 0x%02X         ", read_memory(address++));} break;

         case 0x88: {platform_log("ADC A, B            ");} break;
         case 0x89: {platform_log("ADC A, C            ");} break;
         case 0x8A: {platform_log("ADC A, D            ");} break;
         case 0x8B: {platform_log("ADC A, E            ");} break;
         case 0x8C: {platform_log("ADC A, H            ");} break;
         case 0x8D: {platform_log("ADC A, L            ");} break;
         case 0x8E: {platform_log("ADC A, (HL)         ");} break;
         case 0x8F: {platform_log("ADC A, A            ");} break;

         case 0xCE: {platform_log("ADC A, 0x%02X         ", read_memory(address++));} break;

         case 0x90: {platform_log("SUB A, B            ");} break;
         case 0x91: {platform_log("SUB A, C            ");} break;
         case 0x92: {platform_log("SUB A, D            ");} break;
         case 0x93: {platform_log("SUB A, E            ");} break;
         case 0x94: {platform_log("SUB A, H            ");} break;
         case 0x95: {platform_log("SUB A, L            ");} break;
         case 0x96: {platform_log("SUB A, (HL)         ");} break;
         case 0x97: {platform_log("SUB A               ");} break;

         case 0xD6: {platform_log("SUB 0x%02X            ", read_memory(address++));} break;

         case 0x98: {platform_log("SBC A, B            ");} break;
         case 0x99: {platform_log("SBC A, C            ");} break;
         case 0x9A: {platform_log("SBC A, D            ");} break;
         case 0x9B: {platform_log("SBC A, E            ");} break;
         case 0x9C: {platform_log("SBC A, H            ");} break;
         case 0x9D: {platform_log("SBC A, L            ");} break;
         case 0x9E: {platform_log("SBC A, (HL)         ");} break;
         case 0x9F: {platform_log("SBC A, A            ");} break;

         case 0xDE: {platform_log("SBC A, 0x%02X         ", read_memory(address++));} break;

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

         case 0xEE: {platform_log("XOR 0x%02X            ", read_memory(address++));} break;

         case 0xB0: {platform_log("OR B                ");} break;
         case 0xB1: {platform_log("OR C                ");} break;
         case 0xB2: {platform_log("OR D                ");} break;
         case 0xB3: {platform_log("OR E                ");} break;
         case 0xB4: {platform_log("OR H                ");} break;
         case 0xB5: {platform_log("OR L                ");} break;
         case 0xB6: {platform_log("OR (HL)             ");} break;
         case 0xB7: {platform_log("OR A                ");} break;

         case 0xF6: {platform_log("OR 0x%02X             ", read_memory(address++));} break;

         case 0xA0: {platform_log("AND B               ");} break;
         case 0xA1: {platform_log("AND C               ");} break;
         case 0xA2: {platform_log("AND D               ");} break;
         case 0xA3: {platform_log("AND E               ");} break;
         case 0xA4: {platform_log("AND H               ");} break;
         case 0xA5: {platform_log("AND L               ");} break;
         case 0xA6: {platform_log("AND (HL)            ");} break;
         case 0xA7: {platform_log("AND A               ");} break;

         case 0xE6: {platform_log("AND 0x%02X            ", read_memory(address++));} break;

         case 0xB8: {platform_log("CP B                ");} break;
         case 0xB9: {platform_log("CP C                ");} break;
         case 0xBA: {platform_log("CP D                ");} break;
         case 0xBB: {platform_log("CP E                ");} break;
         case 0xBC: {platform_log("CP H                ");} break;
         case 0xBD: {platform_log("CP L                ");} break;
         case 0xBE: {platform_log("CP (HL)             ");} break;
         case 0xBF: {platform_log("CP A                ");} break;

         case 0xFE: {platform_log("CP 0x%02X             ", read_memory(address++));} break;

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

         case 0xE8: {platform_log("ADD SP, 0x%02X        ", read_memory(address++));} break;
         case 0xF8: {platform_log("LD HL, SP + 0x%02X    ", read_memory(address++));} break;


         // NOTE(law): CPU Control instructions
         case 0x3F: {platform_log("CCF                 ");} break;
         case 0x37: {platform_log("SCF                 ");} break;
         case 0x00: {platform_log("NOP                 ");} break;
         case 0x76: {platform_log("HALT                ");} break;
         case 0x10: {platform_log("STOP 0x%02X           ", read_memory(address++));} break;
         case 0xF3: {platform_log("DI                  ");} break;
         case 0xFB: {platform_log("EI                  ");} break;


         // NOTE(law): Jump instructions
         case 0xE9: {platform_log("JP HL               ");} break;

         case 0xC3:
         {
            u16 operand = read_memory16(address);
            address += 2;
            platform_log("JP 0x%04X           ", operand);
         } break;

         case 0xC2:
         {
            u16 operand = read_memory16(address);
            address += 2;
            platform_log("JP NZ, 0x%04X       ", operand);
         } break;

         case 0xCA:
         {
            u16 operand = read_memory16(address);
            address += 2;
            platform_log("JP Z, 0x%04X        ", operand);
         } break;

         case 0xD2:
         {
            u16 operand = read_memory16(address);
            address += 2;
            platform_log("JP NC, 0x%04X       ", operand);
         } break;

         case 0xDA:
         {
            u16 operand = read_memory16(address);
            address += 2;
            platform_log("JP C, 0x%04X        ", operand);
         } break;

         case 0x18: {platform_log("JR PC + 0x%02X        ", read_memory(address++));} break;
         case 0x20: {platform_log("JR NZ, PC + 0x%02X    ", read_memory(address++));} break;
         case 0x28: {platform_log("JR Z, PC + 0x%02X     ", read_memory(address++));} break;
         case 0x30: {platform_log("JR NC, PC + 0x%02X    ", read_memory(address++));} break;
         case 0x38: {platform_log("JR C, PC + 0x%02X     ", read_memory(address++));} break;

         case 0xC4:
         {
            u16 operand = read_memory16(address);
            address += 2;
            platform_log("CALL NZ, 0x%04X     ", operand);
         } break;

         case 0xCC:
         {
            u16 operand = read_memory16(address);
            address += 2;
            platform_log("CALL Z, 0x%04X      ", operand);
         } break;

         case 0xCD:
         {
            u16 operand = read_memory16(address);
            address += 2;
            platform_log("CALL 0x%04X         ", operand);
         } break;

         case 0xD4:
         {
            u16 operand = read_memory16(address);
            address += 2;
            platform_log("CALL NC, 0x%04X     ", operand);
         } break;

         case 0xDC:
         {
            u16 operand = read_memory16(address);
            address += 2;
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
   for(u16 index = initial_address; index < address; ++index)
   {
      platform_log("%02X ", read_memory(index));
   }

   platform_log("\n");

   u16 result = address - initial_address;
   return(result);
}

static void
disassemble_stream(u16 address, u32 byte_count)
{
   // TODO(law): 16-bit operations are not being endian swapped in this
   // function.

   u32 start = address;
   while((address - start) < byte_count)
   {
      address += disassemble_instruction(address);
   }
}

static void
add(u8 value)
{
   // NOTE(law): Compute these values before updating register A for use in the
   // flag calculations.
   u16 extended_sum = (u16)registers.a + (u16)value;
   u8 half_sum = (registers.a & 0xF) + (value & 0xF);

   registers.a = (u8)extended_sum;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((registers.a == 0) << FLAG_Z_BIT);

   // NOTE(law): Always reset the Subtraction flag.
   registers.f &= ~FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag if a carry occurs from bit 3 to 4.
   registers.f = (registers.f & ~FLAG_H_MASK) | (((half_sum & 0x10) == 0x10) << FLAG_H_BIT);

   // NOTE(law): Set the Half Carry flag if a carry occurs from bit 7.
   registers.f = (registers.f & ~FLAG_C_MASK) | ((extended_sum > 0xFF) << FLAG_C_BIT);
}

static void
adc(u8 value)
{
   // NOTE(law): Compute these values before updating register A for use in the
   // flag calculations.
   u16 extended_sum = (u16)registers.a + (u16)value + FLAG_C;
   u8 half_sum = (registers.a & 0xF) + (value & 0xF) + FLAG_C;

   registers.a = (u8)extended_sum;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((registers.a == 0) << FLAG_Z_BIT);

   // NOTE(law): Always reset the Subtraction flag.
   registers.f &= ~FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag if a carry occurs from bit 3 to 4.
   registers.f = (registers.f & ~FLAG_H_MASK) | (((half_sum & 0x10) == 0x10) << FLAG_H_BIT);

   // NOTE(law): Set the Carry flag if adding the full 8 bits of register A and
   // the incoming value would create a sum greater than the maximum byte value
   // 0xFF (assuming the types used avoided an overflow).
   registers.f = (registers.f & ~FLAG_C_MASK) | ((extended_sum > 0xFF) << FLAG_C_BIT);
}

static void
sub(u8 value)
{
   // NOTE(law): Compute these values before updating register A for use in the
   // flag calculations.
   bool is_negative = (s8)registers.a < (s8)value;
   bool is_half_negative = (s8)(registers.a & 0xF) < (s8)(value & 0xF);

   registers.a -= value;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((registers.a == 0) << FLAG_Z_BIT);

   // NOTE(law): Always set the Subtraction flag.
   registers.f |= FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag if subractring the low 4 bits of register A
   // and the incoming value sets bit 4 of the resulting sum.
   registers.f = (registers.f & ~FLAG_H_MASK) | (is_half_negative << FLAG_H_BIT);

   // NOTE(law): Set the Carry flag if the result is less than zero.
   registers.f = (registers.f & ~FLAG_C_MASK) | (is_negative << FLAG_C_BIT);
}

static void
sbc(u8 value)
{
   s8 value_and_carry = (s8)value + FLAG_C;

   bool is_negative = (s8)registers.a < value_and_carry;
   bool is_half_negative = (s8)(registers.a & 0xF) < (value_and_carry & 0xF);

   registers.a -= (value);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((registers.a == 0) << FLAG_Z_BIT);

   // NOTE(law): Always set the Subtraction flag.
   registers.f |= FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag if subractring the low 4 bits of register A
   // and the incoming value sets bit 4 of the resulting sum.
   registers.f = (registers.f & ~FLAG_H_MASK) | (is_half_negative << FLAG_H_BIT);

   // NOTE(law): Set the Carry flag if the result is less than zero.
   registers.f = (registers.f & ~FLAG_C_MASK) | (is_negative << FLAG_C_BIT);
}

static void
xor(u8 value)
{
   registers.a ^= value;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((registers.a == 0) << FLAG_Z_BIT);

   // NOTE(law): Always reset the Subtraction mask.
   registers.f &= ~FLAG_N_MASK;

   // NOTE(law): Always reset the Half Carry mask.
   registers.f &= ~FLAG_H_MASK;

   // NOTE(law): Always reset the Carry mask.
   registers.f &= ~FLAG_C_MASK;
}

static void
or(u8 value)
{
   registers.a |= value;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((registers.a == 0) << FLAG_Z_BIT);

   // NOTE(law): Always reset the Subtraction mask.
   registers.f &= ~FLAG_N_MASK;

   // NOTE(law): Always reset the Half Carry mask.
   registers.f &= ~FLAG_H_MASK;

   // NOTE(law): Always reset the Carry mask.
   registers.f &= ~FLAG_C_MASK;
}

static void
and(u8 value)
{
   registers.a &= value;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((registers.a == 0) << FLAG_Z_BIT);

   // NOTE(law): Always reset the Subtraction mask.
   registers.f &= ~FLAG_N_MASK;

   // NOTE(law): Always set the Half Carry mask.
   registers.f |= FLAG_H_MASK;

   // NOTE(law): Always reset the Carry mask.
   registers.f &= ~FLAG_C_MASK;
}

static void
cp(u8 value)
{
   // NOTE(law): If the compared values are equivalent, set the Zero flag.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((registers.a == value) << FLAG_Z_BIT);

   // NOTE(law): Always set the subtraction flag for comparisons.
   registers.f |= FLAG_N_MASK;

   // NOTE(law): If the result of subtracting the first 4 bits of value from the
   // first 4 bits of A would produce a result that is less than 0, set the
   // Half Carry flag.
   bool is_half_negative = (s8)(registers.a & 0xF) < (s8)(value & 0xF);
   registers.f = (registers.f & ~FLAG_H_MASK) | (is_half_negative << FLAG_H_BIT);

   // NOTE(law): If the result of subtracting value from A would produce a
   // result that is less than 0, set the Carry flag.
   bool is_negative = (s8)registers.a < (s8)value;
   registers.f = (registers.f & ~FLAG_C_MASK) | (is_negative << FLAG_C_BIT);
}

static u8
inc(u8 value)
{
   // NOTE(law): Increment

   u8 half_sum = (value & 0xF) + 1;

   value += 1;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   registers.f &= ~FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag when a carry from bit 3 occurs.
   registers.f = (registers.f & ~FLAG_H_MASK) | (((half_sum & 0x10) == 0x10) << FLAG_H_BIT);

   // NOTE(law): The Carry flag is not affected.

   return(value);
}

static u8
dec(u8 value)
{
   // NOTE(law): Decrement

   bool is_half_negative = (s8)(value & 0xF) < 1;

   value -= 1;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always set.
   registers.f |= FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag when a carry from bit 3 occurs.
   registers.f = (registers.f & ~FLAG_H_MASK) | (is_half_negative << FLAG_H_BIT);

   // NOTE(law): The Carry flag is not affected.

   return(value);
}

static void
add16(u16 value)
{
   u32 extended_sum = (u32)REGISTER_HL + (u32)value;
   u8 half_sum = (REGISTER_HL & 0xF) + (value & 0xF);

   u16 sum = REGISTER_HL + value;
   registers.h = (sum >> 8);
   registers.l = (sum & 0xFF);

   // NOTE(law): The Zero flag is not affected.

   // NOTE(law): The Subtraction flag is always unset.
   registers.f &= ~FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag when a carry from bit 3 occurs.
   registers.f = (registers.f & ~FLAG_H_MASK) | (((half_sum & 0x10) == 0x10) << FLAG_H_BIT);

   registers.f = (registers.f & ~FLAG_C_MASK) | ((extended_sum > 0xFFFF) << FLAG_C_BIT);
}

static void
inc16_bytes(u8 *high, u8 *low)
{
   u16 value = ((u16)*high << 8) | ((u16)*low & 0xFF);
   value += 1;

   *high = (value >> 8);
   *low  = (value & 0xFF);


   // NOTE(law): The flags are not affected by 16-bit increments/decrements.
}

static void
dec16_bytes(u8 *high, u8 *low)
{
   u16 value = ((u16)*high << 8) | (u16)*low;
   value -= 1;

   *high = (value >> 8);
   *low  = (value & 0xFF);

   // NOTE(law): The flags are not affected by 16-bit increments/decrements.
}

static void
inc16(u16 *value)
{
   *value += 1;

   // NOTE(law): The flags are not affected by 16-bit increments/decrements.
}

static void
dec16(u16 *value)
{
   *value -= 1;

   // NOTE(law): The flags are not affected by 16-bit increments/decrements.
}

static void
jp(bool should_jump)
{
   u8 address_low  = read_memory(registers.pc++);
   u8 address_high = read_memory(registers.pc++);

   if(should_jump)
   {
      u16 address = ((u16)address_high << 8) | (u16)address_low;
      registers.pc = address;
   }
}

static void
jr(bool should_jump)
{
   s8 offset = read_memory(registers.pc++);

   if(should_jump)
   {
      s16 address = (s16)registers.pc + (s16)offset;
      registers.pc = (u16)address;
   }
}

static void
call(bool should_jump)
{
   u8 address_low  = read_memory(registers.pc++);
   u8 address_high = read_memory(registers.pc++);

   if(should_jump)
   {
      write_memory(--registers.sp, registers.pc >> 8);
      write_memory(--registers.sp, registers.pc & 0xFF);

      u16 address = ((u16)address_high << 8) | (u16)address_low;
      registers.pc = address;
   }
}

static void
ret(bool should_jump)
{
   u8 address_low  = read_memory(registers.sp++);
   u8 address_high = read_memory(registers.sp++);

   if(should_jump)
   {
      u16 address = ((u16)address_high << 8) | (u16)address_low;
      registers.pc = address;
   }
}

static void
rst(u8 address_low)
{
   write_memory(--registers.sp, registers.pc >> 8);
   write_memory(--registers.sp, registers.pc & 0xFF);

   u16 address = (u16)address_low;
   registers.pc = address;
}

static u8
rl(u8 value)
{
   // NOTE(law): Rotate Left

   u8 previous_bit7 = (value >> 7);
   u8 previous_c = FLAG_C;

   value <<= 1;

   // NOTE(law): Set bit 0 to the pre-shift value of the Carry flag (a value of zero
   // should have already been shifted into position zero).
   value |= (previous_c << 0);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Half Carry flag is always reset.
   registers.f &= ~FLAG_N_MASK;

   // NOTE(law): The Subtraction flag is always reset.
   registers.f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 7.
   registers.f = (registers.f & ~FLAG_C_MASK) | (previous_bit7 << FLAG_C_BIT);

   return(value);
}

static void
rla()
{
   // NOTE(law): Rotate Left Accumulator

   u8 previous_bit7 = (registers.a >> 7);
   u8 previous_c = FLAG_C;

   registers.a <<= 1;

   // NOTE(law): Set bit 0 of value to the previous value of the Carry flag (a
   // value of zero should have already been shifted into position zero).
   registers.a |= (previous_c << 0);

   // NOTE(law): The Zero flag is always reset.
   registers.f &= ~FLAG_Z_MASK;

   // NOTE(law): The Subtraction flag is always reset.
   registers.f &= ~FLAG_N_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   registers.f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 7.
   registers.f = (registers.f & ~FLAG_C_MASK) | (previous_bit7 << FLAG_C_BIT);
}

static u8
rlc(u8 value)
{
   // NOTE(law): Rotate Left Circular

   u8 previous_bit7 = (value >> 7);

   value <<= 1;

   // NOTE(law): Set bit 0 to the pre-shift value of bit 7 (a value of zero
   // should have already been shifted into position zero).
   value |= (previous_bit7 << 0);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   registers.f &= ~FLAG_N_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   registers.f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 7.
   registers.f = (registers.f & ~FLAG_C_MASK) | (previous_bit7 << FLAG_C_BIT);

   return(value);
}

static void
rlca()
{
   // NOTE(law): Rotate Left Circular Accumulator

   u8 previous_bit7 = (registers.a >> 7);

   registers.a <<= 1;

   // NOTE(law): Set bit 0 to the pre-shift value of bit 7 (a value of zero
   // should have already been shifted into position zero).
   registers.a |= (previous_bit7 << 0);

   // NOTE(law): The Zero flag is always reset.
   registers.f &= ~FLAG_Z_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   registers.f &= ~FLAG_N_MASK;

   // NOTE(law): The Subtraction flag is always reset.
   registers.f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 7.
   registers.f = (registers.f & ~FLAG_C_MASK) | (previous_bit7 << FLAG_C_BIT);
}

static u8
rr(u8 value)
{
   // NOTE(law): Rotate Right

   u8 previous_bit0 = (value & 0x01);
   u8 previous_c = FLAG_C;

   value >>= 1;

   // NOTE(law): Set bit 7 to the pre-shift value of the Carry flag (a value of zero
   // should have already been shifted into position seven).
   value |= (previous_c << 7);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Half Carry flag is always reset.
   registers.f &= ~FLAG_N_MASK;

   // NOTE(law): The Subtraction flag is always reset.
   registers.f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   registers.f = (registers.f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);

   return(value);
}

static void
rra()
{
   // NOTE(law): Rotate Right Accumulator

   u8 previous_bit0 = (registers.a & 0x01);
   u8 previous_c = FLAG_C;

   registers.a >>= 1;

   // NOTE(law): Set bit 7 of value to the previous value of the Carry flag (a
   // value of zero should have already been shifted into position seven).
   registers.a |= (previous_c << 7);

   // NOTE(law): The Zero flag is not affected.

   // NOTE(law): The Subtraction flag is always reset.
   registers.f &= ~FLAG_N_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   registers.f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   registers.f = (registers.f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);
}

static u8
rrc(u8 value)
{
   // NOTE(law): Rotate Right Circular

   u8 previous_bit0 = (value & 0x01);

   value >>= 1;

   // NOTE(law): Set bit 7 to the pre-shift value of bit 0 (a value of zero
   // should have already been shifted into position seven).
   value |= (previous_bit0 << 7);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Half Carry flag is always reset.
   registers.f &= ~FLAG_H_MASK;

   // NOTE(law): The Subtraction flag is always reset.
   registers.f &= ~FLAG_N_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   registers.f = (registers.f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);

   return(value);
}

static void
rrca()
{
   // NOTE(law): Rotate Right Circular Accumulator

   u8 previous_bit0 = (registers.a & 0x01);

   registers.a >>= 1;

   // NOTE(law): Set bit 7 to the pre-shift value of bit 0 (a value of zero
   // should have already been shifted into position seven).
   registers.a |= (previous_bit0 << 7);

   // NOTE(law): The Zero flag is always reset.
   registers.f &= ~FLAG_Z_MASK;

   // NOTE(law): The Subtraction flag is always reset.
   registers.f &= ~FLAG_N_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   registers.f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   registers.f = (registers.f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);
}

static void
bit(u32 bit_index, u8 value)
{
   // NOTE(law) Update the Zero flag based on the value of specified bit index
   // of the value. If the bit is zero, set the flag, else reset it.

   u8 bit_value = ((value >> bit_index) & 0x01);
   registers.f = (registers.f & ~(FLAG_Z_MASK)) | ((bit_value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   registers.f &= ~FLAG_N_MASK;

   // NOTE(law): The Half Carry flag is always set.
   registers.f |= FLAG_H_MASK;

   // NOTE(law): The Carry flag is not affected.
}

static u8
set(u32 bit_index, u8 value)
{
   value |= (1 << bit_index);
   return(value);
}

static u8
res(u32 bit_index, u8 value)
{
   value &= ~(1 << bit_index);
   return(value);
}

static u8
sla(u8 value)
{
   // NOTE(law): Shift Left Arithmetic

   u8 previous_bit7 = (value >> 7);

   value <<= 1;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   registers.f &= ~FLAG_H_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   registers.f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 7.
   registers.f = (registers.f & ~FLAG_C_MASK) | (previous_bit7 << FLAG_C_BIT);

   return(value);
}

static u8
sra(u8 value)
{
   // NOTE(law): Shift Right Arithmetic

   u8 previous_bit0 = (value & 0x01);

   // TODO(law): Confirm that casting to a signed value actually produces an
   // arithmetic shift in this case.
   value = ((s16)value >> 1);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   registers.f &= ~FLAG_H_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   registers.f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   registers.f = (registers.f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);

   return(value);
}

static u8
swap(u8 value)
{
   u8 high_nibble = (value >> 4);
   u8 low_nibble = (value & 0xF);

   value = (low_nibble << 4) | high_nibble;
   return(value);
}

static u8
srl(u8 value)
{
   // NOTE(law): Shift Right Logical

   u8 previous_bit0 = (value & 0x01);

   // TODO(law): Confirm that using an unsigned value actually produces a
   // logical shift in this case.
   value >>= 1;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   registers.f = (registers.f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   registers.f &= ~FLAG_H_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   registers.f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   registers.f = (registers.f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);

   return(value);
}

struct opcode_cycle_count
{
   u32 count;
   u32 failed_condition_count;
};

struct opcode_cycle_count nonprefix_cycle_counts[] =
{
   [0x00] = {4},      [0x01] = {12}, [0x02] = {8},      [0x03] = {8},
   [0x04] = {4},      [0x05] = {4},  [0x06] = {8},      [0x07] = {4},
   [0x08] = {20},     [0x09] = {8},  [0x0A] = {8},      [0x0B] = {8},
   [0x0C] = {4},      [0x0D] = {4},  [0x0E] = {8},      [0x0F] = {4},
   [0x10] = {4},      [0x11] = {12}, [0x12] = {8},      [0x13] = {8},
   [0x14] = {4},      [0x15] = {4},  [0x16] = {8},      [0x17] = {4},
   [0x18] = {12},     [0x19] = {8},  [0x1A] = {8},      [0x1B] = {8},
   [0x1C] = {4},      [0x1D] = {4},  [0x1E] = {8},      [0x1F] = {4},
   [0x20] = {12, 8},  [0x21] = {12}, [0x22] = {8},      [0x23] = {8},
   [0x24] = {4},      [0x25] = {4},  [0x26] = {8},      [0x27] = {4},
   [0x28] = {12, 8},  [0x29] = {8},  [0x2A] = {8},      [0x2B] = {8},
   [0x2C] = {4},      [0x2D] = {4},  [0x2E] = {8},      [0x2F] = {4},
   [0x30] = {12, 8},  [0x31] = {12}, [0x32] = {8},      [0x33] = {8},
   [0x34] = {12},     [0x35] = {12}, [0x36] = {12},     [0x37] = {4},
   [0x38] = {12, 8},  [0x39] = {8},  [0x3A] = {8},      [0x3B] = {8},
   [0x3C] = {4},      [0x3D] = {4},  [0x3E] = {8},      [0x3F] = {4},
   [0x40] = {4},      [0x41] = {4},  [0x42] = {4},      [0x43] = {4},
   [0x44] = {4},      [0x45] = {4},  [0x46] = {8},      [0x47] = {4},
   [0x48] = {4},      [0x49] = {4},  [0x4A] = {4},      [0x4B] = {4},
   [0x4C] = {4},      [0x4D] = {4},  [0x4E] = {8},      [0x4F] = {4},
   [0x50] = {4},      [0x51] = {4},  [0x52] = {4},      [0x53] = {4},
   [0x54] = {4},      [0x55] = {4},  [0x56] = {8},      [0x57] = {4},
   [0x58] = {4},      [0x59] = {4},  [0x5A] = {4},      [0x5B] = {4},
   [0x5C] = {4},      [0x5D] = {4},  [0x5E] = {8},      [0x5F] = {4},
   [0x60] = {4},      [0x61] = {4},  [0x62] = {4},      [0x63] = {4},
   [0x64] = {4},      [0x65] = {4},  [0x66] = {8},      [0x67] = {4},
   [0x68] = {4},      [0x69] = {4},  [0x6A] = {4},      [0x6B] = {4},
   [0x6C] = {4},      [0x6D] = {4},  [0x6E] = {8},      [0x6F] = {4},
   [0x70] = {8},      [0x71] = {8},  [0x72] = {8},      [0x73] = {8},
   [0x74] = {8},      [0x75] = {8},  [0x76] = {4},      [0x77] = {8},
   [0x78] = {4},      [0x79] = {4},  [0x7A] = {4},      [0x7B] = {4},
   [0x7C] = {4},      [0x7D] = {4},  [0x7E] = {8},      [0x7F] = {4},
   [0x80] = {4},      [0x81] = {4},  [0x82] = {4},      [0x83] = {4},
   [0x84] = {4},      [0x85] = {4},  [0x86] = {8},      [0x87] = {4},
   [0x88] = {4},      [0x89] = {4},  [0x8A] = {4},      [0x8B] = {4},
   [0x8C] = {4},      [0x8D] = {4},  [0x8E] = {8},      [0x8F] = {4},
   [0x90] = {4},      [0x91] = {4},  [0x92] = {4},      [0x93] = {4},
   [0x94] = {4},      [0x95] = {4},  [0x96] = {8},      [0x97] = {4},
   [0x98] = {4},      [0x99] = {4},  [0x9A] = {4},      [0x9B] = {4},
   [0x9C] = {4},      [0x9D] = {4},  [0x9E] = {8},      [0x9F] = {4},
   [0xA0] = {4},      [0xA1] = {4},  [0xA2] = {4},      [0xA3] = {4},
   [0xA4] = {4},      [0xA5] = {4},  [0xA6] = {8},      [0xA7] = {4},
   [0xA8] = {4},      [0xA9] = {4},  [0xAA] = {4},      [0xAB] = {4},
   [0xAC] = {4},      [0xAD] = {4},  [0xAE] = {8},      [0xAF] = {4},
   [0xB0] = {4},      [0xB1] = {4},  [0xB2] = {4},      [0xB3] = {4},
   [0xB4] = {4},      [0xB5] = {4},  [0xB6] = {8},      [0xB7] = {4},
   [0xB8] = {4},      [0xB9] = {4},  [0xBA] = {4},      [0xBB] = {4},
   [0xBC] = {4},      [0xBD] = {4},  [0xBE] = {8},      [0xBF] = {4},
   [0xC0] = {20, 8},  [0xC1] = {12}, [0xC2] = {16, 12}, [0xC3] = {16},
   [0xC4] = {24, 12}, [0xC5] = {16}, [0xC6] = {8},      [0xC7] = {16},
   [0xC8] = {20, 8},  [0xC9] = {16}, [0xCA] = {16, 12}, [0xCB] = {4},
   [0xCC] = {24, 12}, [0xCD] = {24}, [0xCE] = {8},      [0xCF] = {16},
   [0xD0] = {20, 8},  [0xD1] = {12}, [0xD2] = {16, 12}, [0xD3] = {4},
   [0xD4] = {24, 12}, [0xD5] = {16}, [0xD6] = {8},      [0xD7] = {16},
   [0xD8] = {20, 8},  [0xD9] = {16}, [0xDA] = {16, 12}, [0xDB] = {4},
   [0xDC] = {24, 12}, [0xDD] = {4},  [0xDE] = {8},      [0xDF] = {16},
   [0xE0] = {12},     [0xE1] = {12}, [0xE2] = {8},      [0xE3] = {4},
   [0xE4] = {4},      [0xE5] = {16}, [0xE6] = {8},      [0xE7] = {16},
   [0xE8] = {16},     [0xE9] = {4},  [0xEA] = {16},     [0xEB] = {4},
   [0xEC] = {4},      [0xED] = {4},  [0xEE] = {8},      [0xEF] = {16},
   [0xF0] = {12},     [0xF1] = {12}, [0xF2] = {8},      [0xF3] = {4},
   [0xF4] = {4},      [0xF5] = {16}, [0xF6] = {8},      [0xF7] = {16},
   [0xF8] = {12},     [0xF9] = {8},  [0xFA] = {16},     [0xFB] = {4},
   [0xFC] = {4},      [0xFD] = {4},  [0xFE] = {8},      [0xFF] = {16},
};

struct opcode_cycle_count cbprefix_cycle_counts[] =
{
   [0x00] = {8}, [0x01] = {8}, [0x02] = {8},  [0x03] = {8},
   [0x04] = {8}, [0x05] = {8}, [0x06] = {16}, [0x07] = {8},
   [0x08] = {8}, [0x09] = {8}, [0x0A] = {8},  [0x0B] = {8},
   [0x0C] = {8}, [0x0D] = {8}, [0x0E] = {16}, [0x0F] = {8},
   [0x10] = {8}, [0x11] = {8}, [0x12] = {8},  [0x13] = {8},
   [0x14] = {8}, [0x15] = {8}, [0x16] = {16}, [0x17] = {8},
   [0x18] = {8}, [0x19] = {8}, [0x1A] = {8},  [0x1B] = {8},
   [0x1C] = {8}, [0x1D] = {8}, [0x1E] = {16}, [0x1F] = {8},
   [0x20] = {8}, [0x21] = {8}, [0x22] = {8},  [0x23] = {8},
   [0x24] = {8}, [0x25] = {8}, [0x26] = {16}, [0x27] = {8},
   [0x28] = {8}, [0x29] = {8}, [0x2A] = {8},  [0x2B] = {8},
   [0x2C] = {8}, [0x2D] = {8}, [0x2E] = {16}, [0x2F] = {8},
   [0x30] = {8}, [0x31] = {8}, [0x32] = {8},  [0x33] = {8},
   [0x34] = {8}, [0x35] = {8}, [0x36] = {16}, [0x37] = {8},
   [0x38] = {8}, [0x39] = {8}, [0x3A] = {8},  [0x3B] = {8},
   [0x3C] = {8}, [0x3D] = {8}, [0x3E] = {16}, [0x3F] = {8},
   [0x40] = {8}, [0x41] = {8}, [0x42] = {8},  [0x43] = {8},
   [0x44] = {8}, [0x45] = {8}, [0x46] = {12}, [0x47] = {8},
   [0x48] = {8}, [0x49] = {8}, [0x4A] = {8},  [0x4B] = {8},
   [0x4C] = {8}, [0x4D] = {8}, [0x4E] = {12}, [0x4F] = {8},
   [0x50] = {8}, [0x51] = {8}, [0x52] = {8},  [0x53] = {8},
   [0x54] = {8}, [0x55] = {8}, [0x56] = {12}, [0x57] = {8},
   [0x58] = {8}, [0x59] = {8}, [0x5A] = {8},  [0x5B] = {8},
   [0x5C] = {8}, [0x5D] = {8}, [0x5E] = {12}, [0x5F] = {8},
   [0x60] = {8}, [0x61] = {8}, [0x62] = {8},  [0x63] = {8},
   [0x64] = {8}, [0x65] = {8}, [0x66] = {12}, [0x67] = {8},
   [0x68] = {8}, [0x69] = {8}, [0x6A] = {8},  [0x6B] = {8},
   [0x6C] = {8}, [0x6D] = {8}, [0x6E] = {12}, [0x6F] = {8},
   [0x70] = {8}, [0x71] = {8}, [0x72] = {8},  [0x73] = {8},
   [0x74] = {8}, [0x75] = {8}, [0x76] = {12}, [0x77] = {8},
   [0x78] = {8}, [0x79] = {8}, [0x7A] = {8},  [0x7B] = {8},
   [0x7C] = {8}, [0x7D] = {8}, [0x7E] = {12}, [0x7F] = {8},
   [0x80] = {8}, [0x81] = {8}, [0x82] = {8},  [0x83] = {8},
   [0x84] = {8}, [0x85] = {8}, [0x86] = {16}, [0x87] = {8},
   [0x88] = {8}, [0x89] = {8}, [0x8A] = {8},  [0x8B] = {8},
   [0x8C] = {8}, [0x8D] = {8}, [0x8E] = {16}, [0x8F] = {8},
   [0x90] = {8}, [0x91] = {8}, [0x92] = {8},  [0x93] = {8},
   [0x94] = {8}, [0x95] = {8}, [0x96] = {16}, [0x97] = {8},
   [0x98] = {8}, [0x99] = {8}, [0x9A] = {8},  [0x9B] = {8},
   [0x9C] = {8}, [0x9D] = {8}, [0x9E] = {16}, [0x9F] = {8},
   [0xA0] = {8}, [0xA1] = {8}, [0xA2] = {8},  [0xA3] = {8},
   [0xA4] = {8}, [0xA5] = {8}, [0xA6] = {16}, [0xA7] = {8},
   [0xA8] = {8}, [0xA9] = {8}, [0xAA] = {8},  [0xAB] = {8},
   [0xAC] = {8}, [0xAD] = {8}, [0xAE] = {16}, [0xAF] = {8},
   [0xB0] = {8}, [0xB1] = {8}, [0xB2] = {8},  [0xB3] = {8},
   [0xB4] = {8}, [0xB5] = {8}, [0xB6] = {16}, [0xB7] = {8},
   [0xB8] = {8}, [0xB9] = {8}, [0xBA] = {8},  [0xBB] = {8},
   [0xBC] = {8}, [0xBD] = {8}, [0xBE] = {16}, [0xBF] = {8},
   [0xC0] = {8}, [0xC1] = {8}, [0xC2] = {8},  [0xC3] = {8},
   [0xC4] = {8}, [0xC5] = {8}, [0xC6] = {16}, [0xC7] = {8},
   [0xC8] = {8}, [0xC9] = {8}, [0xCA] = {8},  [0xCB] = {8},
   [0xCC] = {8}, [0xCD] = {8}, [0xCE] = {16}, [0xCF] = {8},
   [0xD0] = {8}, [0xD1] = {8}, [0xD2] = {8},  [0xD3] = {8},
   [0xD4] = {8}, [0xD5] = {8}, [0xD6] = {16}, [0xD7] = {8},
   [0xD8] = {8}, [0xD9] = {8}, [0xDA] = {8},  [0xDB] = {8},
   [0xDC] = {8}, [0xDD] = {8}, [0xDE] = {16}, [0xDF] = {8},
   [0xE0] = {8}, [0xE1] = {8}, [0xE2] = {8},  [0xE3] = {8},
   [0xE4] = {8}, [0xE5] = {8}, [0xE6] = {16}, [0xE7] = {8},
   [0xE8] = {8}, [0xE9] = {8}, [0xEA] = {8},  [0xEB] = {8},
   [0xEC] = {8}, [0xED] = {8}, [0xEE] = {16}, [0xEF] = {8},
   [0xF0] = {8}, [0xF1] = {8}, [0xF2] = {8},  [0xF3] = {8},
   [0xF4] = {8}, [0xF5] = {8}, [0xF6] = {16}, [0xF7] = {8},
   [0xF8] = {8}, [0xF9] = {8}, [0xFA] = {8},  [0xFB] = {8},
   [0xFC] = {8}, [0xFD] = {8}, [0xFE] = {16}, [0xFF] = {8},
};

static u32
fetch_and_execute()
{
   u8 opcode = read_memory(registers.pc++);
   bool condition_succeeded = true;

   bool prefix_opcode = (opcode == 0xCB);
   if(prefix_opcode)
   {
      // NOTE(law): Parse prefix instructions.
      opcode = read_memory(registers.pc++);
      switch(opcode)
      {
         // NOTE(law): Rotate and Shift instructions
         case 0x00: {registers.b = rlc(registers.b);} break;
         case 0x01: {registers.c = rlc(registers.c);} break;
         case 0x02: {registers.d = rlc(registers.d);} break;
         case 0x03: {registers.e = rlc(registers.e);} break;
         case 0x04: {registers.h = rlc(registers.h);} break;
         case 0x05: {registers.l = rlc(registers.l);} break;
         case 0x06: {write_memory(REGISTER_HL, rlc(read_memory(REGISTER_HL)));} break;
         case 0x07: {registers.a = rlc(registers.a);} break;

         case 0x08: {registers.b = rrc(registers.b);} break;
         case 0x09: {registers.c = rrc(registers.c);} break;
         case 0x0A: {registers.d = rrc(registers.d);} break;
         case 0x0B: {registers.e = rrc(registers.e);} break;
         case 0x0C: {registers.h = rrc(registers.h);} break;
         case 0x0D: {registers.l = rrc(registers.l);} break;
         case 0x0E: {write_memory(REGISTER_HL, rrc(read_memory(REGISTER_HL)));} break;
         case 0x0F: {registers.a = rrc(registers.a);} break;

         case 0x10: {registers.b = rl(registers.b);} break;
         case 0x11: {registers.c = rl(registers.c);} break;
         case 0x12: {registers.d = rl(registers.d);} break;
         case 0x13: {registers.e = rl(registers.e);} break;
         case 0x14: {registers.h = rl(registers.h);} break;
         case 0x15: {registers.l = rl(registers.l);} break;
         case 0x16: {write_memory(REGISTER_HL, rl(read_memory(REGISTER_HL)));} break;
         case 0x17: {registers.a = rl(registers.a);} break;

         case 0x18: {registers.b = rr(registers.b);} break;
         case 0x19: {registers.c = rr(registers.c);} break;
         case 0x1A: {registers.d = rr(registers.d);} break;
         case 0x1B: {registers.e = rr(registers.e);} break;
         case 0x1C: {registers.h = rr(registers.h);} break;
         case 0x1D: {registers.l = rr(registers.l);} break;
         case 0x1E: {write_memory(REGISTER_HL, rr(read_memory(REGISTER_HL)));} break;
         case 0x1F: {registers.a = rr(registers.a);} break;

         case 0x20: {registers.b = sla(registers.b);} break;
         case 0x21: {registers.c = sla(registers.c);} break;
         case 0x22: {registers.d = sla(registers.d);} break;
         case 0x23: {registers.e = sla(registers.e);} break;
         case 0x24: {registers.h = sla(registers.h);} break;
         case 0x25: {registers.l = sla(registers.l);} break;
         case 0x26: {write_memory(REGISTER_HL, sla(read_memory(REGISTER_HL)));} break;
         case 0x27: {registers.a = sla(registers.a);} break;

         case 0x28: {registers.b = sra(registers.b);} break;
         case 0x29: {registers.c = sra(registers.c);} break;
         case 0x2A: {registers.d = sra(registers.d);} break;
         case 0x2B: {registers.e = sra(registers.e);} break;
         case 0x2C: {registers.h = sra(registers.h);} break;
         case 0x2D: {registers.l = sra(registers.l);} break;
         case 0x2E: {write_memory(REGISTER_HL, sra(read_memory(REGISTER_HL)));} break;
         case 0x2F: {registers.a = sra(registers.a);} break;

         case 0x30: {registers.b = swap(registers.b);} break;
         case 0x31: {registers.c = swap(registers.c);} break;
         case 0x32: {registers.d = swap(registers.d);} break;
         case 0x33: {registers.e = swap(registers.e);} break;
         case 0x34: {registers.h = swap(registers.h);} break;
         case 0x35: {registers.l = swap(registers.l);} break;
         case 0x36: {write_memory(REGISTER_HL, swap(read_memory(REGISTER_HL)));} break;
         case 0x37: {registers.a = swap(registers.a);} break;

         case 0x38: {registers.b = srl(registers.b);} break;
         case 0x39: {registers.c = srl(registers.c);} break;
         case 0x3A: {registers.d = srl(registers.d);} break;
         case 0x3B: {registers.e = srl(registers.e);} break;
         case 0x3C: {registers.h = srl(registers.h);} break;
         case 0x3D: {registers.l = srl(registers.l);} break;
         case 0x3E: {write_memory(REGISTER_HL, srl(read_memory(REGISTER_HL)));} break;
         case 0x3F: {registers.a = srl(registers.a);} break;


         // NOTE(law): Single-bit Operation instructions
         case 0x40: bit(0, registers.b); break;
         case 0x41: bit(0, registers.c); break;
         case 0x42: bit(0, registers.d); break;
         case 0x43: bit(0, registers.e); break;
         case 0x44: bit(0, registers.h); break;
         case 0x45: bit(0, registers.l); break;
         case 0x46: bit(0, read_memory(REGISTER_HL)); break;
         case 0x47: bit(0, registers.a); break;

         case 0x48: bit(1, registers.b); break;
         case 0x49: bit(1, registers.c); break;
         case 0x4A: bit(1, registers.d); break;
         case 0x4B: bit(1, registers.e); break;
         case 0x4C: bit(1, registers.h); break;
         case 0x4D: bit(1, registers.l); break;
         case 0x4E: bit(1, read_memory(REGISTER_HL)); break;
         case 0x4F: bit(1, registers.a); break;

         case 0x50: bit(2, registers.b); break;
         case 0x51: bit(2, registers.c); break;
         case 0x52: bit(2, registers.d); break;
         case 0x53: bit(2, registers.e); break;
         case 0x54: bit(2, registers.h); break;
         case 0x55: bit(2, registers.l); break;
         case 0x56: bit(2, read_memory(REGISTER_HL)); break;
         case 0x57: bit(2, registers.a); break;

         case 0x58: bit(3, registers.b); break;
         case 0x59: bit(3, registers.c); break;
         case 0x5A: bit(3, registers.d); break;
         case 0x5B: bit(3, registers.e); break;
         case 0x5C: bit(3, registers.h); break;
         case 0x5D: bit(3, registers.l); break;
         case 0x5E: bit(3, read_memory(REGISTER_HL)); break;
         case 0x5F: bit(3, registers.a); break;

         case 0x60: bit(4, registers.b); break;
         case 0x61: bit(4, registers.c); break;
         case 0x62: bit(4, registers.d); break;
         case 0x63: bit(4, registers.e); break;
         case 0x64: bit(4, registers.h); break;
         case 0x65: bit(4, registers.l); break;
         case 0x66: bit(4, read_memory(REGISTER_HL)); break;
         case 0x67: bit(4, registers.a); break;

         case 0x68: bit(5, registers.b); break;
         case 0x69: bit(5, registers.c); break;
         case 0x6A: bit(5, registers.d); break;
         case 0x6B: bit(5, registers.e); break;
         case 0x6C: bit(5, registers.h); break;
         case 0x6D: bit(5, registers.l); break;
         case 0x6E: bit(5, read_memory(REGISTER_HL)); break;
         case 0x6F: bit(5, registers.a); break;

         case 0x70: bit(6, registers.b); break;
         case 0x71: bit(6, registers.c); break;
         case 0x72: bit(6, registers.d); break;
         case 0x73: bit(6, registers.e); break;
         case 0x74: bit(6, registers.h); break;
         case 0x75: bit(6, registers.l); break;
         case 0x76: bit(6, read_memory(REGISTER_HL)); break;
         case 0x77: bit(6, registers.a); break;

         case 0x78: bit(7, registers.b); break;
         case 0x79: bit(7, registers.c); break;
         case 0x7A: bit(7, registers.d); break;
         case 0x7B: bit(7, registers.e); break;
         case 0x7C: bit(7, registers.h); break;
         case 0x7D: bit(7, registers.l); break;
         case 0x7E: bit(7, read_memory(REGISTER_HL)); break;
         case 0x7F: bit(7, registers.a); break;

         case 0x80: {registers.b = res(0, registers.b);} break;
         case 0x81: {registers.c = res(0, registers.c);} break;
         case 0x82: {registers.d = res(0, registers.d);} break;
         case 0x83: {registers.e = res(0, registers.e);} break;
         case 0x84: {registers.h = res(0, registers.h);} break;
         case 0x85: {registers.l = res(0, registers.l);} break;
         case 0x86: {write_memory(REGISTER_HL, res(0, read_memory(REGISTER_HL)));} break;
         case 0x87: {registers.a = res(0, registers.a);} break;

         case 0x88: {registers.b = res(1, registers.b);} break;
         case 0x89: {registers.c = res(1, registers.c);} break;
         case 0x8A: {registers.d = res(1, registers.d);} break;
         case 0x8B: {registers.e = res(1, registers.e);} break;
         case 0x8C: {registers.h = res(1, registers.h);} break;
         case 0x8D: {registers.l = res(1, registers.l);} break;
         case 0x8E: {write_memory(REGISTER_HL, res(1, read_memory(REGISTER_HL)));} break;
         case 0x8F: {registers.a = res(1, registers.a);} break;

         case 0x90: {registers.b = res(2, registers.b);} break;
         case 0x91: {registers.c = res(2, registers.c);} break;
         case 0x92: {registers.d = res(2, registers.d);} break;
         case 0x93: {registers.e = res(2, registers.e);} break;
         case 0x94: {registers.h = res(2, registers.h);} break;
         case 0x95: {registers.l = res(2, registers.l);} break;
         case 0x96: {write_memory(REGISTER_HL, res(2, read_memory(REGISTER_HL)));} break;
         case 0x97: {registers.a = res(2, registers.a);} break;

         case 0x98: {registers.b = res(3, registers.b);} break;
         case 0x99: {registers.c = res(3, registers.c);} break;
         case 0x9A: {registers.d = res(3, registers.d);} break;
         case 0x9B: {registers.e = res(3, registers.e);} break;
         case 0x9C: {registers.h = res(3, registers.h);} break;
         case 0x9D: {registers.l = res(3, registers.l);} break;
         case 0x9E: {write_memory(REGISTER_HL, res(3, read_memory(REGISTER_HL)));} break;
         case 0x9F: {registers.a = res(3, registers.a);} break;

         case 0xA0: {registers.b = res(4, registers.b);} break;
         case 0xA1: {registers.c = res(4, registers.c);} break;
         case 0xA2: {registers.d = res(4, registers.d);} break;
         case 0xA3: {registers.e = res(4, registers.e);} break;
         case 0xA4: {registers.h = res(4, registers.h);} break;
         case 0xA5: {registers.l = res(4, registers.l);} break;
         case 0xA6: {write_memory(REGISTER_HL, res(4, read_memory(REGISTER_HL)));} break;
         case 0xA7: {registers.a = res(4, registers.a);} break;

         case 0xA8: {registers.b = res(5, registers.b);} break;
         case 0xA9: {registers.c = res(5, registers.c);} break;
         case 0xAA: {registers.d = res(5, registers.d);} break;
         case 0xAB: {registers.e = res(5, registers.e);} break;
         case 0xAC: {registers.h = res(5, registers.h);} break;
         case 0xAD: {registers.l = res(5, registers.l);} break;
         case 0xAE: {write_memory(REGISTER_HL, res(5, read_memory(REGISTER_HL)));} break;
         case 0xAF: {registers.a = res(5, registers.a);} break;

         case 0xB0: {registers.b = res(6, registers.b);} break;
         case 0xB1: {registers.c = res(6, registers.c);} break;
         case 0xB2: {registers.d = res(6, registers.d);} break;
         case 0xB3: {registers.e = res(6, registers.e);} break;
         case 0xB4: {registers.h = res(6, registers.h);} break;
         case 0xB5: {registers.l = res(6, registers.l);} break;
         case 0xB6: {write_memory(REGISTER_HL, res(6, read_memory(REGISTER_HL)));} break;
         case 0xB7: {registers.a = res(6, registers.a);} break;

         case 0xB8: {registers.b = res(7, registers.b);} break;
         case 0xB9: {registers.c = res(7, registers.c);} break;
         case 0xBA: {registers.d = res(7, registers.d);} break;
         case 0xBB: {registers.e = res(7, registers.e);} break;
         case 0xBC: {registers.h = res(7, registers.h);} break;
         case 0xBD: {registers.l = res(7, registers.l);} break;
         case 0xBE: {write_memory(REGISTER_HL, res(7, read_memory(REGISTER_HL)));} break;
         case 0xBF: {registers.a = res(7, registers.a);} break;

         case 0xC0: {registers.b = res(0, registers.b);} break;
         case 0xC1: {registers.c = set(0, registers.c);} break;
         case 0xC2: {registers.d = set(0, registers.d);} break;
         case 0xC3: {registers.e = set(0, registers.e);} break;
         case 0xC4: {registers.h = set(0, registers.h);} break;
         case 0xC5: {registers.l = set(0, registers.l);} break;
         case 0xC6: {write_memory(REGISTER_HL, set(0, read_memory(REGISTER_HL)));} break;
         case 0xC7: {registers.a = set(0, registers.a);} break;

         case 0xC8: {registers.b = set(1, registers.b);} break;
         case 0xC9: {registers.c = set(1, registers.c);} break;
         case 0xCA: {registers.d = set(1, registers.d);} break;
         case 0xCB: {registers.e = set(1, registers.e);} break;
         case 0xCC: {registers.h = set(1, registers.h);} break;
         case 0xCD: {registers.l = set(1, registers.l);} break;
         case 0xCE: {write_memory(REGISTER_HL, set(1, read_memory(REGISTER_HL)));} break;
         case 0xCF: {registers.a = set(1, registers.a);} break;

         case 0xD0: {registers.b = set(2, registers.b);} break;
         case 0xD1: {registers.c = set(2, registers.c);} break;
         case 0xD2: {registers.d = set(2, registers.d);} break;
         case 0xD3: {registers.e = set(2, registers.e);} break;
         case 0xD4: {registers.h = set(2, registers.h);} break;
         case 0xD5: {registers.l = set(2, registers.l);} break;
         case 0xD6: {write_memory(REGISTER_HL, set(2, read_memory(REGISTER_HL)));} break;
         case 0xD7: {registers.a = set(2, registers.a);} break;

         case 0xD8: {registers.b = set(3, registers.b);} break;
         case 0xD9: {registers.c = set(3, registers.c);} break;
         case 0xDA: {registers.d = set(3, registers.d);} break;
         case 0xDB: {registers.e = set(3, registers.e);} break;
         case 0xDC: {registers.h = set(3, registers.h);} break;
         case 0xDD: {registers.l = set(3, registers.l);} break;
         case 0xDE: {write_memory(REGISTER_HL, set(3, read_memory(REGISTER_HL)));} break;
         case 0xDF: {registers.a = set(3, registers.a);} break;

         case 0xE0: {registers.b = set(4, registers.b);} break;
         case 0xE1: {registers.c = set(4, registers.c);} break;
         case 0xE2: {registers.d = set(4, registers.d);} break;
         case 0xE3: {registers.e = set(4, registers.e);} break;
         case 0xE4: {registers.h = set(4, registers.h);} break;
         case 0xE5: {registers.l = set(4, registers.l);} break;
         case 0xE6: {write_memory(REGISTER_HL, set(4, read_memory(REGISTER_HL)));} break;
         case 0xE7: {registers.a = set(4, registers.a);} break;

         case 0xE8: {registers.b = set(5, registers.b);} break;
         case 0xE9: {registers.c = set(5, registers.c);} break;
         case 0xEA: {registers.d = set(5, registers.d);} break;
         case 0xEB: {registers.e = set(5, registers.e);} break;
         case 0xEC: {registers.h = set(5, registers.h);} break;
         case 0xED: {registers.l = set(5, registers.l);} break;
         case 0xEE: {write_memory(REGISTER_HL, set(5, read_memory(REGISTER_HL)));} break;
         case 0xEF: {registers.a = set(5, registers.a);} break;

         case 0xF0: {registers.b = set(6, registers.b);} break;
         case 0xF1: {registers.c = set(6, registers.c);} break;
         case 0xF2: {registers.d = set(6, registers.d);} break;
         case 0xF3: {registers.e = set(6, registers.e);} break;
         case 0xF4: {registers.h = set(6, registers.h);} break;
         case 0xF5: {registers.l = set(6, registers.l);} break;
         case 0xF6: {write_memory(REGISTER_HL, set(6, read_memory(REGISTER_HL)));} break;
         case 0xF7: {registers.a = set(6, registers.a);} break;

         case 0xF8: {registers.b = set(7, registers.b);} break;
         case 0xF9: {registers.c = set(7, registers.c);} break;
         case 0xFA: {registers.d = set(7, registers.d);} break;
         case 0xFB: {registers.e = set(7, registers.e);} break;
         case 0xFC: {registers.h = set(7, registers.h);} break;
         case 0xFD: {registers.l = set(7, registers.l);} break;
         case 0xFE: {write_memory(REGISTER_HL, set(7, read_memory(REGISTER_HL)));} break;
         case 0xFF: {registers.a = set(7, registers.a);} break;

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
         case 0x40: {/*registers.b = registers.b;*/} break;
         case 0x41: {registers.b = registers.c;} break;
         case 0x42: {registers.b = registers.d;} break;
         case 0x43: {registers.b = registers.e;} break;
         case 0x44: {registers.b = registers.h;} break;
         case 0x45: {registers.b = registers.l;} break;
         case 0x46: {registers.b = read_memory(REGISTER_HL);} break;
         case 0x47: {registers.b = registers.a;} break;

         case 0x48: {registers.c = registers.b;} break;
         case 0x49: {/*registers.c = registers.c;*/} break;
         case 0x4A: {registers.c = registers.d;} break;
         case 0x4B: {registers.c = registers.e;} break;
         case 0x4C: {registers.c = registers.h;} break;
         case 0x4D: {registers.c = registers.l;} break;
         case 0x4E: {registers.c = read_memory(REGISTER_HL);} break;
         case 0x4F: {registers.c = registers.a;} break;

         case 0x50: {registers.d = registers.b;} break;
         case 0x51: {registers.d = registers.c;} break;
         case 0x52: {/*registers.d = registers.d;*/} break;
         case 0x53: {registers.d = registers.e;} break;
         case 0x54: {registers.d = registers.h;} break;
         case 0x55: {registers.d = registers.l;} break;
         case 0x56: {registers.d = read_memory(REGISTER_HL);} break;
         case 0x57: {registers.d = registers.a;} break;

         case 0x58: {registers.e = registers.b;} break;
         case 0x59: {registers.e = registers.c;} break;
         case 0x5A: {registers.e = registers.d;} break;
         case 0x5B: {/*registers.e = registers.e;*/} break;
         case 0x5C: {registers.e = registers.h;} break;
         case 0x5D: {registers.e = registers.l;} break;
         case 0x5E: {registers.e = read_memory(REGISTER_HL);} break;
         case 0x5F: {registers.e = registers.a;} break;

         case 0x60: {registers.h = registers.b;} break;
         case 0x61: {registers.h = registers.c;} break;
         case 0x62: {registers.h = registers.d;} break;
         case 0x63: {registers.h = registers.e;} break;
         case 0x64: {/*registers.h = registers.h;*/} break;
         case 0x65: {registers.h = registers.l;} break;
         case 0x66: {registers.h = read_memory(REGISTER_HL);} break;
         case 0x67: {registers.h = registers.a;} break;

         case 0x68: {registers.l = registers.b;} break;
         case 0x69: {registers.l = registers.c;} break;
         case 0x6A: {registers.l = registers.d;} break;
         case 0x6B: {registers.l = registers.e;} break;
         case 0x6C: {registers.l = registers.h;} break;
         case 0x6D: {/*registers.l = registers.l;*/} break;
         case 0x6E: {registers.l = read_memory(REGISTER_HL);} break;
         case 0x6F: {registers.l = registers.a;} break;

         case 0x70: {write_memory(REGISTER_HL, registers.b);} break;
         case 0x71: {write_memory(REGISTER_HL, registers.c);} break;
         case 0x72: {write_memory(REGISTER_HL, registers.d);} break;
         case 0x73: {write_memory(REGISTER_HL, registers.e);} break;
         case 0x74: {write_memory(REGISTER_HL, registers.h);} break;
         case 0x75: {write_memory(REGISTER_HL, registers.l);} break;
         case 0x77: {write_memory(REGISTER_HL, registers.a);} break;

         case 0x78: {registers.a = registers.b;} break;
         case 0x79: {registers.a = registers.c;} break;
         case 0x7A: {registers.a = registers.d;} break;
         case 0x7B: {registers.a = registers.e;} break;
         case 0x7C: {registers.a = registers.h;} break;
         case 0x7D: {registers.a = registers.l;} break;
         case 0x7E: {registers.a = read_memory(REGISTER_HL);} break;
         case 0x7F: {/*registers.a = registers.a;*/} break;

         case 0x06: {registers.b = read_memory(registers.pc++);} break; // LD B, n
         case 0x0E: {registers.c = read_memory(registers.pc++);} break; // LD C, n
         case 0x1E: {registers.e = read_memory(registers.pc++);} break; // LD E, n
         case 0x16: {registers.d = read_memory(registers.pc++);} break; // LD D, n
         case 0x26: {registers.h = read_memory(registers.pc++);} break; // LD H, n
         case 0x2E: {registers.l = read_memory(registers.pc++);} break; // LD L, n
         case 0x36: {write_memory(REGISTER_HL, read_memory(registers.pc++));} break; // LD (HL), n
         case 0x3E: {registers.a = read_memory(registers.pc++);} break; // LD A, n

         case 0x0A: {registers.a = read_memory(REGISTER_BC);} break; // LD A, (BC)
         case 0x1A: {registers.a = read_memory(REGISTER_DE);} break; // LD A, (DE)

         case 0xFA: // LD A, (nn)
         {
            u8 address_low  = read_memory(registers.pc++);
            u8 address_high = read_memory(registers.pc++);
            u16 address = ((u16)address_high << 8) | ((u16)address_low);

            registers.a = read_memory(address);
         } break;

         case 0x02: {write_memory(REGISTER_BC, registers.a);} break; // LD (BC), A
         case 0x12: {write_memory(REGISTER_DE, registers.a);} break; // LD (DE), A

         case 0xEA: // LD (nn), A
         {
            u8 address_low  = read_memory(registers.pc++);
            u8 address_high = read_memory(registers.pc++);
            u16 address = ((u16)address_high << 8) | ((u16)address_low);

            write_memory(address, registers.a);
         } break;

         case 0xF2: {registers.a = read_memory(0xFF00 + registers.c);} break;  // LDH A, (0xFF00 + C)
         case 0xE2: {write_memory(0xFF00 + registers.c, registers.a);} break; // LDH (0xFF00 + C), A

         case 0xF0: {registers.a = read_memory(0xFF00 + read_memory(registers.pc++));} break;  // LDH A, (0xFF00 + n)
         case 0xE0: {write_memory(0xFF00 + read_memory(registers.pc++), registers.a);} break; // LDH (0xFF00 + n), A

         case 0x22: // LDI (HL), A
         {
            write_memory(REGISTER_HL, registers.a);
            u16 updated_value = REGISTER_HL + 1;

            registers.h = updated_value >> 8;
            registers.l = updated_value & 0xFF;
         } break;

         case 0x32: // LDD (HL), A
         {
            write_memory(REGISTER_HL, registers.a);
            u16 updated_value = REGISTER_HL - 1;

            registers.h = updated_value >> 8;
            registers.l = updated_value & 0xFF;
         } break;

         case 0x2A: // LDI A, (HL)
         {
            registers.a = read_memory(REGISTER_HL);
            u16 updated_value = REGISTER_HL + 1;

            registers.h = updated_value >> 8;
            registers.l = updated_value & 0xFF;
         } break;

         case 0x3A: // LDD A, (HL)
         {
            registers.a = read_memory(REGISTER_HL);
            u16 updated_value = REGISTER_HL - 1;

            registers.h = updated_value >> 8;
            registers.l = updated_value & 0xFF;
         } break;

         case 0xF9: {registers.sp = REGISTER_HL;} break; // LD SP, HL

         case 0xC5: // PUSH BC
         {
            write_memory(--registers.sp, registers.b);
            write_memory(--registers.sp, registers.c);
         } break;

         case 0xD5: // PUSH DE
         {
            write_memory(--registers.sp, registers.d);
            write_memory(--registers.sp, registers.e);
         } break;

         case 0xE5: // PUSH HL
         {
            write_memory(--registers.sp, registers.h);
            write_memory(--registers.sp, registers.l);
         } break;

         case 0xF5: // PUSH AP
         {
            write_memory(--registers.sp, registers.a);
            write_memory(--registers.sp, registers.f);
         } break;

         case 0xC1: // POP BC
         {
            registers.c = read_memory(registers.sp++);
            registers.b = read_memory(registers.sp++);
         } break;

         case 0xD1: // POP DE
         {
            registers.e = read_memory(registers.sp++);
            registers.d = read_memory(registers.sp++);
         } break;

         case 0xE1: //POP HL
         {
            registers.l = read_memory(registers.sp++);
            registers.h = read_memory(registers.sp++);
         } break;

         case 0xF1: // POP AF
         {
            registers.f = read_memory(registers.sp++);
            registers.a = read_memory(registers.sp++);
         } break;


         // NOTE(law): 16-bit load instructions
         case 0x08: // LD (nn), SP
         {
            u8 address_low  = read_memory(registers.pc++);
            u8 address_high = read_memory(registers.pc++);
            u16 address = ((u16)address_high << 8) | ((u16)address_low);

            write_memory16(address, registers.sp);
         } break;

         case 0x01: // LD BC, nn
         {
            registers.c = read_memory(registers.pc++);
            registers.b = read_memory(registers.pc++);
         } break;

         case 0x11: // LD DE, nn
         {
            registers.e = read_memory(registers.pc++);
            registers.d = read_memory(registers.pc++);
         } break;

         case 0x21: // LD HL, nn
         {
            registers.l = read_memory(registers.pc++);
            registers.h = read_memory(registers.pc++);
         } break;

         case 0x31: // LD SP, nn
         {
            u8 value_low  = read_memory(registers.pc++);
            u8 value_high = read_memory(registers.pc++);
            u16 value = ((u16)value_high << 8) | ((u16)value_low);

            registers.sp = value;
         } break;


         // NOTE(law): Rotate and Shift instructions
         case 0x07: rlca(); break;
         case 0x17: rla(); break;
         case 0x0F: rrca(); break;
         case 0x1F: rra(); break;


         // NOTE(law): 8-bit Arithmetic/Logic instructions
         case 0x80: add(registers.b); break; // ADD A, B
         case 0x81: add(registers.c); break; // ADD A, C
         case 0x82: add(registers.d); break; // ADD A, D
         case 0x83: add(registers.e); break; // ADD A, E
         case 0x84: add(registers.h); break; // ADD A, H
         case 0x85: add(registers.l); break; // ADD A, L
         case 0x86: add(read_memory(REGISTER_HL)); break; // ADD A, (HL)
         case 0x87: add(registers.a); break; // ADD A, A

         case 0xC6: add(read_memory(registers.pc++)); break; // ADD A, n

         case 0x88: adc(registers.b); break; // ADC A, B
         case 0x89: adc(registers.c); break; // ADC A, C
         case 0x8A: adc(registers.d); break; // ADC A, D
         case 0x8B: adc(registers.e); break; // ADC A, E
         case 0x8C: adc(registers.h); break; // ADC A, H
         case 0x8D: adc(registers.l); break; // ADC A, L
         case 0x8E: adc(read_memory(REGISTER_HL)); break; // ADC A, (HL)
         case 0x8F: adc(registers.a); break; // ADC A, A

         case 0xCE: {adc(read_memory(registers.pc++));} break; // ADC A, n

         case 0x90: sub(registers.b); break; // SUB A, B
         case 0x91: sub(registers.c); break; // SUB A, C
         case 0x92: sub(registers.d); break; // SUB A, D
         case 0x93: sub(registers.e); break; // SUB A, E
         case 0x94: sub(registers.h); break; // SUB A, H
         case 0x95: sub(registers.l); break; // SUB A, L
         case 0x96: sub(read_memory(REGISTER_HL)); break; // SUB A, (HL)
         case 0x97: sub(registers.a); break; // SUB A, A

         case 0xD6: sub(read_memory(registers.pc++)); break; // SUB A, n

         case 0x98: sbc(registers.b); break; // SBC A, B
         case 0x99: sbc(registers.c); break; // SBC A, C
         case 0x9A: sbc(registers.d); break; // SBC A, D
         case 0x9B: sbc(registers.e); break; // SBC A, E
         case 0x9C: sbc(registers.h); break; // SBC A, H
         case 0x9D: sbc(registers.l); break; // SBC A, L
         case 0x9E: sbc(read_memory(REGISTER_HL)); break; // SBC A, (HL)
         case 0x9F: sbc(registers.a); break; // SBC A, A

         case 0xDE: sbc(read_memory(registers.pc++)); break; // SBC A, n

         case 0x27: // DAA
         {
            // TODO(law): Look up how decimal adjustment actually works and
            // implement it!

            assert(!"DAA");
         } break;

         case 0x2F: // CPL
         {
            registers.a = ~registers.a;
            registers.f |= FLAG_N_MASK;
            registers.f |= FLAG_H_MASK;
         } break;

         case 0xA8: xor(registers.b); break; // XOR A, B
         case 0xA9: xor(registers.c); break; // XOR A, C
         case 0xAA: xor(registers.d); break; // XOR A, D
         case 0xAB: xor(registers.e); break; // XOR A, E
         case 0xAC: xor(registers.h); break; // XOR A, H
         case 0xAD: xor(registers.l); break; // XOR A, L
         case 0xAE: xor(read_memory(REGISTER_HL)); break; // XOR A, (HL)
         case 0xAF: xor(registers.a); break; // XOR A, A

         case 0xEE: xor(read_memory(registers.pc++)); break; // XOR A, n

         case 0xB0: or(registers.b); break; // OR A, B
         case 0xB1: or(registers.c); break; // OR A, C
         case 0xB2: or(registers.d); break; // OR A, D
         case 0xB3: or(registers.e); break; // OR A, E
         case 0xB4: or(registers.h); break; // OR A, H
         case 0xB5: or(registers.l); break; // OR A, L
         case 0xB6: or(read_memory(REGISTER_HL)); break; // OR A, (HL)
         case 0xB7: or(registers.a); break; // OR A, A

         case 0xF6: or(read_memory(registers.pc++)); break; // OR A, n

         case 0xA0: and(registers.b); break; // AND A, B
         case 0xA1: and(registers.c); break; // AND A, C
         case 0xA2: and(registers.d); break; // AND A, D
         case 0xA3: and(registers.e); break; // AND A, E
         case 0xA4: and(registers.h); break; // AND A, H
         case 0xA5: and(registers.l); break; // AND A, L
         case 0xA6: and(read_memory(REGISTER_HL)); break; // AND A, (HL)
         case 0xA7: and(registers.a); break; // AND A, A

         case 0xE6: and(read_memory(registers.pc++)); break; // AND A, n

         case 0xB8: cp(registers.b); break; // CP A, B
         case 0xB9: cp(registers.c); break; // CP A, C
         case 0xBA: cp(registers.d); break; // CP A, D
         case 0xBB: cp(registers.e); break; // CP A, E
         case 0xBC: cp(registers.h); break; // CP A, H
         case 0xBD: cp(registers.l); break; // CP A, L
         case 0xBE: cp(read_memory(REGISTER_HL)); break; // CP A, (HL)
         case 0xBF: cp(registers.a); break; // CP A, A

         case 0xFE: cp(read_memory(registers.pc++)); break; // CP A, n

         case 0x04: {registers.b = inc(registers.b);} break; // INC B
         case 0x0C: {registers.c = inc(registers.c);} break; // INC C
         case 0x14: {registers.d = inc(registers.d);} break; // INC D
         case 0x1C: {registers.e = inc(registers.e);} break; // INC E
         case 0x24: {registers.h = inc(registers.h);} break; // INC H
         case 0x2C: {registers.l = inc(registers.l);} break; // INC L
         case 0x34: {write_memory(REGISTER_HL, inc(read_memory(REGISTER_HL)));} break; // INC (HL)
         case 0x3C: {registers.a = inc(registers.a); break;} // INC A

         case 0x05: {registers.b = dec(registers.b);} break; // DEC B
         case 0x0D: {registers.c = dec(registers.c);} break; // DEC C
         case 0x15: {registers.d = dec(registers.d);} break; // DEC D
         case 0x1D: {registers.e = dec(registers.e);} break; // DEC E
         case 0x25: {registers.h = dec(registers.h);} break; // DEC H
         case 0x2D: {registers.l = dec(registers.l);} break; // DEC L
         case 0x35: write_memory(REGISTER_HL, dec(read_memory(REGISTER_HL))); break; // DEC (HL)
         case 0x3D: {registers.a = dec(registers.a);} break; // DEC A


         // NOTE(law): 16-bit Arithmetic/Logic instructions
         case 0x09: add16(REGISTER_BC); break; // ADD HL, BC
         case 0x19: add16(REGISTER_DE); break; // ADD HL, DE
         case 0x29: add16(REGISTER_HL); break; // ADD HL, HL
         case 0x39: add16(registers.sp); break; // ADD HL, SP

         case 0x03: inc16_bytes(&registers.b, &registers.c); break; // INC BC
         case 0x13: inc16_bytes(&registers.d, &registers.e); break; // INC DE
         case 0x23: inc16_bytes(&registers.h, &registers.l); break; // INC HL
         case 0x33: inc16(&registers.sp); break; // INC SP

         case 0x0B: dec16_bytes(&registers.b, &registers.c); break; // DEC BC
         case 0x1B: dec16_bytes(&registers.d, &registers.e); break; // DEC DE
         case 0x2B: dec16_bytes(&registers.h, &registers.l); break; // DEC HL
         case 0x3B: dec16(&registers.sp); break; // DEC SP

         case 0xE8: // ADD SP, dd
         {
            s8 offset = read_memory(registers.pc++);

            s32 extended_address = (s32)registers.sp + (s32)offset;
            u8 half_sum = (registers.sp & 0xF) + (offset & 0xF);

            registers.sp = (u16)extended_address;

            // NOTE(law): The Zero flag is always unset.
            registers.f &= ~FLAG_Z_MASK;

            // NOTE(law): The Subtraction flag is always unset.
            registers.f &= ~FLAG_N_MASK;

            // NOTE(law): Set the Half Carry flag when a carry from bit 3 occurs.
            registers.f = (registers.f & ~FLAG_H_MASK) | (((half_sum & 0x10) == 0x10) << FLAG_H_BIT);

            // NOTE(law): Set the Carry flag when a carry from bit 7 occurs.
            registers.f = (registers.f & ~FLAG_C_MASK) | ((extended_address > 0xFFFF) << FLAG_C_BIT);
         } break;

         case 0xF8: // LD HL, SP + dd
         {
            s8 offset = read_memory(registers.pc++);

            s32 extended_address = (s32)registers.sp + (s32)offset;
            u8 half_sum = (registers.sp & 0xF) + (offset & 0xF);

            registers.h = (u8)((u16)extended_address >> 8);
            registers.l = (u8)((u16)extended_address & 0xFF);

            // NOTE(law): The Zero flag is always unset.
            registers.f &= ~FLAG_Z_MASK;

            // NOTE(law): The Subtraction flag is always unset.
            registers.f &= ~FLAG_N_MASK;

            // NOTE(law): Set the Half Carry flag when a carry from bit 3 occurs.
            registers.f = (registers.f & ~FLAG_H_MASK) | (((half_sum & 0x10) == 0x10) << FLAG_H_BIT);

            // NOTE(law): Set the Carry flag when a carry from bit 7 occurs.
            registers.f = (registers.f & ~FLAG_C_MASK) | ((extended_address > 0xFFFF) << FLAG_C_BIT);
         } break;


         // NOTE(law): CPU Control instructions
         case 0x3F: // CCF
         {
            registers.f &= ~FLAG_N_MASK;
            registers.f &= ~FLAG_H_MASK;

            u8 flipped_c = !FLAG_C;
            registers.f = (registers.f & ~FLAG_C_MASK) | (flipped_c << FLAG_C_BIT);
         } break;

         case 0x37: // SCF
         {
            registers.f &= ~FLAG_N_MASK;
            registers.f &= ~FLAG_H_MASK;
            registers.f |= FLAG_C_MASK;
         } break;

         case 0x00: {} break; // NOP
         case 0x76: {halt = true;} break; // HALT
         case 0x10: {stop = true; registers.pc++;} break; // STOP
         case 0xF3: {ime = false;} break; // DI
         case 0xFB: {ime = true;} break; // EI


         // NOTE(law): Jump instructions
         case 0xC3: jp(true); break; // JP nn
         case 0xE9: {registers.pc = REGISTER_HL;} break; // JP HL

         case 0xC2: {condition_succeeded = !FLAG_Z; jp(condition_succeeded);} break; // JP NZ, nn
         case 0xCA: {condition_succeeded = FLAG_Z;  jp(condition_succeeded);} break; // JP Z, nn
         case 0xD2: {condition_succeeded = !FLAG_C; jp(condition_succeeded);} break; // JP NC, nn
         case 0xDA: {condition_succeeded = FLAG_C;  jp(condition_succeeded);} break; // JP C, nn

         case 0x18: jr(true); break; // JR PC + n
         case 0x20: {condition_succeeded = !FLAG_Z; jr(condition_succeeded);} break; // JR NZ, PC + n
         case 0x28: {condition_succeeded = FLAG_Z;  jr(condition_succeeded);} break; // JR Z, PC + n
         case 0x30: {condition_succeeded = !FLAG_C; jr(condition_succeeded);} break; // JR NC, PC + n
         case 0x38: {condition_succeeded = FLAG_C;  jr(condition_succeeded);} break; // JR C, PC + n

         case 0xC4: {condition_succeeded = !FLAG_Z; call(condition_succeeded);} break; // CALL NZ, nn
         case 0xCC: {condition_succeeded = FLAG_Z;  call(condition_succeeded);} break; // CALL Z, nn
         case 0xCD: call(true); break; // CALL nn
         case 0xD4: {condition_succeeded = !FLAG_C; call(condition_succeeded);} break; // CALL NC, nn
         case 0xDC: {condition_succeeded = FLAG_C;  call(condition_succeeded);} break; // CALL C, nn

         case 0xC0: {condition_succeeded = !FLAG_Z; ret(condition_succeeded);} break; // RET NZ
         case 0xC8: {condition_succeeded = FLAG_Z;  ret(condition_succeeded);} break; // RET Z
         case 0xC9: ret(true); break; // RET
         case 0xD0: {condition_succeeded = !FLAG_C; ret(condition_succeeded);} break; // RET NC
         case 0xD8: {condition_succeeded = FLAG_C;  ret(condition_succeeded);} break; // RET C

         case 0xD9: {ret(true); ime = true;} break; // RETI

         case 0xC7: rst(0x00); break;
         case 0xCF: rst(0x08); break;
         case 0xD7: rst(0x10); break;
         case 0xDF: rst(0x18); break;
         case 0xE7: rst(0x20); break;
         case 0xEF: rst(0x28); break;
         case 0xF7: rst(0x30); break;
         case 0xFF: rst(0x38); break;

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

   u32 cycles = 0;
   if(prefix_opcode)
   {
      if(condition_succeeded)
      {
         cycles = cbprefix_cycle_counts[opcode].count;
      }
      else
      {
         cycles = cbprefix_cycle_counts[opcode].failed_condition_count;
      }
   }
   else
   {
      if(condition_succeeded)
      {
         cycles = nonprefix_cycle_counts[opcode].count;
      }
      else
      {
         cycles = nonprefix_cycle_counts[opcode].failed_condition_count;
      }
   }

   assert(cycles > 0);

   return(cycles);
}

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

#define LCD_STATUS_LYC_LY_INTERRUPT_BIT 6
#define LCD_STATUS_OAM_INTERRUPT_BIT    5
#define LCD_STATUS_VBLANK_INTERRUPT_BIT 4
#define LCD_STATUS_HBLANK_INTERRUPT_BIT 3
#define LCD_STATUS_LYC_EQUALS_LY_BIT    2

#define LCD_STATUS_INTERRUPT_LYC_LY_MASK (1 << LCD_STATUS_LYC_LY_INTERRUPT_BIT)
#define LCD_STATUS_INTERRUPT_OAM_MASK    (1 << LCD_STATUS_OAM_INTERRUPT_BIT)
#define LCD_STATUS_INTERRUPT_VBLANK_MASK (1 << LCD_STATUS_VBLANK_INTERRUPT_BIT)
#define LCD_STATUS_INTERRUPT_HBLANK_MASK (1 << LCD_STATUS_HBLANK_INTERRUPT_BIT)
#define LCD_STATUS_LYC_EQUALS_LY_MASK    (1 << LCD_STATUS_LYC_EQUALS_LY_BIT)

#define LCD_STATUS_MODE_HBLANK   0
#define LCD_STATUS_MODE_VBLANK   1
#define LCD_STATUS_MODE_OAM      2
#define LCD_STATUS_MODE_TRANSFER 3

#define JOYPAD_DOWN_START_BIT 3
#define JOYPAD_UP_SELECT_BIT  2
#define JOYPAD_LEFT_B_BIT     1
#define JOYPAD_RIGHT_A_BIT    0

#define JOYPAD_DOWN_START_MASK (1 << JOYPAD_DOWN_START_MASK)
#define JOYPAD_UP_SELECT_MASK  (1 << JOYPAD_UP_SELECT_MASK
#define JOYPAD_LEFT_B_MASK     (1 << JOYPAD_LEFT_B_MASK)
#define JOYPAD_RIGHT_A_MASK    (1 << JOYPAD_RIGHT_A_MASK)

static void
handle_interrupts()
{
   if(ime && (read_memory(REGISTER_IE_ADDRESS) & read_memory(REGISTER_IF_ADDRESS)))
   {
      // NOTE(law): Disable interrupts for the duration of the handler.
      ime = false;

      // NOTE(law): The priority of interrupts are ordered by increasing bit
      // index in registers.if (i.e. VBlank with bit index 0 has the highest
      // priority).
      u32 bit_index = 0;
      for(; bit_index <= 4; ++bit_index)
      {
         if((REGISTER_IF_ADDRESS >> bit_index) & 0x1)
         {
            break;
         }
      }
      assert(bit_index <= 4);

      // NOTE(law): Reset the bit of the interrupt we plan to handle.
      u8 register_if = read_memory(REGISTER_IF_ADDRESS) & ~(1 << bit_index);
      write_memory(REGISTER_IF_ADDRESS, register_if);

      // TODO(law): Wait for two cycles using NOPs.

      write_memory(--registers.sp, registers.pc >> 8);
      write_memory(--registers.sp, registers.pc & 0xFF);

      u16 isr_addresses[] = {0x40, 0x48, 0x50, 0x58, 0x60};
      registers.pc = isr_addresses[bit_index];
   }
}

struct pixel_bitmap
{
   u32 width;
   u32 height;
   u32 *memory;
};

enum monochrome_color_scheme
{
   MONOCHROME_COLOR_SCHEME_DMG,
   MONOCHROME_COLOR_SCHEME_MGB,
   MONOCHROME_COLOR_SCHEME_LIGHT,

   MONOCHROME_COLOR_SCHEME_COUNT,
};

// NOTE(law): Update the global color scheme from platform code, since each
// platform might want to handle the necessary UI differently.
static enum monochrome_color_scheme gem_global_color_scheme;

static u32 monochrome_color_schemes[][5] =
{
   // NOTE(law): The value at index 4 of each color scheme is the color of the
   // LCD when the display is disabled. It is not actually used by the palette.

   {0xFFE0F8D0, 0xFF88C070, 0xFF346856, 0xFF081820, 0xFFACC480}, // DMG
   {0xFFE0DBCD, 0xFFA89F94, 0xFF706B66, 0xFF2B2B26, 0xFFBDB890}, // MGB
   {0xFF65F2BA, 0xFF39C28C, 0xFF30B37F, 0xFF0E7F54, 0xFFBDB890}, // LIGHT
};

static u32
get_display_off_color()
{
   enum monochrome_color_scheme color_scheme = gem_global_color_scheme;

   u32 result = monochrome_color_schemes[color_scheme][4];
   return(result);
}

static void
get_palette(u32 *palette, u16 address)
{
   enum monochrome_color_scheme color_scheme = gem_global_color_scheme;

   assert(ARRAY_LENGTH(monochrome_color_schemes) == MONOCHROME_COLOR_SCHEME_COUNT);
   u32 *colors = monochrome_color_schemes[color_scheme];

   u8 palette_data = read_memory(address);

   palette[0] = colors[(palette_data >> 0) & 0x3];
   palette[1] = colors[(palette_data >> 2) & 0x3];
   palette[2] = colors[(palette_data >> 4) & 0x3];
   palette[3] = colors[(palette_data >> 6) & 0x3];
}

static void
clear(struct pixel_bitmap *bitmap, u32 color)
{
   for(u32 y = 0; y < bitmap->height; ++y)
   {
      for(u32 x = 0; x < bitmap->width; ++x)
      {
         bitmap->memory[(bitmap->width * y) + x] = color;
      }
   }
}

static void
clear_scanline(struct pixel_bitmap *bitmap, u32 scanline, u32 color)
{
   for(u32 x = 0; x < bitmap->width; ++x)
   {
      bitmap->memory[(bitmap->width * scanline) + x] = color;
   }
}

#define RESOLUTION_BASE_WIDTH 160
#define RESOLUTION_BASE_HEIGHT 144

#define TILE_PIXEL_DIM 8
#define TILEMAP_TILE_DIM 32
#define TILES_PER_SCREEN_WIDTH  (RESOLUTION_BASE_WIDTH  / TILE_PIXEL_DIM)
#define TILES_PER_SCREEN_HEIGHT (RESOLUTION_BASE_HEIGHT / TILE_PIXEL_DIM)

#define VRAM_TILE_BLOCK_0 0x8000
#define VRAM_TILE_BLOCK_1 0x8800
#define VRAM_TILE_BLOCK_2 0x9000

#define PALETTE_DATA_BG   0xFF47
#define PALETTE_DATA_OBJ0 0xFF48
#define PALETTE_DATA_OBJ1 0xFF49

#define LCD_PPU_ENABLED      ((register_lcdc >> 7) & 0x1) // 0=Off, 1=On
#define WINDOW_TILE_MAP_AREA ((register_lcdc >> 6) & 0x1) // 0=9800-9BFF, 1=9C00-9FFF
#define WINDOW_ENABLED       ((register_lcdc >> 5) & 0x1) // 0=Off, 1=On
#define USE_8000_ADDRESSING  ((register_lcdc >> 4) & 0x1) // 0=8800-97FF, 1=8000-8FFF
#define BG_TILE_MAP_AREA     ((register_lcdc >> 3) & 0x1) // 0=9800-9BFF, 1=9C00-9FFF
#define OBJECT_SIZE_8X16     ((register_lcdc >> 2) & 0x1) // 0=8x8, 1=8x16
#define OBJECTS_ENABLED      ((register_lcdc >> 1) & 0x1) // 0=Off, 1=On
#define BG_WINDOW_ENABLED    ((register_lcdc >> 0) & 0x1) // 0=Off, 1=On

#define BYTES_PER_TILE_SCANLINE 2
#define BYTES_PER_TILE (BYTES_PER_TILE_SCANLINE * TILE_PIXEL_DIM)

static void
render_scanline(struct pixel_bitmap *bitmap, u16 scanline)
{
   // TODO(law): Implement the full pixel FIFO with sprite and window support.

   // TODO(law): Optimize pixel fill!

   u8 register_lcdc = read_memory(0xFF40);

   u32 palette[4];
   get_palette(palette, PALETTE_DATA_BG);

   clear_scanline(bitmap, scanline, palette[0]);

   for(u16 screen_tile_x = 0; screen_tile_x < TILES_PER_SCREEN_WIDTH; ++screen_tile_x)
   {
      for(u16 tile_pixel_x = 0; tile_pixel_x < TILE_PIXEL_DIM; ++tile_pixel_x)
      {
         u8 viewport_x = read_memory(0xFF43);
         u8 viewport_y = read_memory(0xFF42);

         u16 destination_pixel_x = (screen_tile_x * TILE_PIXEL_DIM) + tile_pixel_x; // 0-159
         u16 destination_pixel_y = scanline; // 0-143

         u16 source_pixel_x = viewport_x + destination_pixel_x;
         u16 source_pixel_y = viewport_y + destination_pixel_y;

         u16 source_tile_x = (source_pixel_x / TILE_PIXEL_DIM) & 0x1F; // 0-31
         u16 source_tile_y = (source_pixel_y / TILE_PIXEL_DIM) & 0x1F; // 0-31

         u16 source_pixel_offset_x = source_pixel_x % TILE_PIXEL_DIM; // 0-7
         u16 source_pixel_offset_y = source_pixel_y % TILE_PIXEL_DIM; // 0-7

         u16 tilemap_address_base = (BG_TILE_MAP_AREA) ? 0x9C00 : 0x9800;
         u16 tilemap_address_offset = (source_tile_y * TILEMAP_TILE_DIM) + source_tile_x;

         u16 tile_index_address = tilemap_address_base + tilemap_address_offset;
         u8 tile_index = read_memory(tile_index_address);

         u16 tile_address = 0;
         if(USE_8000_ADDRESSING)
         {
            u16 offset = tile_index * BYTES_PER_TILE;
            tile_address = 0x8000 + offset;
         }
         else
         {
            s32 offset = (s32)tile_index * BYTES_PER_TILE;
            tile_address = (u16)((s32)0x9000 + offset);
         }
         assert(tile_address > 0);

         u8 byte0 = read_memory((tile_address + source_pixel_offset_y * 2) + 0);
         u8 byte1 = read_memory((tile_address + source_pixel_offset_y * 2) + 1);

         u32 bit_offset = TILE_PIXEL_DIM - 1 - tile_pixel_x;
         u8 low_bit = (byte0 >> bit_offset) & 0x1;
         u8 high_bit = (byte1 >> bit_offset) & 0x1;

         u32 color_index = (high_bit << 1) | low_bit;
         u32 bitmap_index = (bitmap->width * destination_pixel_y) + destination_pixel_x;

         bitmap->memory[bitmap_index] = palette[color_index];
      }
   }
}

#define SOUND_OUTPUT_HZ 48000
#define SOUND_OUTPUT_CHANNEL_COUNT 2
#define SOUND_OUTPUT_BYTES_PER_SAMPLE (SOUND_OUTPUT_CHANNEL_COUNT * sizeof(s16))

#define DEBUG_SINE_WAVE 0

#define MASTER_ON_BIT 7
#define CHANNEL1_ON_BIT 0
#define CHANNEL2_ON_BIT 1
#define CHANNEL3_ON_BIT 2
#define CHANNEL4_ON_BIT 3

#define MASTER_ON   ((register_nr52 >> MASTER_ON_BIT)   & 0x1)
#define CHANNEL1_ON ((register_nr52 >> CHANNEL1_ON_BIT) & 0x1)
#define CHANNEL2_ON ((register_nr52 >> CHANNEL2_ON_BIT) & 0x1)
#define CHANNEL3_ON ((register_nr52 >> CHANNEL3_ON_BIT) & 0x1)
#define CHANNEL4_ON ((register_nr52 >> CHANNEL4_ON_BIT) & 0x1)

#define CHANNEL1_RESTART (register_nr14 >> 7)
#define CHANNEL2_RESTART (register_nr24 >> 7)
#define CHANNEL3_RESTART (register_nr34 >> 7)
#define CHANNEL4_RESTART (register_nr44 >> 7)

struct sound_samples
{
   u32 sample_index;
   u32 size;

   s16 *samples;
};

static void
clear_sound_samples(struct sound_samples *sound)
{
   sound->sample_index = 0;
   zero_memory(sound->samples, sound->size);
}

static void
generate_debug_samples(struct sound_samples *destination, u32 sample_count)
{
   assert(destination->size >= (destination->sample_index + sample_count) * SOUND_OUTPUT_BYTES_PER_SAMPLE);

   static float wave_t = 0;

   float volume = 1000.0f;
   float wave_period = SOUND_OUTPUT_HZ / 256;

   u32 offset = destination->sample_index * SOUND_OUTPUT_CHANNEL_COUNT;
   s16 *samples = destination->samples;

   for(u32 index = 0; index < (sample_count * SOUND_OUTPUT_CHANNEL_COUNT); index += SOUND_OUTPUT_CHANNEL_COUNT)
   {
      s16 sample = (s16)(sin(wave_t) * volume);

      *(samples + offset + index + 0) = sample;
      *(samples + offset + index + 1) = sample;

      wave_t += (TAU / wave_period);
      if(wave_t > TAU)
      {
         wave_t = 0;
      }
   }

   destination->sample_index += sample_count;
}

static void
generate_sound_sample(struct sound_samples *destination)
{
   static u32 frame_sequncer_clock = 0;

   // NOTE(law): Master volume.
   u32 register_nr50 = read_memory(0xFF24);

   u8 enable_vin_left = (register_nr50 >> 7) & 0x1;
   u8 enable_vin_right = (register_nr50 >> 3) & 0x1;

   u8 volume_left = ((register_nr50 >> 4) & 0x7) + 1; // Range of 1-8
   u8 volume_right = ((register_nr50 >> 0) & 0x7) + 1; // Range of 1-8

   // NOTE(law): Per-channel sound panning.
   u32 register_nr51 = read_memory(0xFF25);

   u8 channel4_left_on = (register_nr51 >> 7) & 0x1;
   u8 channel3_left_on = (register_nr51 >> 6) & 0x1;
   u8 channel2_left_on = (register_nr51 >> 5) & 0x1;
   u8 channel1_left_on = (register_nr51 >> 4) & 0x1;

   u8 channel4_right_on = (register_nr51 >> 3) & 0x1;
   u8 channel3_right_on = (register_nr51 >> 2) & 0x1;
   u8 channel2_right_on = (register_nr51 >> 1) & 0x1;
   u8 channel1_right_on = (register_nr51 >> 0) & 0x1;

   // NOTE(law): Channel 1 registers.
   u8 register_nr10 = read_memory(0xFF10); // NOTE(law): Sweep
   u8 register_nr11 = read_memory(0xFF11); // NOTE(law): Length
   u8 register_nr12 = read_memory(0xFF12); // NOTE(law): Volume
   u8 register_nr13 = read_memory(0xFF13); // NOTE(law): Frequency
   u8 register_nr14 = read_memory(0xFF14); // NOTE(law): Control

   // NOTE(law): Channel 2 registers.
   u8 register_nr21 = read_memory(0xFF16); // NOTE(law): Length
   u8 register_nr22 = read_memory(0xFF17); // NOTE(law): Volume
   u8 register_nr23 = read_memory(0xFF18); // NOTE(law): Frequency
   u8 register_nr24 = read_memory(0xFF19); // NOTE(law): Control

   // NOTE(law): Channel 3 registers.
   u8 register_nr30 = read_memory(0xFF30); // NOTE(law): On/off
   u8 register_nr31 = read_memory(0xFF31); // NOTE(law): Length
   u8 register_nr32 = read_memory(0xFF32); // NOTE(law): Volume
   u8 register_nr33 = read_memory(0xFF33); // NOTE(law): Frequency
   u8 register_nr34 = read_memory(0xFF34); // NOTE(law): Control

   // NOTE(law): Channel 4 registers.
   u8 register_nr41 = read_memory(0xFF20); // NOTE(law): Length
   u8 register_nr42 = read_memory(0xFF21); // NOTE(law): Volume
   u8 register_nr43 = read_memory(0xFF22); // NOTE(law): Frequency
   u8 register_nr44 = read_memory(0xFF23); // NOTE(law): Control

   // NOTE(law); Sound on/off.
   u8 register_nr52 = read_memory(0xFF26);
   if(MASTER_ON)
   {
      // NOTE(law): Restart channels that request a restart.
      if(CHANNEL1_RESTART) register_nr52 |= (1 << CHANNEL1_ON_BIT);
      if(CHANNEL2_RESTART) register_nr52 |= (1 << CHANNEL2_ON_BIT);
      if(CHANNEL3_RESTART) register_nr52 |= (1 << CHANNEL3_ON_BIT);
      if(CHANNEL4_RESTART) register_nr52 |= (1 << CHANNEL4_ON_BIT);
   }
   else
   {
      // NOTE(law): Reset the individual channel on/off bits.
      register_nr52 = 0x00;
   }

   // NOTE(law): Write back the on/off channel changes to memory.
   write_memory(0xFF26, register_nr52);

   static u8 channel1_volume = 0;
   static u8 channel1_envelope_period = 0;
   static u8 channel1_envelope_direction = 0;
   static float channel1_frequency = 0;
   static float channel1_duty = 0;

   // NOTE(law): Channel 1 (tone and sweep)
   if(CHANNEL1_ON)
   {
      // NOTE(law): Length
      u8 wave_pattern_duty = (register_nr11 >> 6) & 0x3;
      u8 sound_length_data = (register_nr11 >> 0) & 0x1F; // Range: 0-63

      // NOTE(law): Volume
      u8 initial_volume = (register_nr12 >> 4) & 0xF; // Range 0-15
      u8 envelope_direction = (register_nr12 >> 3) & 0x1; // 0 <-, 1 ->
      u8 envelope_period = (register_nr12 >> 0) & 0x7; // Range 0-7

      // NOTE(law): Frequency
      u8 frequency_low = register_nr13;

      // NOTE(law): Control
      u8 counter_selection = (register_nr14 >> 6) & 0x1;
      u8 frequency_high = (register_nr14 >> 0) & 0x7;

      u16 frequency_data = ((u16)frequency_high << 8) | frequency_low;

      if(CHANNEL1_RESTART)
      {
         register_nr14 &= ~(1 << 7);
         write_memory(0xFF14, register_nr14);

         channel1_volume = initial_volume;
         channel1_envelope_period = envelope_period;
         channel1_envelope_direction = envelope_direction;
         channel1_frequency = 131072.0f / (float)(2048 - frequency_data);
         channel1_duty = 1.0f / (float)(4 - wave_pattern_duty);
      }
   }

   // NOTE(law): Channel 2 (tone)
   if(CHANNEL2_ON)
   {
      // TODO(law) Implement this channel!
   }

   // NOTE(law): Channel 3 (wave output)
   if(CHANNEL3_ON)
   {
      // TODO(law) Implement this channel!
   }

   // NOTE(law): Channel 4 (noise)
   if(CHANNEL4_ON)
   {
      // TODO(law) Implement this channel!
   }

   // NOTE(law): Update frame sequencer.
   if((frame_sequncer_clock % (SOUND_OUTPUT_HZ / 256)) == 0)
   {
      // TODO(law): Update length counters.
   }
   if((frame_sequncer_clock % (SOUND_OUTPUT_HZ / 64)) == 0)
   {
      // TODO(law): Update volume envelope counters.

      if(channel1_envelope_period)
      {
         static u8 counter = 0;
         if(counter > 0)
         {
            counter--;
         }

         if(counter == 0)
         {
            counter = channel1_envelope_period;
            if(channel1_volume > 0x0 && !channel1_envelope_direction)
            {
               channel1_volume--;
            }
            else if(channel1_volume < 0xF && channel1_envelope_direction)
            {
               channel1_volume++;
            }
         }
      }
   }
   if((frame_sequncer_clock % (SOUND_OUTPUT_HZ / 128)) == 0)
   {
      // TODO(law): Update sweep counters.
   }
   frame_sequncer_clock++;

   // NOTE(law): Fill output buffer with samples.
   s16 sample_left = 0;
   s16 sample_right = 0;

   if(CHANNEL1_ON)
   {
      static float wave_position = 0;

      s16 sample = (wave_position < channel1_duty)
      ? -(s16)channel1_volume
      : (s16)channel1_volume;

      if(channel1_left_on) {sample_left += sample * volume_left;}
      if(channel1_right_on) {sample_right += sample * volume_right;}

      wave_position += (channel1_frequency / SOUND_OUTPUT_HZ);
      if(wave_position >= 1.0f)
      {
         wave_position = 0;
      }
   }

   if(CHANNEL2_ON)
   {
      s16 sample = 0;
      if(channel1_left_on) {sample_left += sample * volume_left;}
      if(channel1_right_on) {sample_right += sample * volume_right;}
   }

   if(CHANNEL3_ON)
   {
      s16 sample = 0;
      if(channel1_left_on) {sample_left += sample * volume_left;}
      if(channel1_right_on) {sample_right += sample * volume_right;}
   }

   if(CHANNEL4_ON)
   {
      s16 sample = 0;
      if(channel1_left_on) {sample_left += sample * volume_left;}
      if(channel1_right_on) {sample_right += sample * volume_right;}
   }

   u32 offset = destination->sample_index * SOUND_OUTPUT_CHANNEL_COUNT;

   *(destination->samples + offset + 0) = sample_left;
   *(destination->samples + offset + 1) = sample_right;

   destination->sample_index++;
}

struct cycle_clocks
{
   u32 cpu;
   u32 sound;
   u32 horizontal_sync;
};

#define SOUND_PERIOD (CPU_HZ / SOUND_OUTPUT_HZ)
#define HORIZONTAL_SYNC_PERIOD (CPU_HZ / HORIZONTAL_SYNC_HZ)

static void
cpu_tick(struct cycle_clocks *clocks, struct pixel_bitmap *bitmap, struct sound_samples *sound)
{
   if(!map.boot_complete && read_memory(0xFF50))
   {
      map.boot_complete = true;
   }

   handle_interrupts();
   u32 cycles = fetch_and_execute();

   clocks->cpu += cycles;
   clocks->sound += cycles;
   clocks->horizontal_sync += cycles;

   // NOTE(law): Sound clock.
   if(clocks->sound >= SOUND_PERIOD)
   {
      clocks->sound = 0;

#if DEBUG_SINE_WAVE
      generate_debug_samples(sound, 1);
#else
      generate_sound_sample(sound);
#endif
   }

   // NOTE(law): Horizontal sync clock.
   if(clocks->horizontal_sync >= HORIZONTAL_SYNC_PERIOD)
   {
      clocks->horizontal_sync = 0;

      u8 scanline = read_memory(0xFF44);
      if(scanline < 144)
      {
         render_scanline(bitmap, scanline);
      }

      if(++scanline > 153)
      {
         scanline = 0;
      }
      write_memory(0xFF44, scanline);
   }
}
