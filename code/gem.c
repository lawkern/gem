/* /////////////////////////////////////////////////////////////////////////// */
/* (c) copyright 2022 Lawrence D. Kern /////////////////////////////////////// */
/* /////////////////////////////////////////////////////////////////////////// */

#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define TAU 6.2831853071f

#define CPU_HZ 4194304
#define VERTICAL_SYNC_HZ 59.73f
#define HORIZONTAL_SYNC_HZ 9198

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

typedef struct
{
   size_t size;
   unsigned char *memory;
} Platform_File;

typedef struct
{
   unsigned char *base_address;
   size_t size;
   size_t used;
} Memory_Arena;

typedef struct
{
   size_t size;
   unsigned char *memory;
} Memory_Bank;

typedef enum
{
   MEMORY_BANKING_MODE_SIMPLE   = 0x00,
   MEMORY_BANKING_MODE_ADVANCED = 0x01,
} Memory_Banking_Mode;

typedef enum
{
   MEMORY_BANK_CONTROLLER_NONE,
   MEMORY_BANK_CONTROLLER_MBC1,
   MEMORY_BANK_CONTROLLER_MBC2,
} Memory_Bank_Controller;

static struct
{
   Memory_Bank rom_banks[512];
   Memory_Bank ram_banks[16];

   unsigned int rom_bank_count;
   unsigned int ram_bank_count;

   unsigned char rom_bank_mask;

   unsigned int rom_selected_index;
   unsigned int ram_selected_index;

   Memory_Banking_Mode banking_mode;
   unsigned char upper_rom_bank_bits;

   Memory_Bank vram;
   Memory_Bank wram;
   Memory_Bank oam;
   Memory_Bank io;
   Memory_Bank hram;

   unsigned char register_ie;

   Memory_Bank_Controller mbc;

   bool load_complete;
   bool boot_complete;
   bool ram_enabled;
} map;


#define PLATFORM_LOG(name) void name(char *format, ...)
#define PLATFORM_FREE_FILE(name) void name(Platform_File *file)
#define PLATFORM_LOAD_FILE(name) Platform_File name(char *file_path)

static PLATFORM_LOG(platform_log);
static PLATFORM_FREE_FILE(platform_free_file);
static PLATFORM_LOAD_FILE(platform_load_file);

#define ARRAY_LENGTH(array) (sizeof(array) / sizeof((array)[0]))
#define MAXIMUM(a, b) ((a) > (b) ? (a) : (b))

#define KIBIBYTES(value) (1024LL * (value))
#define MEBIBYTES(value) (1024LL * KIBIBYTES(value))

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

#define ALLOCATE(arena, Type) allocate((arena), sizeof(Type))

static void *
allocate(Memory_Arena *arena, size_t size)
{
   assert(size <= (arena->size - arena->used));

   void *result = arena->base_address + arena->used;
   arena->used += size;

   return(result);
}

static void
reset_arena(Memory_Arena *arena)
{
   arena->used = 0;
}

static void
zero_memory(void *memory, size_t size)
{
   // TODO(law): Remove dependency on stdlib!
   memset(memory, 0, size);
}

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

static unsigned char
read_memory(unsigned short address)
{
   // TODO(law): Condense this down once everything is working.

   unsigned char result = 0x00;

   if(address <= 0x3FFF)
   {
      // NOTE(law): ROM bank 00 (0000 - 3FFF).
      if(map.boot_complete || address >= 0x100)
      {
         unsigned char bank_index = 0;
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
      unsigned int selected_rom_index = MAXIMUM(map.rom_selected_index, 1);
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
write_memory(unsigned short address, char value)
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
         unsigned int extended_index = map.rom_selected_index + map.upper_rom_bank_bits;
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
         if((unsigned int)value < map.ram_bank_count)
         {
            map.ram_selected_index = (unsigned int)value;
         }
      }
      else
      {
         if((unsigned int)value < map.rom_bank_count)
         {
            // TODO(law): This calculation is handled differently for MBC1M
            // cartridges - it only shifts up by 4.
            map.upper_rom_bank_bits = (unsigned int)value << 5;
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

static unsigned short
read_memory16(unsigned short address)
{
   // TODO(law): Confirm the endian-ness here.
   unsigned char low  = read_memory(address + 0);
   unsigned char high = read_memory(address + 1);

   unsigned short result = ((unsigned short)high << 8) | low;
   return(result);
}

static void
write_memory16(unsigned short address, unsigned short value)
{
   // TODO(law): Confirm the endian-ness here.
   unsigned char low  = (value & 0xF);
   unsigned char high = (value >> 8);

   write_memory(address + 0, low);
   write_memory(address + 1, high);
}

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
allocate_memory_bank(Memory_Arena *arena, Memory_Bank *bank, size_t size)
{
   bank->size = size;
   bank->memory = allocate(arena, size);
}

static void
unload_cartridge(Memory_Arena *arena)
{
   reset_arena(arena);
   zero_memory(&map, sizeof(map));
   register_pc = 0;
}

static void
load_cartridge(Memory_Arena *arena, char *file_path)
{
   unload_cartridge(arena);

   platform_log("Loading ROM at \"%s\"...\n", file_path);
   Platform_File rom = platform_load_file(file_path);

   if(!rom.memory)
   {
      platform_log("ERROR: The ROM \"%s\" was not loaded.\n", file_path);
      return;
   }

   if(rom.size < (sizeof(boot_rom) + sizeof(Cartridge_Header)))
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

   Cartridge_Header *header = get_cartridge_header(rom.memory);

   map.rom_bank_mask = ~(0xFF << (header->rom_size + 1));
   map.rom_bank_count = (2 << header->rom_size);
   for(unsigned int bank_index = 0; bank_index < map.rom_bank_count; ++bank_index)
   {
      Memory_Bank *bank = map.rom_banks + bank_index;
      allocate_memory_bank(arena, bank, KIBIBYTES(16));
   }

   unsigned int ram_bank_counts[] = {0, 0, 1, 4, 16, 8};
   map.ram_bank_count = ram_bank_counts[header->ram_size];
   for(unsigned int bank_index = 0; bank_index < map.ram_bank_count; ++bank_index)
   {
      Memory_Bank *bank = map.ram_banks + bank_index;
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

   unsigned char *source = rom.memory;
   for(unsigned int bank_index = 0; bank_index < map.rom_bank_count; ++bank_index)
   {
      size_t size = map.rom_banks[bank_index].size;
      memcpy(map.rom_banks[bank_index].memory, source, size);
      source += size;
   }

   // NOTE(law): Ensure that this byte is set to zero on cartridge load. Setting
   // it to a non-zero value is what unmaps the boot code.
   write_memory(0xFF50, 0x0);

   platform_free_file(&rom);

   map.load_complete = true;
}

static unsigned short
disassemble_instruction(unsigned short address)
{
   unsigned short initial_address = address;

   // NOTE(law): Print the address of the current instruction.
   platform_log("0x%04X  ", address);

   unsigned char opcode = read_memory(address++);
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
            unsigned short operand = read_memory16(address);
            address += 2;
            platform_log("LD A, (0x%04X)      ", operand);
         } break;

         case 0x02: {platform_log("LD (BC), A          ");} break;
         case 0x12: {platform_log("LD (DE), A          ");} break;

         case 0xEA:
         {
            unsigned short operand = read_memory16(address);
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
            unsigned short operand = read_memory16(address);
            address += 2;
            platform_log("LD 0x%04X, SP       ", operand);
         } break;

         case 0x01:
         {
            unsigned short operand = read_memory16(address);
            address += 2;
            platform_log("LD BC, 0x%04X       ", operand);
         } break;

         case 0x11:
         {
            unsigned short operand = read_memory16(address);
            address += 2;
            platform_log("LD DE, 0x%04X       ", operand);
         } break;

         case 0x21:
         {
            unsigned short operand = read_memory16(address);
            address += 2;
            platform_log("LD HL, 0x%04X       ", operand);
         } break;

         case 0x31:
         {
            unsigned short operand = read_memory16(address);
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
            unsigned short operand = read_memory16(address);
            address += 2;
            platform_log("JP 0x%04X           ", operand);
         } break;

         case 0xC2:
         {
            unsigned short operand = read_memory16(address);
            address += 2;
            platform_log("JP NZ, 0x%04X       ", operand);
         } break;

         case 0xCA:
         {
            unsigned short operand = read_memory16(address);
            address += 2;
            platform_log("JP Z, 0x%04X        ", operand);
         } break;

         case 0xD2:
         {
            unsigned short operand = read_memory16(address);
            address += 2;
            platform_log("JP NC, 0x%04X       ", operand);
         } break;

         case 0xDA:
         {
            unsigned short operand = read_memory16(address);
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
            unsigned short operand = read_memory16(address);
            address += 2;
            platform_log("CALL NZ, 0x%04X     ", operand);
         } break;

         case 0xCC:
         {
            unsigned short operand = read_memory16(address);
            address += 2;
            platform_log("CALL Z, 0x%04X      ", operand);
         } break;

         case 0xCD:
         {
            unsigned short operand = read_memory16(address);
            address += 2;
            platform_log("CALL 0x%04X         ", operand);
         } break;

         case 0xD4:
         {
            unsigned short operand = read_memory16(address);
            address += 2;
            platform_log("CALL NC, 0x%04X     ", operand);
         } break;

         case 0xDC:
         {
            unsigned short operand = read_memory16(address);
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
   for(unsigned short index = initial_address; index < address; ++index)
   {
      platform_log("%02X ", read_memory(index));
   }

   platform_log("\n");

   unsigned short result = address - initial_address;
   return(result);
}

static void
disassemble_stream(unsigned short address, unsigned int byte_count)
{
   // TODO(law): 16-bit operations are not being endian swapped in this
   // function.

   unsigned int start = address;
   while((address - start) < byte_count)
   {
      address += disassemble_instruction(address);
   }
}

#define REGISTER_BC (((unsigned short)register_b << 8) | (unsigned short)register_c)
#define REGISTER_DE (((unsigned short)register_d << 8) | (unsigned short)register_e)
#define REGISTER_HL (((unsigned short)register_h << 8) | (unsigned short)register_l)
#define REGISTER_AF (((unsigned short)register_a << 8) | (unsigned short)register_f)

#define REGISTER_IE 0xFFFF
#define REGISTER_IF 0xFF0F

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

#define ENABLE_VBLANK   ((read_memory(REGISTER_IE) >> INTERRUPT_VBLANK_BIT)   & 0x1)
#define ENABLE_LCD_STAT ((read_memory(REGISTER_IE) >> INTERRUPT_LCD_STAT_BIT) & 0x1)
#define ENABLE_TIMER    ((read_memory(REGISTER_IE) >> INTERRUPT_TIMER_BIT)    & 0x1)
#define ENABLE_SERIAL   ((read_memory(REGISTER_IE) >> INTERRUPT_SERIAL_BIT)   & 0x1)
#define ENABLE_JOYPAD   ((read_memory(REGISTER_IE) >> INTERRUPT_JOYPAD_BIT)   & 0x1)

#define REQUEST_VBLANK   ((read_memory(REGISTER_IF) >> INTERRUPT_VBLANK_BIT)   & 0x1)
#define REQUEST_LCD_STAT ((read_memory(REGISTER_IF) >> INTERRUPT_LCD_STAT_BIT) & 0x1)
#define REQUEST_TIMER    ((read_memory(REGISTER_IF) >> INTERRUPT_TIMER_BIT)    & 0x1)
#define REQUEST_SERIAL   ((read_memory(REGISTER_IF) >> INTERRUPT_SERIAL_BIT)   & 0x1)
#define REQUEST_JOYPAD   ((read_memory(REGISTER_IF) >> INTERRUPT_JOYPAD_BIT)   & 0x1)

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

static unsigned char
inc(unsigned char value)
{
   // NOTE(law): Increment

   unsigned char half_sum = (value & 0xF) + 1;

   value += 1;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag when a carry from bit 3 occurs.
   register_f = (register_f & ~FLAG_H_MASK) | (((half_sum & 0x10) == 0x10) << FLAG_H_BIT);

   // NOTE(law): The Carry flag is not affected.

   return(value);
}

static unsigned char
dec(unsigned char value)
{
   // NOTE(law): Decrement

   bool is_half_negative = (signed char)(value & 0xF) < 1;

   value -= 1;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always set.
   register_f |= FLAG_N_MASK;

   // NOTE(law): Set the Half Carry flag when a carry from bit 3 occurs.
   register_f = (register_f & ~FLAG_H_MASK) | (is_half_negative << FLAG_H_BIT);

   // NOTE(law): The Carry flag is not affected.

   return(value);
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
jp(bool should_jump)
{
   unsigned char address_low  = read_memory(register_pc++);
   unsigned char address_high = read_memory(register_pc++);

   if(should_jump)
   {
      unsigned short address = ((unsigned short)address_high << 8) | (unsigned short)address_low;
      register_pc = address;
   }
}

static void
jr(bool should_jump)
{
   signed char offset = read_memory(register_pc++);

   if(should_jump)
   {
      signed short address = (signed short)register_pc + (signed short)offset;
      register_pc = (unsigned short)address;
   }
}

static void
call(bool should_jump)
{
   unsigned char address_low  = read_memory(register_pc++);
   unsigned char address_high = read_memory(register_pc++);

   if(should_jump)
   {
      write_memory(--register_sp, register_pc >> 8);
      write_memory(--register_sp, register_pc & 0xFF);

      unsigned short address = ((unsigned short)address_high << 8) | (unsigned short)address_low;
      register_pc = address;
   }
}

static void
ret(bool should_jump)
{
   unsigned char address_low  = read_memory(register_sp++);
   unsigned char address_high = read_memory(register_sp++);

   if(should_jump)
   {
      unsigned short address = ((unsigned short)address_high << 8) | (unsigned short)address_low;
      register_pc = address;
   }
}

static void
rst(unsigned char address_low)
{
   write_memory(--register_sp, register_pc >> 8);
   write_memory(--register_sp, register_pc & 0xFF);

   unsigned short address = (unsigned short)address_low;
   register_pc = address;
}

static unsigned char
rl(unsigned char value)
{
   // NOTE(law): Rotate Left

   unsigned char previous_bit7 = (value >> 7);
   unsigned char previous_c = FLAG_C;

   value <<= 1;

   // NOTE(law): Set bit 0 to the pre-shift value of the Carry flag (a value of zero
   // should have already been shifted into position zero).
   value |= (previous_c << 0);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 7.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit7 << FLAG_C_BIT);

   return(value);
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

static unsigned char
rlc(unsigned char value)
{
   // NOTE(law): Rotate Left Circular

   unsigned char previous_bit7 = (value >> 7);

   value <<= 1;

   // NOTE(law): Set bit 0 to the pre-shift value of bit 7 (a value of zero
   // should have already been shifted into position zero).
   value |= (previous_bit7 << 0);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 7.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit7 << FLAG_C_BIT);

   return(value);
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

static unsigned char
rr(unsigned char value)
{
   // NOTE(law): Rotate Right

   unsigned char previous_bit0 = (value & 0x01);
   unsigned char previous_c = FLAG_C;

   value >>= 1;

   // NOTE(law): Set bit 7 to the pre-shift value of the Carry flag (a value of zero
   // should have already been shifted into position seven).
   value |= (previous_c << 7);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);

   return(value);
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

static unsigned char
rrc(unsigned char value)
{
   // NOTE(law): Rotate Right Circular

   unsigned char previous_bit0 = (value & 0x01);

   value >>= 1;

   // NOTE(law): Set bit 7 to the pre-shift value of bit 0 (a value of zero
   // should have already been shifted into position seven).
   value |= (previous_bit0 << 7);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_N_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);

   return(value);
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

static unsigned char
set(unsigned int bit_index, unsigned char value)
{
   value |= (1 << bit_index);
   return(value);
}

static unsigned char
res(unsigned int bit_index, unsigned char value)
{
   value &= ~(1 << bit_index);
   return(value);
}

static unsigned char
sla(unsigned char value)
{
   // NOTE(law): Shift Left Arithmetic

   unsigned char previous_bit7 = (value >> 7);

   value <<= 1;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 7.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit7 << FLAG_C_BIT);

   return(value);
}

static unsigned char
sra(unsigned char value)
{
   // NOTE(law): Shift Right Arithmetic

   unsigned char previous_bit0 = (value & 0x01);

   // TODO(law): Confirm that casting to a signed value actually produces an
   // arithmetic shift in this case.
   value = ((signed short)value >> 1);

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);

   return(value);
}

static unsigned char
swap(unsigned char value)
{
   unsigned char high_nibble = (value >> 4);
   unsigned char low_nibble = (value & 0xF);

   value = (low_nibble << 4) | high_nibble;
   return(value);
}

static unsigned char
srl(unsigned char value)
{
   // NOTE(law): Shift Right Logical

   unsigned char previous_bit0 = (value & 0x01);

   // TODO(law): Confirm that using an unsigned value actually produces a
   // logical shift in this case.
   value >>= 1;

   // NOTE(law): Set the Zero flag if the resulting computation produced a zero.
   register_f = (register_f & ~FLAG_Z_MASK) | ((value == 0) << FLAG_Z_BIT);

   // NOTE(law): The Subtraction flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): The Half Carry flag is always reset.
   register_f &= ~FLAG_H_MASK;

   // NOTE(law): Set the Carry flag to the pre-shift value of bit 0.
   register_f = (register_f & ~FLAG_C_MASK) | (previous_bit0 << FLAG_C_BIT);

   return(value);
}

typedef struct
{
   unsigned int count;
   unsigned int failed_condition_count;
} Opcode_Cycle_Count;

Opcode_Cycle_Count nonprefix_cycle_counts[] =
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

Opcode_Cycle_Count cbprefix_cycle_counts[] =
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

static unsigned int
fetch_and_execute()
{
   unsigned char opcode = read_memory(register_pc++);
   bool condition_succeeded = true;

   bool prefix_opcode = (opcode == 0xCB);
   if(prefix_opcode)
   {
      // NOTE(law): Parse prefix instructions.
      opcode = read_memory(register_pc++);
      switch(opcode)
      {
         // NOTE(law): Rotate and Shift instructions
         case 0x00: {register_b = rlc(register_b);} break;
         case 0x01: {register_c = rlc(register_c);} break;
         case 0x02: {register_d = rlc(register_d);} break;
         case 0x03: {register_e = rlc(register_e);} break;
         case 0x04: {register_h = rlc(register_h);} break;
         case 0x05: {register_l = rlc(register_l);} break;
         case 0x06: {write_memory(REGISTER_HL, rlc(read_memory(REGISTER_HL)));} break;
         case 0x07: {register_a = rlc(register_a);} break;

         case 0x08: {register_b = rrc(register_b);} break;
         case 0x09: {register_c = rrc(register_c);} break;
         case 0x0A: {register_d = rrc(register_d);} break;
         case 0x0B: {register_e = rrc(register_e);} break;
         case 0x0C: {register_h = rrc(register_h);} break;
         case 0x0D: {register_l = rrc(register_l);} break;
         case 0x0E: {write_memory(REGISTER_HL, rrc(read_memory(REGISTER_HL)));} break;
         case 0x0F: {register_a = rrc(register_a);} break;

         case 0x10: {register_b = rl(register_b);} break;
         case 0x11: {register_c = rl(register_c);} break;
         case 0x12: {register_d = rl(register_d);} break;
         case 0x13: {register_e = rl(register_e);} break;
         case 0x14: {register_h = rl(register_h);} break;
         case 0x15: {register_l = rl(register_l);} break;
         case 0x16: {write_memory(REGISTER_HL, rl(read_memory(REGISTER_HL)));} break;
         case 0x17: {register_a = rl(register_a);} break;

         case 0x18: {register_b = rr(register_b);} break;
         case 0x19: {register_c = rr(register_c);} break;
         case 0x1A: {register_d = rr(register_d);} break;
         case 0x1B: {register_e = rr(register_e);} break;
         case 0x1C: {register_h = rr(register_h);} break;
         case 0x1D: {register_l = rr(register_l);} break;
         case 0x1E: {write_memory(REGISTER_HL, rr(read_memory(REGISTER_HL)));} break;
         case 0x1F: {register_a = rr(register_a);} break;

         case 0x20: {register_b = sla(register_b);} break;
         case 0x21: {register_c = sla(register_c);} break;
         case 0x22: {register_d = sla(register_d);} break;
         case 0x23: {register_e = sla(register_e);} break;
         case 0x24: {register_h = sla(register_h);} break;
         case 0x25: {register_l = sla(register_l);} break;
         case 0x26: {write_memory(REGISTER_HL, sla(read_memory(REGISTER_HL)));} break;
         case 0x27: {register_a = sla(register_a);} break;

         case 0x28: {register_b = sra(register_b);} break;
         case 0x29: {register_c = sra(register_c);} break;
         case 0x2A: {register_d = sra(register_d);} break;
         case 0x2B: {register_e = sra(register_e);} break;
         case 0x2C: {register_h = sra(register_h);} break;
         case 0x2D: {register_l = sra(register_l);} break;
         case 0x2E: {write_memory(REGISTER_HL, sra(read_memory(REGISTER_HL)));} break;
         case 0x2F: {register_a = sra(register_a);} break;

         case 0x30: {register_b = swap(register_b);} break;
         case 0x31: {register_c = swap(register_c);} break;
         case 0x32: {register_d = swap(register_d);} break;
         case 0x33: {register_e = swap(register_e);} break;
         case 0x34: {register_h = swap(register_h);} break;
         case 0x35: {register_l = swap(register_l);} break;
         case 0x36: {write_memory(REGISTER_HL, swap(read_memory(REGISTER_HL)));} break;
         case 0x37: {register_a = swap(register_a);} break;

         case 0x38: {register_b = srl(register_b);} break;
         case 0x39: {register_c = srl(register_c);} break;
         case 0x3A: {register_d = srl(register_d);} break;
         case 0x3B: {register_e = srl(register_e);} break;
         case 0x3C: {register_h = srl(register_h);} break;
         case 0x3D: {register_l = srl(register_l);} break;
         case 0x3E: {write_memory(REGISTER_HL, srl(read_memory(REGISTER_HL)));} break;
         case 0x3F: {register_a = srl(register_a);} break;


         // NOTE(law): Single-bit Operation instructions
         case 0x40: bit(0, register_b); break;
         case 0x41: bit(0, register_c); break;
         case 0x42: bit(0, register_d); break;
         case 0x43: bit(0, register_e); break;
         case 0x44: bit(0, register_h); break;
         case 0x45: bit(0, register_l); break;
         case 0x46: bit(0, read_memory(REGISTER_HL)); break;
         case 0x47: bit(0, register_a); break;

         case 0x48: bit(1, register_b); break;
         case 0x49: bit(1, register_c); break;
         case 0x4A: bit(1, register_d); break;
         case 0x4B: bit(1, register_e); break;
         case 0x4C: bit(1, register_h); break;
         case 0x4D: bit(1, register_l); break;
         case 0x4E: bit(1, read_memory(REGISTER_HL)); break;
         case 0x4F: bit(1, register_a); break;

         case 0x50: bit(2, register_b); break;
         case 0x51: bit(2, register_c); break;
         case 0x52: bit(2, register_d); break;
         case 0x53: bit(2, register_e); break;
         case 0x54: bit(2, register_h); break;
         case 0x55: bit(2, register_l); break;
         case 0x56: bit(2, read_memory(REGISTER_HL)); break;
         case 0x57: bit(2, register_a); break;

         case 0x58: bit(3, register_b); break;
         case 0x59: bit(3, register_c); break;
         case 0x5A: bit(3, register_d); break;
         case 0x5B: bit(3, register_e); break;
         case 0x5C: bit(3, register_h); break;
         case 0x5D: bit(3, register_l); break;
         case 0x5E: bit(3, read_memory(REGISTER_HL)); break;
         case 0x5F: bit(3, register_a); break;

         case 0x60: bit(4, register_b); break;
         case 0x61: bit(4, register_c); break;
         case 0x62: bit(4, register_d); break;
         case 0x63: bit(4, register_e); break;
         case 0x64: bit(4, register_h); break;
         case 0x65: bit(4, register_l); break;
         case 0x66: bit(4, read_memory(REGISTER_HL)); break;
         case 0x67: bit(4, register_a); break;

         case 0x68: bit(5, register_b); break;
         case 0x69: bit(5, register_c); break;
         case 0x6A: bit(5, register_d); break;
         case 0x6B: bit(5, register_e); break;
         case 0x6C: bit(5, register_h); break;
         case 0x6D: bit(5, register_l); break;
         case 0x6E: bit(5, read_memory(REGISTER_HL)); break;
         case 0x6F: bit(5, register_a); break;

         case 0x70: bit(6, register_b); break;
         case 0x71: bit(6, register_c); break;
         case 0x72: bit(6, register_d); break;
         case 0x73: bit(6, register_e); break;
         case 0x74: bit(6, register_h); break;
         case 0x75: bit(6, register_l); break;
         case 0x76: bit(6, read_memory(REGISTER_HL)); break;
         case 0x77: bit(6, register_a); break;

         case 0x78: bit(7, register_b); break;
         case 0x79: bit(7, register_c); break;
         case 0x7A: bit(7, register_d); break;
         case 0x7B: bit(7, register_e); break;
         case 0x7C: bit(7, register_h); break;
         case 0x7D: bit(7, register_l); break;
         case 0x7E: bit(7, read_memory(REGISTER_HL)); break;
         case 0x7F: bit(7, register_a); break;

         case 0x80: {register_b = res(0, register_b);} break;
         case 0x81: {register_c = res(0, register_c);} break;
         case 0x82: {register_d = res(0, register_d);} break;
         case 0x83: {register_e = res(0, register_e);} break;
         case 0x84: {register_h = res(0, register_h);} break;
         case 0x85: {register_l = res(0, register_l);} break;
         case 0x86: {write_memory(REGISTER_HL, res(0, read_memory(REGISTER_HL)));} break;
         case 0x87: {register_a = res(0, register_a);} break;

         case 0x88: {register_b = res(1, register_b);} break;
         case 0x89: {register_c = res(1, register_c);} break;
         case 0x8A: {register_d = res(1, register_d);} break;
         case 0x8B: {register_e = res(1, register_e);} break;
         case 0x8C: {register_h = res(1, register_h);} break;
         case 0x8D: {register_l = res(1, register_l);} break;
         case 0x8E: {write_memory(REGISTER_HL, res(1, read_memory(REGISTER_HL)));} break;
         case 0x8F: {register_a = res(1, register_a);} break;

         case 0x90: {register_b = res(2, register_b);} break;
         case 0x91: {register_c = res(2, register_c);} break;
         case 0x92: {register_d = res(2, register_d);} break;
         case 0x93: {register_e = res(2, register_e);} break;
         case 0x94: {register_h = res(2, register_h);} break;
         case 0x95: {register_l = res(2, register_l);} break;
         case 0x96: {write_memory(REGISTER_HL, res(2, read_memory(REGISTER_HL)));} break;
         case 0x97: {register_a = res(2, register_a);} break;

         case 0x98: {register_b = res(3, register_b);} break;
         case 0x99: {register_c = res(3, register_c);} break;
         case 0x9A: {register_d = res(3, register_d);} break;
         case 0x9B: {register_e = res(3, register_e);} break;
         case 0x9C: {register_h = res(3, register_h);} break;
         case 0x9D: {register_l = res(3, register_l);} break;
         case 0x9E: {write_memory(REGISTER_HL, res(3, read_memory(REGISTER_HL)));} break;
         case 0x9F: {register_a = res(3, register_a);} break;

         case 0xA0: {register_b = res(4, register_b);} break;
         case 0xA1: {register_c = res(4, register_c);} break;
         case 0xA2: {register_d = res(4, register_d);} break;
         case 0xA3: {register_e = res(4, register_e);} break;
         case 0xA4: {register_h = res(4, register_h);} break;
         case 0xA5: {register_l = res(4, register_l);} break;
         case 0xA6: {write_memory(REGISTER_HL, res(4, read_memory(REGISTER_HL)));} break;
         case 0xA7: {register_a = res(4, register_a);} break;

         case 0xA8: {register_b = res(5, register_b);} break;
         case 0xA9: {register_c = res(5, register_c);} break;
         case 0xAA: {register_d = res(5, register_d);} break;
         case 0xAB: {register_e = res(5, register_e);} break;
         case 0xAC: {register_h = res(5, register_h);} break;
         case 0xAD: {register_l = res(5, register_l);} break;
         case 0xAE: {write_memory(REGISTER_HL, res(5, read_memory(REGISTER_HL)));} break;
         case 0xAF: {register_a = res(5, register_a);} break;

         case 0xB0: {register_b = res(6, register_b);} break;
         case 0xB1: {register_c = res(6, register_c);} break;
         case 0xB2: {register_d = res(6, register_d);} break;
         case 0xB3: {register_e = res(6, register_e);} break;
         case 0xB4: {register_h = res(6, register_h);} break;
         case 0xB5: {register_l = res(6, register_l);} break;
         case 0xB6: {write_memory(REGISTER_HL, res(6, read_memory(REGISTER_HL)));} break;
         case 0xB7: {register_a = res(6, register_a);} break;

         case 0xB8: {register_b = res(7, register_b);} break;
         case 0xB9: {register_c = res(7, register_c);} break;
         case 0xBA: {register_d = res(7, register_d);} break;
         case 0xBB: {register_e = res(7, register_e);} break;
         case 0xBC: {register_h = res(7, register_h);} break;
         case 0xBD: {register_l = res(7, register_l);} break;
         case 0xBE: {write_memory(REGISTER_HL, res(7, read_memory(REGISTER_HL)));} break;
         case 0xBF: {register_a = res(7, register_a);} break;

         case 0xC0: {register_b = res(0, register_b);} break;
         case 0xC1: {register_c = set(0, register_c);} break;
         case 0xC2: {register_d = set(0, register_d);} break;
         case 0xC3: {register_e = set(0, register_e);} break;
         case 0xC4: {register_h = set(0, register_h);} break;
         case 0xC5: {register_l = set(0, register_l);} break;
         case 0xC6: {write_memory(REGISTER_HL, set(0, read_memory(REGISTER_HL)));} break;
         case 0xC7: {register_a = set(0, register_a);} break;

         case 0xC8: {register_b = set(1, register_b);} break;
         case 0xC9: {register_c = set(1, register_c);} break;
         case 0xCA: {register_d = set(1, register_d);} break;
         case 0xCB: {register_e = set(1, register_e);} break;
         case 0xCC: {register_h = set(1, register_h);} break;
         case 0xCD: {register_l = set(1, register_l);} break;
         case 0xCE: {write_memory(REGISTER_HL, set(1, read_memory(REGISTER_HL)));} break;
         case 0xCF: {register_a = set(1, register_a);} break;

         case 0xD0: {register_b = set(2, register_b);} break;
         case 0xD1: {register_c = set(2, register_c);} break;
         case 0xD2: {register_d = set(2, register_d);} break;
         case 0xD3: {register_e = set(2, register_e);} break;
         case 0xD4: {register_h = set(2, register_h);} break;
         case 0xD5: {register_l = set(2, register_l);} break;
         case 0xD6: {write_memory(REGISTER_HL, set(2, read_memory(REGISTER_HL)));} break;
         case 0xD7: {register_a = set(2, register_a);} break;

         case 0xD8: {register_b = set(3, register_b);} break;
         case 0xD9: {register_c = set(3, register_c);} break;
         case 0xDA: {register_d = set(3, register_d);} break;
         case 0xDB: {register_e = set(3, register_e);} break;
         case 0xDC: {register_h = set(3, register_h);} break;
         case 0xDD: {register_l = set(3, register_l);} break;
         case 0xDE: {write_memory(REGISTER_HL, set(3, read_memory(REGISTER_HL)));} break;
         case 0xDF: {register_a = set(3, register_a);} break;

         case 0xE0: {register_b = set(4, register_b);} break;
         case 0xE1: {register_c = set(4, register_c);} break;
         case 0xE2: {register_d = set(4, register_d);} break;
         case 0xE3: {register_e = set(4, register_e);} break;
         case 0xE4: {register_h = set(4, register_h);} break;
         case 0xE5: {register_l = set(4, register_l);} break;
         case 0xE6: {write_memory(REGISTER_HL, set(4, read_memory(REGISTER_HL)));} break;
         case 0xE7: {register_a = set(4, register_a);} break;

         case 0xE8: {register_b = set(5, register_b);} break;
         case 0xE9: {register_c = set(5, register_c);} break;
         case 0xEA: {register_d = set(5, register_d);} break;
         case 0xEB: {register_e = set(5, register_e);} break;
         case 0xEC: {register_h = set(5, register_h);} break;
         case 0xED: {register_l = set(5, register_l);} break;
         case 0xEE: {write_memory(REGISTER_HL, set(5, read_memory(REGISTER_HL)));} break;
         case 0xEF: {register_a = set(5, register_a);} break;

         case 0xF0: {register_b = set(6, register_b);} break;
         case 0xF1: {register_c = set(6, register_c);} break;
         case 0xF2: {register_d = set(6, register_d);} break;
         case 0xF3: {register_e = set(6, register_e);} break;
         case 0xF4: {register_h = set(6, register_h);} break;
         case 0xF5: {register_l = set(6, register_l);} break;
         case 0xF6: {write_memory(REGISTER_HL, set(6, read_memory(REGISTER_HL)));} break;
         case 0xF7: {register_a = set(6, register_a);} break;

         case 0xF8: {register_b = set(7, register_b);} break;
         case 0xF9: {register_c = set(7, register_c);} break;
         case 0xFA: {register_d = set(7, register_d);} break;
         case 0xFB: {register_e = set(7, register_e);} break;
         case 0xFC: {register_h = set(7, register_h);} break;
         case 0xFD: {register_l = set(7, register_l);} break;
         case 0xFE: {write_memory(REGISTER_HL, set(7, read_memory(REGISTER_HL)));} break;
         case 0xFF: {register_a = set(7, register_a);} break;

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
         case 0x46: {register_b = read_memory(REGISTER_HL);} break;
         case 0x47: {register_b = register_a;} break;

         case 0x48: {register_c = register_b;} break;
         case 0x49: {/*register_c = register_c;*/} break;
         case 0x4A: {register_c = register_d;} break;
         case 0x4B: {register_c = register_e;} break;
         case 0x4C: {register_c = register_h;} break;
         case 0x4D: {register_c = register_l;} break;
         case 0x4E: {register_c = read_memory(REGISTER_HL);} break;
         case 0x4F: {register_c = register_a;} break;

         case 0x50: {register_d = register_b;} break;
         case 0x51: {register_d = register_c;} break;
         case 0x52: {/*register_d = register_d;*/} break;
         case 0x53: {register_d = register_e;} break;
         case 0x54: {register_d = register_h;} break;
         case 0x55: {register_d = register_l;} break;
         case 0x56: {register_d = read_memory(REGISTER_HL);} break;
         case 0x57: {register_d = register_a;} break;

         case 0x58: {register_e = register_b;} break;
         case 0x59: {register_e = register_c;} break;
         case 0x5A: {register_e = register_d;} break;
         case 0x5B: {/*register_e = register_e;*/} break;
         case 0x5C: {register_e = register_h;} break;
         case 0x5D: {register_e = register_l;} break;
         case 0x5E: {register_e = read_memory(REGISTER_HL);} break;
         case 0x5F: {register_e = register_a;} break;

         case 0x60: {register_h = register_b;} break;
         case 0x61: {register_h = register_c;} break;
         case 0x62: {register_h = register_d;} break;
         case 0x63: {register_h = register_e;} break;
         case 0x64: {/*register_h = register_h;*/} break;
         case 0x65: {register_h = register_l;} break;
         case 0x66: {register_h = read_memory(REGISTER_HL);} break;
         case 0x67: {register_h = register_a;} break;

         case 0x68: {register_l = register_b;} break;
         case 0x69: {register_l = register_c;} break;
         case 0x6A: {register_l = register_d;} break;
         case 0x6B: {register_l = register_e;} break;
         case 0x6C: {register_l = register_h;} break;
         case 0x6D: {/*register_l = register_l;*/} break;
         case 0x6E: {register_l = read_memory(REGISTER_HL);} break;
         case 0x6F: {register_l = register_a;} break;

         case 0x70: {write_memory(REGISTER_HL, register_b);} break;
         case 0x71: {write_memory(REGISTER_HL, register_c);} break;
         case 0x72: {write_memory(REGISTER_HL, register_d);} break;
         case 0x73: {write_memory(REGISTER_HL, register_e);} break;
         case 0x74: {write_memory(REGISTER_HL, register_h);} break;
         case 0x75: {write_memory(REGISTER_HL, register_l);} break;
         case 0x77: {write_memory(REGISTER_HL, register_a);} break;

         case 0x78: {register_a = register_b;} break;
         case 0x79: {register_a = register_c;} break;
         case 0x7A: {register_a = register_d;} break;
         case 0x7B: {register_a = register_e;} break;
         case 0x7C: {register_a = register_h;} break;
         case 0x7D: {register_a = register_l;} break;
         case 0x7E: {register_a = read_memory(REGISTER_HL);} break;
         case 0x7F: {/*register_a = register_a;*/} break;

         case 0x06: {register_b = read_memory(register_pc++);} break; // LD B, n
         case 0x0E: {register_c = read_memory(register_pc++);} break; // LD C, n
         case 0x1E: {register_e = read_memory(register_pc++);} break; // LD E, n
         case 0x16: {register_d = read_memory(register_pc++);} break; // LD D, n
         case 0x26: {register_h = read_memory(register_pc++);} break; // LD H, n
         case 0x2E: {register_l = read_memory(register_pc++);} break; // LD L, n
         case 0x36: {write_memory(REGISTER_HL, read_memory(register_pc++));} break; // LD (HL), n
         case 0x3E: {register_a = read_memory(register_pc++);} break; // LD A, n

         case 0x0A: {register_a = read_memory(REGISTER_BC);} break; // LD A, (BC)
         case 0x1A: {register_a = read_memory(REGISTER_DE);} break; // LD A, (DE)

         case 0xFA: // LD A, (nn)
         {
            unsigned char address_low  = read_memory(register_pc++);
            unsigned char address_high = read_memory(register_pc++);
            unsigned short address = ((unsigned short)address_high << 8) | ((unsigned short)address_low);

            register_a = read_memory(address);
         } break;

         case 0x02: {write_memory(REGISTER_BC, register_a);} break; // LD (BC), A
         case 0x12: {write_memory(REGISTER_DE, register_a);} break; // LD (DE), A

         case 0xEA: // LD (nn), A
         {
            unsigned char address_low  = read_memory(register_pc++);
            unsigned char address_high = read_memory(register_pc++);
            unsigned short address = ((unsigned short)address_high << 8) | ((unsigned short)address_low);

            write_memory(address, register_a);
         } break;

         case 0xF2: {register_a = read_memory(0xFF00 + register_c);} break;  // LDH A, (0xFF00 + C)
         case 0xE2: {write_memory(0xFF00 + register_c, register_a);} break; // LDH (0xFF00 + C), A

         case 0xF0: {register_a = read_memory(0xFF00 + read_memory(register_pc++));} break;  // LDH A, (0xFF00 + n)
         case 0xE0: {write_memory(0xFF00 + read_memory(register_pc++), register_a);} break; // LDH (0xFF00 + n), A

         case 0x22: // LDI (HL), A
         {
            write_memory(REGISTER_HL, register_a);
            unsigned short updated_value = REGISTER_HL + 1;

            register_h = updated_value >> 8;
            register_l = updated_value & 0xFF;
         } break;

         case 0x32: // LDD (HL), A
         {
            write_memory(REGISTER_HL, register_a);
            unsigned short updated_value = REGISTER_HL - 1;

            register_h = updated_value >> 8;
            register_l = updated_value & 0xFF;
         } break;

         case 0x2A: // LDI A, (HL)
         {
            register_a = read_memory(REGISTER_HL);
            unsigned short updated_value = REGISTER_HL + 1;

            register_h = updated_value >> 8;
            register_l = updated_value & 0xFF;
         } break;

         case 0x3A: // LDD A, (HL)
         {
            register_a = read_memory(REGISTER_HL);
            unsigned short updated_value = REGISTER_HL - 1;

            register_h = updated_value >> 8;
            register_l = updated_value & 0xFF;
         } break;

         case 0xF9: {register_sp = REGISTER_HL;} break; // LD SP, HL

         case 0xC5: // PUSH BC
         {
            write_memory(--register_sp, register_b);
            write_memory(--register_sp, register_c);
         } break;

         case 0xD5: // PUSH DE
         {
            write_memory(--register_sp, register_d);
            write_memory(--register_sp, register_e);
         } break;

         case 0xE5: // PUSH HL
         {
            write_memory(--register_sp, register_h);
            write_memory(--register_sp, register_l);
         } break;

         case 0xF5: // PUSH AP
         {
            write_memory(--register_sp, register_a);
            write_memory(--register_sp, register_f);
         } break;

         case 0xC1: // POP BC
         {
            register_c = read_memory(register_sp++);
            register_b = read_memory(register_sp++);
         } break;

         case 0xD1: // POP DE
         {
            register_e = read_memory(register_sp++);
            register_d = read_memory(register_sp++);
         } break;

         case 0xE1: //POP HL
         {
            register_l = read_memory(register_sp++);
            register_h = read_memory(register_sp++);
         } break;

         case 0xF1: // POP AF
         {
            register_f = read_memory(register_sp++);
            register_a = read_memory(register_sp++);
         } break;


         // NOTE(law): 16-bit load instructions
         case 0x08: // LD (nn), SP
         {
            unsigned char address_low  = read_memory(register_pc++);
            unsigned char address_high = read_memory(register_pc++);
            unsigned short address = ((unsigned short)address_high << 8) | ((unsigned short)address_low);

            write_memory16(address, register_sp);
         } break;

         case 0x01: // LD BC, nn
         {
            register_c = read_memory(register_pc++);
            register_b = read_memory(register_pc++);
         } break;

         case 0x11: // LD DE, nn
         {
            register_e = read_memory(register_pc++);
            register_d = read_memory(register_pc++);
         } break;

         case 0x21: // LD HL, nn
         {
            register_l = read_memory(register_pc++);
            register_h = read_memory(register_pc++);
         } break;

         case 0x31: // LD SP, nn
         {
            unsigned char value_low  = read_memory(register_pc++);
            unsigned char value_high = read_memory(register_pc++);
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
         case 0x86: add(read_memory(REGISTER_HL)); break; // ADD A, (HL)
         case 0x87: add(register_a); break; // ADD A, A

         case 0xC6: add(read_memory(register_pc++)); break; // ADD A, n

         case 0x88: adc(register_b); break; // ADC A, B
         case 0x89: adc(register_c); break; // ADC A, C
         case 0x8A: adc(register_d); break; // ADC A, D
         case 0x8B: adc(register_e); break; // ADC A, E
         case 0x8C: adc(register_h); break; // ADC A, H
         case 0x8D: adc(register_l); break; // ADC A, L
         case 0x8E: adc(read_memory(REGISTER_HL)); break; // ADC A, (HL)
         case 0x8F: adc(register_a); break; // ADC A, A

         case 0xCE: {adc(read_memory(register_pc++));} break; // ADC A, n

         case 0x90: sub(register_b); break; // SUB A, B
         case 0x91: sub(register_c); break; // SUB A, C
         case 0x92: sub(register_d); break; // SUB A, D
         case 0x93: sub(register_e); break; // SUB A, E
         case 0x94: sub(register_h); break; // SUB A, H
         case 0x95: sub(register_l); break; // SUB A, L
         case 0x96: sub(read_memory(REGISTER_HL)); break; // SUB A, (HL)
         case 0x97: sub(register_a); break; // SUB A, A

         case 0xD6: sub(read_memory(register_pc++)); break; // SUB A, n

         case 0x98: sbc(register_b); break; // SBC A, B
         case 0x99: sbc(register_c); break; // SBC A, C
         case 0x9A: sbc(register_d); break; // SBC A, D
         case 0x9B: sbc(register_e); break; // SBC A, E
         case 0x9C: sbc(register_h); break; // SBC A, H
         case 0x9D: sbc(register_l); break; // SBC A, L
         case 0x9E: sbc(read_memory(REGISTER_HL)); break; // SBC A, (HL)
         case 0x9F: sbc(register_a); break; // SBC A, A

         case 0xDE: sbc(read_memory(register_pc++)); break; // SBC A, n

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
         case 0xAE: xor(read_memory(REGISTER_HL)); break; // XOR A, (HL)
         case 0xAF: xor(register_a); break; // XOR A, A

         case 0xEE: xor(read_memory(register_pc++)); break; // XOR A, n

         case 0xB0: or(register_b); break; // OR A, B
         case 0xB1: or(register_c); break; // OR A, C
         case 0xB2: or(register_d); break; // OR A, D
         case 0xB3: or(register_e); break; // OR A, E
         case 0xB4: or(register_h); break; // OR A, H
         case 0xB5: or(register_l); break; // OR A, L
         case 0xB6: or(read_memory(REGISTER_HL)); break; // OR A, (HL)
         case 0xB7: or(register_a); break; // OR A, A

         case 0xF6: or(read_memory(register_pc++)); break; // OR A, n

         case 0xA0: and(register_b); break; // AND A, B
         case 0xA1: and(register_c); break; // AND A, C
         case 0xA2: and(register_d); break; // AND A, D
         case 0xA3: and(register_e); break; // AND A, E
         case 0xA4: and(register_h); break; // AND A, H
         case 0xA5: and(register_l); break; // AND A, L
         case 0xA6: and(read_memory(REGISTER_HL)); break; // AND A, (HL)
         case 0xA7: and(register_a); break; // AND A, A

         case 0xE6: and(read_memory(register_pc++)); break; // AND A, n

         case 0xB8: cp(register_b); break; // CP A, B
         case 0xB9: cp(register_c); break; // CP A, C
         case 0xBA: cp(register_d); break; // CP A, D
         case 0xBB: cp(register_e); break; // CP A, E
         case 0xBC: cp(register_h); break; // CP A, H
         case 0xBD: cp(register_l); break; // CP A, L
         case 0xBE: cp(read_memory(REGISTER_HL)); break; // CP A, (HL)
         case 0xBF: cp(register_a); break; // CP A, A

         case 0xFE: cp(read_memory(register_pc++)); break; // CP A, n

         case 0x04: {register_b = inc(register_b);} break; // INC B
         case 0x0C: {register_c = inc(register_c);} break; // INC C
         case 0x14: {register_d = inc(register_d);} break; // INC D
         case 0x1C: {register_e = inc(register_e);} break; // INC E
         case 0x24: {register_h = inc(register_h);} break; // INC H
         case 0x2C: {register_l = inc(register_l);} break; // INC L
         case 0x34: {write_memory(REGISTER_HL, inc(read_memory(REGISTER_HL)));} break; // INC (HL)
         case 0x3C: {register_a = inc(register_a); break;} // INC A

         case 0x05: {register_b = dec(register_b);} break; // DEC B
         case 0x0D: {register_c = dec(register_c);} break; // DEC C
         case 0x15: {register_d = dec(register_d);} break; // DEC D
         case 0x1D: {register_e = dec(register_e);} break; // DEC E
         case 0x25: {register_h = dec(register_h);} break; // DEC H
         case 0x2D: {register_l = dec(register_l);} break; // DEC L
         case 0x35: write_memory(REGISTER_HL, dec(read_memory(REGISTER_HL))); break; // DEC (HL)
         case 0x3D: {register_a = dec(register_a);} break; // DEC A


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
            signed char offset = read_memory(register_pc++);

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
            signed char offset = read_memory(register_pc++);

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
         case 0xC3: jp(true); break; // JP nn
         case 0xE9: {register_pc = REGISTER_HL;} break; // JP HL

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

   unsigned int cycles = 0;
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

static void
handle_interrupts()
{
   if(ime && (read_memory(REGISTER_IE) & read_memory(REGISTER_IF)))
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
      write_memory(REGISTER_IF, read_memory(REGISTER_IF & ~(1 << bit_index)));

      // TODO(law): Wait for two cycles using NOPs.

      write_memory(--register_sp, register_pc >> 8);
      write_memory(--register_sp, register_pc & 0xFF);

      unsigned short isr_addresses[] = {0x40, 0x48, 0x50, 0x58, 0x60};
      register_pc = isr_addresses[bit_index];
   }
}

#define RESOLUTION_BASE_WIDTH 160
#define RESOLUTION_BASE_HEIGHT 144

#define TILE_PIXEL_DIM 8
#define TILES_PER_SCREEN_WIDTH  (RESOLUTION_BASE_WIDTH  / TILE_PIXEL_DIM)
#define TILES_PER_SCREEN_HEIGHT (RESOLUTION_BASE_HEIGHT / TILE_PIXEL_DIM)

#define VRAM_TILE_BLOCK_0 0x8000
#define VRAM_TILE_BLOCK_1 0x8800
#define VRAM_TILE_BLOCK_2 0x9000

#define PALETTE_DATA_BG   0xFF47
#define PALETTE_DATA_OBJ0 0xFF48
#define PALETTE_DATA_OBJ1 0xFF49

typedef struct
{
   unsigned int width;
   unsigned int height;
   unsigned int *memory;
} Bitmap;

typedef enum
{
   MONOCHROME_COLOR_OPTION_DMG,
   MONOCHROME_COLOR_OPTION_MGB,
   MONOCHROME_COLOR_OPTION_LIGHT,

   MONOCHROME_COLOR_OPTION_COUNT,
} Monochrome_Color_Scheme;

unsigned int monochrome_color_schemes[][5] =
{
   {0xFFE0F8D0, 0xFF88C070, 0xFF346856, 0xFF081820, 0xFFACC480}, // DMG
   {0xFFE0DBCD, 0xFFA89F94, 0xFF706B66, 0xFF2B2B26, 0xFFBDB890}, // MGB
   {0xFF65F2BA, 0xFF39C28C, 0xFF30B37F, 0xFF0E7F54, 0xFFBDB890}, // LIGHT
};

static unsigned int
get_display_off_color(Monochrome_Color_Scheme color_scheme)
{
   unsigned int result = monochrome_color_schemes[color_scheme][4];
   return(result);
}

static void
get_palette(unsigned int *palette, unsigned short address, Monochrome_Color_Scheme color_scheme)
{
   assert(ARRAY_LENGTH(monochrome_color_schemes) == MONOCHROME_COLOR_OPTION_COUNT);
   unsigned int *colors = monochrome_color_schemes[color_scheme];

   unsigned char palette_data = read_memory(address);

   palette[0] = colors[(palette_data >> 0) & 0x3];
   palette[1] = colors[(palette_data >> 2) & 0x3];
   palette[2] = colors[(palette_data >> 4) & 0x3];
   palette[3] = colors[(palette_data >> 6) & 0x3];
}

static void
clear(Bitmap *bitmap, unsigned int color)
{
   for(unsigned int y = 0; y < bitmap->height; ++y)
   {
      for(unsigned int x = 0; x < bitmap->width; ++x)
      {
         bitmap->memory[(bitmap->width * y) + x] = color;
      }
   }
}

static void
clear_scanline(Bitmap *bitmap, unsigned int scanline, unsigned int color)
{
   for(unsigned int x = 0; x < bitmap->width; ++x)
   {
      bitmap->memory[(bitmap->width * scanline) + x] = color;
   }
}

static void
render_vram_scanline(Bitmap *bitmap, Monochrome_Color_Scheme scheme, unsigned int scanline)
{
   unsigned int palette[4];
   unsigned short palette_address = PALETTE_DATA_BG;
   get_palette(palette, palette_address, scheme);

   clear_scanline(bitmap, scanline, palette[0]);

   unsigned int tile_offset = 0;
   unsigned char *tiles = map.vram.memory + (tile_offset * 16);

   unsigned int tile_y = scanline / TILE_PIXEL_DIM;
   unsigned int pixel_y = scanline % TILE_PIXEL_DIM;

   for(unsigned int tile_x = 0; tile_x < TILES_PER_SCREEN_WIDTH; ++tile_x)
   {
      unsigned int tile_index = (TILES_PER_SCREEN_WIDTH * tile_y) + tile_x;
      unsigned char *tile = tiles + (2 * tile_index * TILE_PIXEL_DIM);

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

         bool is_object = (palette_address == PALETTE_DATA_OBJ1 || palette_address == PALETTE_DATA_OBJ0);
         if(!(is_object && color_index == 0))
         {
            bitmap->memory[(bitmap->width * bitmap_y) + bitmap_x] = palette[color_index];
         }
      }
   }
}

#define SOUND_OUTPUT_HZ 48000
#define SOUND_OUTPUT_CHANNEL_COUNT 2
#define SOUND_OUTPUT_BYTES_PER_SAMPLE (SOUND_OUTPUT_CHANNEL_COUNT * sizeof(signed short))

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

typedef struct
{
   unsigned int sample_index;
   unsigned int size;

   signed short *samples;
} Sound_Samples;

static void
clear_sound_samples(Sound_Samples *sound)
{
   sound->sample_index = 0;
   zero_memory(sound->samples, sound->size);
}

static void
generate_debug_samples(Sound_Samples *destination, unsigned int sample_count)
{
   assert(destination->size >= (destination->sample_index + sample_count) * SOUND_OUTPUT_BYTES_PER_SAMPLE);

   static float wave_t = 0;

   float volume = 1000.0f;
   float wave_period = SOUND_OUTPUT_HZ / 256;

   unsigned int offset = destination->sample_index * SOUND_OUTPUT_CHANNEL_COUNT;
   signed short *samples = destination->samples;

   for(unsigned int index = 0; index < (sample_count * SOUND_OUTPUT_CHANNEL_COUNT); index += SOUND_OUTPUT_CHANNEL_COUNT)
   {
      signed short sample = (signed short)(sin(wave_t) * volume);

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
generate_sound_sample(Sound_Samples *destination)
{
   static int frame_sequncer_clock = 0;

   // NOTE(law): Master volume.
   unsigned int register_nr50 = read_memory(0xFF24);

   unsigned char enable_vin_left = (register_nr50 >> 7) & 0x1;
   unsigned char enable_vin_right = (register_nr50 >> 3) & 0x1;

   unsigned char volume_left = ((register_nr50 >> 4) & 0x7) + 1; // Range of 1-8
   unsigned char volume_right = ((register_nr50 >> 0) & 0x7) + 1; // Range of 1-8

   // NOTE(law): Per-channel sound panning.
   unsigned int register_nr51 = read_memory(0xFF25);

   unsigned char channel4_left_on = (register_nr51 >> 7) & 0x1;
   unsigned char channel3_left_on = (register_nr51 >> 6) & 0x1;
   unsigned char channel2_left_on = (register_nr51 >> 5) & 0x1;
   unsigned char channel1_left_on = (register_nr51 >> 4) & 0x1;

   unsigned char channel4_right_on = (register_nr51 >> 3) & 0x1;
   unsigned char channel3_right_on = (register_nr51 >> 2) & 0x1;
   unsigned char channel2_right_on = (register_nr51 >> 1) & 0x1;
   unsigned char channel1_right_on = (register_nr51 >> 0) & 0x1;

   // NOTE(law): Channel 1 registers.
   unsigned char register_nr10 = read_memory(0xFF10); // NOTE(law): Sweep
   unsigned char register_nr11 = read_memory(0xFF11); // NOTE(law): Length
   unsigned char register_nr12 = read_memory(0xFF12); // NOTE(law): Volume
   unsigned char register_nr13 = read_memory(0xFF13); // NOTE(law): Frequency
   unsigned char register_nr14 = read_memory(0xFF14); // NOTE(law): Control

   // NOTE(law): Channel 2 registers.
   unsigned char register_nr21 = read_memory(0xFF16); // NOTE(law): Length
   unsigned char register_nr22 = read_memory(0xFF17); // NOTE(law): Volume
   unsigned char register_nr23 = read_memory(0xFF18); // NOTE(law): Frequency
   unsigned char register_nr24 = read_memory(0xFF19); // NOTE(law): Control

   // NOTE(law): Channel 3 registers.
   unsigned char register_nr30 = read_memory(0xFF30); // NOTE(law): On/off
   unsigned char register_nr31 = read_memory(0xFF31); // NOTE(law): Length
   unsigned char register_nr32 = read_memory(0xFF32); // NOTE(law): Volume
   unsigned char register_nr33 = read_memory(0xFF33); // NOTE(law): Frequency
   unsigned char register_nr34 = read_memory(0xFF34); // NOTE(law): Control

   // NOTE(law): Channel 4 registers.
   unsigned char register_nr41 = read_memory(0xFF20); // NOTE(law): Length
   unsigned char register_nr42 = read_memory(0xFF21); // NOTE(law): Volume
   unsigned char register_nr43 = read_memory(0xFF22); // NOTE(law): Frequency
   unsigned char register_nr44 = read_memory(0xFF23); // NOTE(law): Control

   // NOTE(law); Sound on/off.
   unsigned char register_nr52 = read_memory(0xFF26);
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

   static char channel1_volume = 0;
   static char channel1_envelope_period = 0;
   static char channel1_envelope_direction = 0;
   static float channel1_frequency = 0;
   static float channel1_duty = 0;

   // NOTE(law): Channel 1 (tone and sweep)
   if(CHANNEL1_ON)
   {
      // NOTE(law): Length
      unsigned char wave_pattern_duty = (register_nr11 >> 6) & 0x3;
      unsigned char sound_length_data = (register_nr11 >> 0) & 0x1F; // Range: 0-63

      // NOTE(law): Volume
      unsigned char initial_volume = (register_nr12 >> 4) & 0xF; // Range 0-15
      unsigned char envelope_direction = (register_nr12 >> 3) & 0x1; // 0 <-, 1 ->
      unsigned char envelope_period = (register_nr12 >> 0) & 0x7; // Range 0-7

      // NOTE(law): Frequency
      unsigned char frequency_low = register_nr13;

      // NOTE(law): Control
      unsigned char counter_selection = (register_nr14 >> 6) & 0x1;
      unsigned char frequency_high = (register_nr14 >> 0) & 0x7;

      unsigned short frequency_data = ((unsigned short)frequency_high << 8) | frequency_low;

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
         static char counter = 0;
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
   signed short sample_left = 0;
   signed short sample_right = 0;

   if(CHANNEL1_ON)
   {
      static float wave_position = 0;

      signed short sample = (wave_position < channel1_duty)
      ? -(signed short)channel1_volume
      : (signed short)channel1_volume;

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
      signed short sample = 0;
      if(channel1_left_on) {sample_left += sample * volume_left;}
      if(channel1_right_on) {sample_right += sample * volume_right;}
   }

   if(CHANNEL3_ON)
   {
      signed short sample = 0;
      if(channel1_left_on) {sample_left += sample * volume_left;}
      if(channel1_right_on) {sample_right += sample * volume_right;}
   }

   if(CHANNEL4_ON)
   {
      signed short sample = 0;
      if(channel1_left_on) {sample_left += sample * volume_left;}
      if(channel1_right_on) {sample_right += sample * volume_right;}
   }

   unsigned int offset = destination->sample_index * SOUND_OUTPUT_CHANNEL_COUNT;

   *(destination->samples + offset + 0) = sample_left;
   *(destination->samples + offset + 1) = sample_right;

   destination->sample_index++;
}

typedef struct
{
   unsigned int cpu;
   unsigned int sound;
   unsigned int horizontal_sync;
} Clocks;

#define SOUND_PERIOD (CPU_HZ / SOUND_OUTPUT_HZ)
#define HORIZONTAL_SYNC_PERIOD (CPU_HZ / HORIZONTAL_SYNC_HZ)

static void
cpu_tick(Clocks *clocks, Bitmap *bitmap, Monochrome_Color_Scheme scheme, Sound_Samples *sound)
{
   if(!map.boot_complete && read_memory(0xFF50))
   {
      map.boot_complete = true;
   }

   handle_interrupts();
   unsigned int cycles = fetch_and_execute();

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

      unsigned char scanline = read_memory(0xFF44);
      if(scanline < 144)
      {
         render_vram_scanline(bitmap, scheme, scanline);
      }

      if(++scanline > 153)
      {
         scanline = 0;
      }
      write_memory(0xFF44, scanline);
   }
}
