/***
 * MIT License
 *
 * Copyright (c) 2023-2024 Konychev Valerii
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

///////////////////////////////////////////////////////////////////////////////
// \brief                                                                    //
// File contains definitions only for 64 bits mach-o file format.            //
///////////////////////////////////////////////////////////////////////////////
#ifndef __MACHO64_H
#define __MACHO64_H

#ifndef __LP64__
# define __LP64__ 1
#endif /* __LP64__ */

#include "macho.h"

/***
 *  struct fat_arch_64
 *  {
 *      cpu_type_t cputype;         // cpu specifier (int)
 *      cpu_subtype_t cpusubtype;   // machine specifier (int)
 *      uint64_t offset;            // file offset to this object file
 *      uint64_t size;              // size of this object file
 *      uint32_t align;
 *      uint32_t reserved;
 *  };
 */
typedef struct fat_arch_64 FatArch64;

/***
 *  struct mach_header_64
 *  {
 *      uint32_t magic;             // mach magic number identifier
 *      cpu_type_t cputype;         // cpu specifier
 *      cpu_subtype_t cpusubtype;   // machine specifier
 *      uint32_t filetype;          // type of file
 *      uint32_t ncmds;             // number of load commands
 *      uint32_t sizeofcmds;        // the size of all the load commands
 *      uint32_t flags;
 *      uint32_t reserved;
 *  };
 */
typedef struct mach_header_64 Macho64Header;

/***
 *  Describes an entry in the symbol table for 64-bit architectures.
 *  struct nlist_64
 *  {
 *      union {
 *          uint32_t n_strx;    // index into the string table
 *      } n_un;
 *      uint8_t n_type;         // byte value consisting of data
 *      uint8_t n_sect;         // number of the section
 *      int16_t n_desc;         // add info about this symbol
 *      uint64_t n_value;       // value of the symbol, depends on type
 *  };
 */
typedef struct nlist_64 Macho64Sym;

/***
 *  struct segment_command_64
 *  {
 *      uint32_t cmd;       // LC_SEGMENT_64
 *      uint32_t cmdsize;   // includes sizeof section_64 structs
 *      char segname[16];   // segment name
 *      uint64_t vmaddr;    // start vaddr of segment
 *      uint64_t vmsize;    // size of segment in virtual memory
 *      uint64_t fileoff;   // position of segment in file
 *      uint64_t filesize;  // size of segment in file
 *      vm_prot_t maxprot;  // maximal permissions r/w/x
 *      vm_prot_t initprot; // initial permissions r/w/x
 *      uint32_t nsects;    // The number of section data structures
 *      uint32_t flags;
 *  };
 */
typedef struct segment_command_64 Macho64Seg;

/***
 *  struct section_64
 *  {
 *      char sectname[16];  // name of section
 *      char segname[16];   // name of segment contains this section
 *      uint64_t addr;      // the virtual memory address
 *      uint64_t size;      // the size in bytes of the virtual memory
 *      uint32_t offset;    // file offset for this section
 *      uint32_t align;     // the power of two (for 8 bytes: align = 3)
 *      uint32_t reloff;    // file offset of relocation entries
 *      uint32_t nreloc;    // number of relocation entries
 *      uint32_t flags;
 *      // Import section contains entries for imported symbols in correspond orders
 *      // as in the indirect symbol table, start at index stored in reserved1.
 *      uint32_t reserved1; // index into indirect symbol table
 *      uint32_t reserved2; // size of stubs
 *  };
 */
typedef struct section_64 Macho64Sect;

// Header introduced in 2018, cannot be found for 32-bits
#include <mach-o/fixup-chains.h>
typedef struct dyld_chained_fixups_header macho_fixups_headers_t;
typedef struct dyld_chained_starts_in_image macho_starts_in_image_t;
typedef struct dyld_chained_starts_in_segment macho_starts_in_segment_t;
typedef struct dyld_chained_import macho_chained_import_t;
typedef struct dyld_chained_ptr_64_rebase macho_ptr_64_rebase_t;
typedef struct dyld_chained_ptr_64_bind macho_ptr_64_bind_t;

/***
 * Returns sections type value
 */
#define MACHO64_SECT_TYPE(sect) MACHO_SECT_TYPE(sect)

/***
 * If section contains stubs.
 */
#define IS_MACHO64_SECT_STUBS(sect) IS_MACHO_SECT_STUBS(sect)

/***
 * If section interacts with inderect symbols.
 */
#define IS_MACHO64_SECT_INDIRECT_SYMBOLS(sect) IS_MACHO_SECT_INDIRECT_SYMBOLS(sect)

#define MACHO64_SYM_TYPE(sym)       MACHO_SYM_TYPE(sym)
#define IS_MACHO64_SYM_UNDEF(sym)   IS_MACHO_SYM_UNDEF(sym)
#define IS_MACHO64_SYM_ABS(sym)     IS_MACHO_SYM_ABS(sym)
#define IS_MACHO64_SYM_SECT(sym)    IS_MACHO_SYM_SECT(sym)
#define IS_MACHO64_SYM_PBUD(sym)    IS_MACHO_SYM_PBUD(sym)
#define IS_MACHO64_SYM_INDR(sym)    IS_MACHO_SYM_INDR(sym)

#define IS_MACHO64_SYM_EXT(sym)     IS_MACHO_SYM_EXT(sym)

/***
 * If any of these bits set, a symbolic debugging entry
 */
#define IS_MACHO64_SYM_DEBUG(sym)   IS_MACHO_SYM_DEBUG(sym)

#define MACHO64_SYM_REF_TYPE(sym) \
        MACHO_SYM_REF_TYPE(sym)
#define IS_MACHO64_SYM_REF_TYPE_LAZY(sym) \
        IS_MACHO_SYM_REF_TYPE_LAZY(sym)
#define IS_MACHO64_SYM_REF_TYPE_NON_LAZY(sym) \
        IS_MACHO_SYM_REF_TYPE_NON_LAZY(sym)
#define IS_MACHO64_DSYM_LAZY(sym) \
        IS_MACHO_DSYM_LAZY(sym)
#define IS_MACHO64_DSYM_NON_LAZY(sym) \
        IS_MACHO_DSYM_NON_LAZY(sym)

#endif /* __MACHO64_H */

