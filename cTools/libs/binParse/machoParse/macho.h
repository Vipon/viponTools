/***
 * MIT License
 *
 * Copyright (c) 2023 Konychev Valerii
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
// File contains definitions common for 32 and 64 bits mach-o file format.   //
///////////////////////////////////////////////////////////////////////////////

#ifndef __MACHO_H
#define __MACHO_H

/* binary format headers */
#include <mach-o/fat.h>
#include <mach-o/nlist.h>
#include <mach-o/reloc.h>
#include <mach-o/loader.h>

/***
 *  struct fat_header
 *  {
 *      uint32_t magic;     // FAT_MAGIC or FAT_MAGIC_64
 *      uint32_t nfat_arch; // number of structs that follow
 *  };
 */
typedef struct fat_header FatHeader;

typedef struct fat_arch FatArch;

/***
 *  struct load_command
 *  {
 *      uint32_t cmd;       // type of load command
 *      uint32_t cmdsize;   // total size of command in bytes
 *  };
 */
typedef struct load_command LoadCommand;

/***
 *  struct symtab_command
 *  {
 *      uint_32 cmd;        // LC_SYMTAB
 *      uint_32 cmdsize;    // sizeof(struct symtab_command)
 *      uint_32 symoff;     // Offset from the start of the image to the
 *                          // location of the symbol table.
 *      uint_32 nsyms;      // Number of entries in the symbol table.
 *      uint_32 stroff;     // Offset from the start of the image to the
 *                          // location of the string table.
 *      uint_32 strsize;    // The size (in bytes) of the string table.
 *  };
 */
typedef struct symtab_command SymtabCommand;

/***
 *  struct dysymtab_command
 *  {
 *      uint32_t cmd;           // LC_DYSYMTAB
 *      uint32_t cmdsize;       // sizeof(struct dysymtab_command)
 *
 *  The local symbols are used only for debugging.
 *      uint32_t ilocalsym;     // index to local symbols in the symtab
 *      uint32_t nlocalsym;     // number of local symbols
 *
 *  The last two groups are used by the dynamic binding process.
 *      uint32_t iextdefsym;    // index to externally defined symbols
 *      uint32_t nextdefsym;    // number of externally defined symbols
 *      uint32_t iundefsym;     // Index of first undef symbol in the symtab
 *      uint32_t nundefsym;     // number of undefined symbols
 *
 *      uint32_t tocoff;
 *      uint32_t ntoc;
 *      uint32_t modtaboff;
 *      uint32_t nmodtab;
 *      uint32_t extrefsymoff;
 *      uint32_t nextrefsyms;
 *      uint32_t indirectsymoff;// Image Offset to the indirect symbol table
 *      uint32_t nindirectsyms; // Number of indirect symbol table entries
 *      uint32_t extreloff;
 *      uint32_t nextrel;
 *      uint32_t locreloff;
 *      uint32_t nlocrel;
 *  };
 */
typedef struct dysymtab_command DysymtabCommand;

/***
 *  struct relocation_info {
 *      int32_t     r_address;
 *      uint32_t    r_symbolnum:24,
 *                  r_pcrel:1,  // was relocated pc relative already
 *                  r_length:2, // 0=byte, 1=word, 2=long, 3=quad
 *                  r_extern:1, // does not include value of sym referenced
 *                  r_type:4;   // if not 0, machine specific relocation type
 *  };
 */
typedef struct relocation_info MachoRelocInfo;

/***
 * The linkedit_data_command contains the offsets and sizes of a blob
 * of data in the __LINKEDIT segment.
 *
 *  struct linkedit_data_command {
 *      uint32_t    cmd;      // LC_CODE_SIGNATURE, LC_SEGMENT_SPLIT_INFO,
 *                            // LC_FUNCTION_STARTS, LC_DATA_IN_CODE,
 *                            // LC_DYLIB_CODE_SIGN_DRS,
 *                            // LC_LINKER_OPTIMIZATION_HINT,
 *                            // LC_DYLD_EXPORTS_TRIE, or
 *                            // LC_DYLD_CHAINED_FIXUPS.
 *      uint32_t    cmdsize;  // sizeof(struct linkedit_data_command)
 *      uint32_t    dataoff;  // file offset of data in __LINKEDIT segment
 *      uint32_t    datasize; // file size of data in __LINKEDIT segment
 * };
 */
typedef struct linkedit_data_command MachoLinkEditData;

/***
 * A dynamically linked shared library (filetype == MH_DYLIB in the mach header)
 * contains a dylib_command (cmd == LC_ID_DYLIB) to identify the library.
 * An object that uses a dynamically linked shared library also contains a
 * dylib_command (cmd == LC_LOAD_DYLIB, LC_LOAD_WEAK_DYLIB, or
 * LC_REEXPORT_DYLIB) for each library it uses.
 *
 *  struct dylib_command {
 *      uint32_t        cmd;     // LC_ID_DYLIB, LC_LOAD_{,WEAK_}DYLIB,
 *                               // LC_REEXPORT_DYLIB
 *      uint32_t        cmdsize; // includes pathname string
 *      struct dylib    dylib;   // the library identification
 *  };
 *  struct dylib {
 *      union lc_str  name;             // library's path name
 *      uint32_t timestamp;             // library's build time stamp
 *      uint32_t current_version;       // library's current version number
 *      uint32_t compatibility_version; // library's compatibility vers number
 *  };
 */
typedef struct dylib_command MachoDylibCommand;
typedef struct dylib MachoDylib;

/***
 * The entry_point_command is a replacement for thread_command.
 * It is used for main executables to specify the location (file offset)
 * of main().  If -stack_size was used at link time, the stacksize
 * field will contain the stack size need for the main thread.
 *
 *  struct entry_point_command {
 *      uint32_t  cmd;       // LC_MAIN only used in MH_EXECUTE filetypes
 *      uint32_t  cmdsize;   // 24
 *      uint64_t  entryoff;  // file (__TEXT) offset of main()
 *      uint64_t  stacksize; // if not zero, initial stack size
 *  };
 */
typedef struct entry_point_command MachoEntryPointCom;

/*
 * A program that uses a dynamic linker contains a dylinker_command to identify
 * the name of the dynamic linker (LC_LOAD_DYLINKER).  And a dynamic linker
 * contains a dylinker_command to identify the dynamic linker (LC_ID_DYLINKER).
 * A file can have at most one of these.
 * This struct is also used for the LC_DYLD_ENVIRONMENT load command and
 * contains string for dyld to treat like environment variable.
 *
 *  struct dylinker_command {
 *      uint32_t        cmd;    // LC_ID_DYLINKER, LC_LOAD_DYLINKER or
 *                                 LC_DYLD_ENVIRONMENT
 *      uint32_t        cmdsize;//includes pathname string
 *      union lc_str    name;       // dynamic linker's path name
 * };
 */
typedef struct dylinker_command MachoDylinkerCom;

/***
 * The rpath_command contains a path which at runtime should be added to
 * the current run path used to find @rpath prefixed dylibs.
 *
 *  struct rpath_command {
 *      uint32_t     cmd;     // LC_RPATH
 *      uint32_t     cmdsize; // includes string
 *      union lc_str path;    // path to add to run path
 *  };
 */
typedef struct rpath_command MachoRpathComm;

/*
 * The uuid load command contains a single 128-bit unique random number that
 * identifies an object produced by the static link editor.
 *
 *  struct uuid_command {
 *      uint32_t    cmd;      // LC_UUID
 *      uint32_t    cmdsize;  // sizeof(struct uuid_command)
 *      uint8_t     uuid[16]; // the 128-bit uuid
 * };
 */
typedef struct uuid_command MachoUUIDComm;

/***
 * The build_version_command contains the min OS version on which this
 * binary was built to run for its platform.  The list of known platforms and
 * tool values following it.
 *
 *  struct build_version_command {
 *      uint32_t    cmd;        // LC_BUILD_VERSION
 *      uint32_t    cmdsize;    // sizeof(struct build_version_command) plus
 *                              // ntools * sizeof(struct build_tool_version)
 *      uint32_t    platform;   // platform
 *      uint32_t    minos;      // X.Y.Z is encoded in nibbles xxxx.yy.zz
 *      uint32_t    sdk;        // X.Y.Z is encoded in nibbles xxxx.yy.zz
 *      uint32_t    ntools;     // number of tool entries following this
 *  };
 *
 *  struct build_tool_version {
 *      uint32_t    tool;       // enum for the tool
 *      uint32_t    version;    // version number of the tool
 *  };
 */
typedef struct build_version_command MachoBuildVer;
typedef struct build_tool_version    MachoBuildToolVer;

/***
 * The source_version_command is an optional load command containing
 * the version of the sources used to build the binary.
 *
 *  struct source_version_command {
 *      uint32_t  cmd;      // LC_SOURCE_VERSION
 *      uint32_t  cmdsize;  // 16
 *      uint64_t  version;  // A.B.C.D.E packed as a24.b10.c10.d10.e10
 *  };
 */
typedef struct source_version_command MachoSourceVer;

/***
 * Returns sections type value
 */
#define MACHO_SECT_TYPE(sect) ((sect->flags & SECTION_TYPE))

/***
 * If section contains stubs.
 */
#define IS_MACHO_SECT_STUBS(sect) \
    ((MACHO64_SECT_TYPE(sect) == S_SYMBOL_STUBS))

/***
 * If section interacts with inderect symbols.
 */
#define IS_MACHO_SECT_INDIRECT_SYMBOLS(sect)                  \
    (  (MACHO_SECT_TYPE(sect) == S_NON_LAZY_SYMBOL_POINTERS)  \
    || (MACHO_SECT_TYPE(sect) == S_LAZY_SYMBOL_POINTERS)      \
    || (MACHO_SECT_TYPE(sect) == S_SYMBOL_STUBS)              \
    )

#define MACHO_SYM_TYPE(sym)       ((sym).n_type & N_TYPE)
#define IS_MACHO_SYM_UNDEF(sym)   (MACHO_SYM_TYPE(sym) == N_UNDF)
#define IS_MACHO_SYM_ABS(sym)     (MACHO_SYM_TYPE(sym) == N_ABS)
#define IS_MACHO_SYM_SECT(sym)    (MACHO_SYM_TYPE(sym) == N_SECT)
#define IS_MACHO_SYM_PBUD(sym)    (MACHO_SYM_TYPE(sym) == N_PBUD)
#define IS_MACHO_SYM_INDR(sym)    (MACHO_SYM_TYPE(sym) == N_INDR)

#define IS_MACHO_SYM_EXT(sym)     ((sym).n_type & N_EXT)

/***
 * If any of these bits set, a symbolic debugging entry
 */
#define IS_MACHO_SYM_DEBUG(sym)   ((sym).n_type & N_STAB)

#define MACHO_SYM_REF_TYPE(sym) \
    ((sym).n_desc & REFERENCE_TYPE)
#define IS_MACHO_SYM_REF_TYPE_LAZY(sym) \
    (MACHO_SYM_REF_TYPE(sym) == REFERENCE_FLAG_UNDEFINED_LAZY)
#define IS_MACHO_SYM_REF_TYPE_NON_LAZY(sym) \
    (MACHO_SYM_REF_TYPE(sym) == REFERENCE_FLAG_UNDEFINED_NON_LAZY)
#define IS_MACHO_DSYM_LAZY(sym) \
    (IS_MACHO_SYM_UNDEF(sym) && IS_MACHO_SYM_REF_TYPE_LAZY(sym))
#define IS_MACHO_DSYM_NON_LAZY(sym) \
    (IS_MACHO_SYM_UNDEF(sym) && IS_MACHO_SYM_REF_TYPE_NON_LAZY(sym))

#endif /* __MACHO_H */

