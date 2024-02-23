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

#include "os.h"
#include "binParse.h"
#include "binDynMod.h"
#ifdef __WIN__
# include "pe64DynMod.h"
#endif /* __WIN__ */
#ifdef __LINUX__
# include "elf32DynMod.h"
# include "elf64DynMod.h"
#endif /* __LINUX__ */
#ifdef __MAC_OS_X__
# include "macho64DynMod.h"
# include "fatMacho64DynMod.h"
#endif /* __MAC_OS_X__ */

BinDynMod binDynMod = {0};

#ifndef __WIN__
static uint64_t
get_seed(void)
{
    // Get seed for work with randomize adress space
    BinSymPtr sym = binParser.getSymByName(binParser.bin, SYM_PREFIX"get_seed");
    if (sym == NULL)  {
        //STDERROR_PRINT("Cannot get the symbol "SYM_PREFIX"get_seed\n");
        return (uint64_t)-1;
    }

    uint64_t sym_addr = binParser.getSSymAddr(sym);
    if (sym_addr == (uint64_t)-1) {
        //STDERROR_PRINT("Cannot get an addr of symbol "SYM_PREFIX"get_seed\n");
        return (uint64_t)-1;
    }

    uint64_t seed = (uint64_t)get_seed - sym_addr;
    return seed;
}
#else /* __WIN__ */
static uint64_t
get_seed(void)
{
    uint64_t seed = 0;
    PE64File *pe = (PE64File*)binParser.bin;
    if (pe->optHeader->DllCharacteristics & IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE)
        seed = (size_t)GetModuleHandle(NULL);
    else
        seed = pe->optHeader->ImageBase;

    return seed;
}
#endif /* __WIN__ */

#define INIT_BIN_DYN_MOD(type)                  \
    binDynMod.hook = (BinHook)&type ## Hook;    \
    binDynMod.get_seed = (BinGetSeed)&get_seed;

int initBinDynMod(BIN_FILE_TYPE type)
{
    switch(type) {
#ifdef __MAC_OS_X__
    case MACHO64:
        INIT_BIN_DYN_MOD(macho64);
        break;
    case FATMACHO64:
        INIT_BIN_DYN_MOD(fatMacho64);
        break;
#endif /* __MAC_OS_X__ */
#ifdef __LINUX__
    case ELF64:
        INIT_BIN_DYN_MOD(elf64);
        break;
    case ELF32:
        INIT_BIN_DYN_MOD(elf32);
        break;
#endif /* __LINUX__ */
#ifdef __WIN__
    case PE64:
        INIT_BIN_DYN_MOD(pe64);
        break;
#endif /* __WIN__ */
    default:
        return -1;
    }

    return 0;
}



