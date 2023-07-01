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

#include "mem.h"
#include "comdef.h"
#include "macho64DynMod.h"

void *macho64Hook(const Macho64File *mf, const char *func, const void *hand)
{
    if(macho64Check(mf) || func == NULL || hand == NULL)
        return NULL;

    uint64_t *rel_addr = NULL;
    uint64_t indx = macho64GetDSymIndxByName(mf, func);
    if (IS_MACHO64_ERROR(indx)) {
        LOG_ERROR("Cannot get index of the symbol %s", func);
        return NULL;
    }

    /***
     * reserved1 in __Data.__la_symbol_ptr section contains a start position
     * of Lazy Symbols indexes in indirect tables.
     */
    Macho64Sect *lazyImportSect = macho64GetSectByName(mf, "__la_symbol_ptr");
    /***
     * __got section contains imports for non-lazy inderect symbols
    */
    Macho64Sect *nonLazyImportSect = macho64GetSectByName(mf, "__got");
    if ((lazyImportSect == NULL) && (nonLazyImportSect == NULL)) {
        LOG_WARNING("Trere are no __la_symbol_ptr and __got sections.");
        return NULL;
    }

    Macho64Sect *importSect = lazyImportSect;
    uint64_t i = macho64GetImportSymbolPosInSectByIndx(mf, importSect, indx);
    if (IS_MACHO64_ERROR(i)) {
        importSect = nonLazyImportSect;
        i = macho64GetImportSymbolPosInSectByIndx(mf, importSect, indx);
        if (IS_MACHO64_ERROR(i)) {
            LOG_ERROR("Cannot find symbol position in import sections");
            return NULL;
        }
    }

    // Get seed for work with randomize adress space
    Macho64Sym *_mach_hook = macho64GetSymByName(mf, "_macho64Hook");
    if (_mach_hook == NULL)  {
        LOG_ERROR("Cannot get the symbol _mach_hook");
        return NULL;
    }

    uint64_t _mach_hook_addr = macho64GetSSymAddr(_mach_hook);
    if (IS_MACHO64_ERROR(_mach_hook_addr)) {
        LOG_ERROR("Cannot get an addr of symbol _mach_hook");
        return NULL;
    }

    uint64_t seed = (uint64_t)macho64Hook - _mach_hook_addr;

    // Get real virtual address of import table start
    uint64_t *real_vaddr = (uint64_t*)(importSect->addr + seed);
    uint64_t mprotect_real_vaddr = alignToPageSize((size_t)real_vaddr);
    size_t mprotect_size = importSect->addr + seed - mprotect_real_vaddr + importSect->size;

    rel_addr = (uint64_t*)(real_vaddr[i]);
    if (Mprotect((void*)mprotect_real_vaddr, mprotect_size, PROT_WRITE | PROT_READ)) {
        LOG_ERROR("Cannot change memory protection");
        return NULL;
    }
    real_vaddr[i] = (uint64_t)hand;

    return rel_addr;
}

