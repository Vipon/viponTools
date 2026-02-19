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

#include "comdef.h"
#include "elf32DynMod.h"

void *elf32Hook(const Elf32File *elf32, const char *func, const void *hand)
{
    if (  elf32Check(elf32)
       || func == NULL
       || hand == NULL
       ) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    /***
     * symbolIndex -   index of target symbol in .dynsym section.
     */
    uint32_t symbolIndex = elf32GetDSymIndxByName(elf32, func);
    if (symbolIndex == (uint32_t)-1) {
        LOG_ERROR("Cannot get an index of a dynamic symbol %s.", func);
        return NULL;
    }

    /***
     * shSize      -   contains the size, in bytes, of the section.
     * relpltAmount-   amount of Elf32Rel structures in .rela.ptl section.
     */
    Elf32Shdr *relplt = elf32GetSectByName(elf32, RELAPLT);
    if (relplt == NULL) {
        LOG_ERROR("Cannot get the section " RELAPLT);
        return NULL;
    }

    uint32_t relpltAmount = relplt->sh_size / sizeof(Elf32Rel);
    Elf32Sym *hook_sym = elf32GetSymByName(elf32, "elf32Hook");
    uint32_t func_original_addr = elf32GetSSymAddr(hook_sym);
    uint32_t func_addr_diff = (uint32_t)(size_t)(void*)&elf32Hook - func_original_addr;

    /***
     * r_info        -   This member gives both the symbol table index,
     *                   with respect to which the relocation must be made,
     *                   and the type of relocation to apply.
     * r_offset      -   This member gives the location at which to apply
     *                   the relocation action.
     * For __x86_32 allowed only PIC code, consequently relocation information
     * for all dynamic symbols are in .rela.plt section.
     * In this case r_offset is an address, where is address for relocation of
     * original function.
     */
    void *relAddr = NULL;
    uint32_t i = 0;
    for (i = 0; i < relpltAmount; ++i)
        if (ELF32_R_SYM(elf32->relaplt[i].r_info) == symbolIndex){
            uint32_t offset = elf32->relaplt[i].r_offset;
            uint32_t* addr = (uint32_t*)(size_t)(func_addr_diff + offset);
            relAddr = (void*)(size_t) *addr;
            *addr = (uint32_t)(size_t) hand;

            return relAddr;
        }

    return NULL;
}

