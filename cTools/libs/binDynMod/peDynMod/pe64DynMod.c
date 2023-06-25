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

#include "Windows.h"
#include "pe64DynMod.h"

void *pe64Hook(const PE64File *pe, const char *func, const void *hand)
{
    if (pe == NULL || func == NULL || hand == NULL)
        return NULL;

    size_t base = 0;
    if (pe->optHeader->DllCharacteristics & IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE)
        base = (size_t)GetModuleHandle(NULL);
    else
        base = pe->optHeader->ImageBase;

    uint64_t i = 0;
    uint64_t impNum = pe->importNum - 1; // Last entry is NULL padded
    for (i = 0; i < impNum; ++i) {
        PEImport *imp = pe->import + i;
        ThunkData64 *IAT = (ThunkData64*)(base + imp->FirstThunk);
        ThunkData64 *INT = (ThunkData64*)(base + imp->OriginalFirstThunk);

        if (imp ->TimeDateStamp == (DWORD)-1) {
            // Static bound
            // !TODO: Add check in bound libs
            break;
        }

        uint64_t j = 0;
        for (;;++j) {
            ImportByName *importByName = (ImportByName*)(base + INT[j].u1.AddressOfData);
            if (INT[j].u1.AddressOfData == 0)
                break;

            if (IS_BIT_SET(INT[j].u1.AddressOfData, 63)) {
                // Import by number
                // !TODO: Add check symbol name in export table of libs
                break;
            }

            char *name = importByName->Name;
            if (strcmp(name, func) == 0) {
                void *oldVal = (void*)IAT[j].u1.Function;

                DWORD oldProtect = 0;
                void *addr = (void*)alignToPageSize((size_t)(IAT + j));
                size_t pageSize = (size_t)getPageSize();

                VirtualProtect(addr, pageSize, PAGE_READWRITE, &oldProtect);
                IAT[j].u1.Function = (size_t)hand;
                VirtualProtect(addr, pageSize, oldProtect, &oldProtect);
                return oldVal;
            }
        }
    }

    // !TODO: Add hook for delay imports

    return NULL;
}

