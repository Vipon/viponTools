/***
 * MIT License
 *
 * Copyright (c) 2021-2023 Konychev Valerii
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

// vipon headers
#include "os.h"
#include "mem.h"
#include "file.h"
#include "comdef.h"
#include "macho64Parse.h"

// C standard headers
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <inttypes.h>

static MACHO64_ERROR macho64ParseArch(Macho64File *mf)
{
    if (mf == NULL || mf->header == NULL)
        return MACHO64_INV_ARG;

    switch (mf->header->cputype) {
    case CPU_TYPE_X86:
        mf->arch = X86;
        break;
    case CPU_TYPE_X86_64:
        mf->arch = X86_64;
        break;
    case CPU_TYPE_ARM:
        mf->arch = ARM;
        break;
    case CPU_TYPE_ARM64:
        mf->arch = ARM64;
        break;
    default:
        mf->arch = UNKNOWN_ARCH;
        break;
    };

    return MACHO64_OK;
}

/***
 * Description:
 *  Function init Macho64Header of a Macho64File  @mf.
 * Input:
 *  @mf - macho64 file descriptor.
 * Output:
 *  Success:
 *      MACHO64_OK.
 *  Fail:
 *      MACHO64_INV_ARG, MACHO64_NO_MEM, MACHO64_NO_HEADER.
 */
static MACHO64_ERROR macho64ParseHeader(Macho64File *mf)
{
    if (mf == NULL || IS_INV_FD(mf->fd))
        return MACHO64_INV_ARG;

    FileD fd = mf->fd;
    size_t off = mf->hOff;
    size_t h_size = sizeof(Macho64Header);

    mf->header = (Macho64Header*)readFromFile(fd, &off, h_size);
    if (mf->header == NULL)
        return MACHO64_NO_MEM;

    macho64ParseArch(mf);

    switch (mf->header->magic) {
    case MH_MAGIC:
    case MH_CIGAM:
        // 32-bit macho file
        return MACHO64_NO_HEADER;
    case MH_MAGIC_64:
    case MH_CIGAM_64:
        mf->type = mf->header->filetype;
        return MACHO64_OK;
    default:
        return MACHO64_NO_HEADER;
    }
}

/***
 * Description:
 *  Function init mach-o LoadCommands of a Macho64File @mf.
 * Input:
 *  @mf - point to target Macho64File structure with initialized header
 *  @hOff - offset to the Macho64Header in the file
 * Output:
 *  Success:
 *      MACHO64_OK
 *  Fail:
 *      MACHO64_INV_ARG, MACHO64_NO_LOAD_COMMAND, MACHO64_NO_MEM
 */
static MACHO64_ERROR macho64ParseLCommands(Macho64File *mf)
{
    if (mf == NULL || IS_INV_FD(mf->fd) || mf->header == NULL)
        return MACHO64_INV_ARG;

    FileD fd = mf->fd;
    size_t off = mf->hOff + sizeof(Macho64Header);
    size_t size = (size_t)mf->header->sizeofcmds;
    if (mf->header->ncmds == 0)
        return MACHO64_NO_LOAD_COMMAND;

    uint8_t *lcom = (uint8_t*)readFromFile(fd, &off, size);
    if (lcom == NULL)
        return MACHO64_NO_MEM;

    mf->lcom = lcom;
    return MACHO64_OK;
}

/***
 * Description:
 *  Function initializes symtabCmd field in mf.
 * Input:
 *  @mf - point to target Macho64File structure with initialized header and lcom
 * Output:
 *  Success:
 *      MACHO64_OK
 *  Fail:
 *      MACHO64_INV_ARG, MACHO64_NO_SYMTAB_CMD
 */
static MACHO64_ERROR macho64ParseSymtabCom(Macho64File *mf)
{
    if (mf == NULL || mf->lcom == NULL)
        return MACHO64_INV_ARG;

    FOREACH_LOAD_COMMAND(mf,
        if (lcom->cmd == LC_SYMTAB)
            mf->symtabCmd = (SymtabCommand*)lcom;
    )

    if (mf->symtabCmd == NULL)
        return MACHO64_NO_SYMTAB_CMD;

    return MACHO64_OK;
}

/***
 * Description:
 *  Function reads mach-o table of symbols from file.
 * Input:
 *  @mf - point to target Macho64File structure with initialized symtabCmd.
 * Output:
 *  Success:
 *      MACHO64_OK
 *  Fail:
 *      MACHO64_INV_ARG, MACHO64_NO_SYMTAB, MACHO64_NO_MEM
 */
static MACHO64_ERROR macho64ParseSymTab(Macho64File *mf)
{
    if (mf == NULL || IS_INV_FD(mf->fd) || mf->symtabCmd == NULL)
        return MACHO64_INV_ARG;

    uint32_t num = mf->symtabCmd->nsyms;
    size_t off = mf->hOff + mf->symtabCmd->symoff;
    uint64_t size = sizeof(Macho64Sym) * num;
    if (num == 0)
        return MACHO64_NO_SYMTAB;

    Macho64Sym *symtab = (Macho64Sym*) readFromFile(mf->fd, &off, size);
    if (symtab == NULL)
        return MACHO64_NO_MEM;

    mf->symtab = symtab;
    return MACHO64_OK;
}

/***
 * Description:
 *  Function reads mach-o table of symtab from file and sort it
 * Input:
 *  @mf - point to target Macho64File structure with initialized symtabCmd.
 * Output:
 *  Success:
 *      MACHO64_OK
 *  Fail:
 *      MACHO64_INV_ARG, MACHO64_NO_SYMTAB, MACHO64_NO_MEM
 */
static MACHO64_ERROR macho64ParseSortSymTab(Macho64File *mf)
{
    if (mf == NULL || IS_INV_FD(mf->fd) || mf->symtabCmd == NULL)
        return MACHO64_INV_ARG;

    FileD fd = mf->fd;
    size_t num = mf->symtabCmd->nsyms;
    size_t off = mf->hOff + mf->symtabCmd->symoff;
    size_t size = sizeof(Macho64Sym) * num;
    if (num == 0)
        return MACHO64_NO_SYMTAB;

    Macho64Sym *symtab = (Macho64Sym*) readFromFile(fd, &off, size);
    if (symtab == NULL)
        return MACHO64_NO_MEM;

    qsort(symtab, num, sizeof(Macho64Sym), macho64CmpSym);

    mf->sortSymtab = symtab;
    return MACHO64_OK;
}

/***
 * Description:
 *  Function returns a mach-o table of symbols name from a file
 * Input:
 *  @mf - point to target Macho64File structure with initialized symtab
 * Output:
 *  Success:
 *      MACHO64_OK
 *  Fail:
 *      MACHO64_INV_ARG, MACHO64_NO_SYM_NAME_TAB, MACHO64_NO_MEM
 */
static MACHO64_ERROR macho64ParseSymNameTab(Macho64File *mf)
{
    if (mf == NULL || mf->symtab == NULL)
        return MACHO64_INV_ARG;

    size_t off = mf->hOff + mf->symtabCmd->stroff;
    size_t size = mf->symtabCmd->strsize;
    if (size == 0)
        return MACHO64_NO_SYM_NAME_TAB;

    char *symNameTab = (char*)readFromFile(mf->fd, &off, size);
    if (symNameTab == NULL)
        return MACHO64_NO_MEM;

    mf->symNameTab = symNameTab;
    return MACHO64_OK;
}

/***
 * Description:
 *  Function initializes dysymtabCmd fields in @mf.
 * Input:
 *  @mf - point to target Macho64File structure with initialized header and lcom
 * Output:
 *  Success:
 *      MACHO64_OK
 *  Fail:
 *      MACHO64_INV_ARG, MACHO64_NO_DYSYMTAB_CMD
 */
static MACHO64_ERROR macho64ParseDysymtabCom(Macho64File *mf)
{
    if (mf == NULL || mf->lcom == NULL)
        return MACHO64_INV_ARG;

    uint32_t i = 0;
    uint32_t ncmds = mf->header->ncmds;
    uint32_t cmdsize = 0;
    LoadCommand *lcom = (LoadCommand*)(void*)mf->lcom;
    for (i = 0; i < ncmds; ++i) {
        lcom = (LoadCommand*) ((size_t)lcom + cmdsize);
        cmdsize = lcom->cmdsize;
        if (lcom->cmd == LC_DYSYMTAB)
            mf->dynsymCmd = (DysymtabCommand*)lcom;
    }

    if (mf->dynsymCmd == NULL)
        return MACHO64_NO_DYSYMTAB_CMD;

    return MACHO64_OK;
}

/***
 * Description:
 *  Function reads mach-o inderect symbol table
 * Input:
 *  @mf - point to target Macho64File structure with initialized dynsymCmd
 * Output:
 *  Success:
 *      MACHO64_OK
 *  Fail:
 *      MACHO64_INV_ARG, MACHO64_NO_MEM, MACHO64_NO_DYSYMTAB_CMD
 */
static MACHO64_ERROR macho64ParseInderectSymtab(Macho64File *mf)
{
    if (mf == NULL || IS_INV_FD(mf->fd))
        return MACHO64_INV_ARG;

    if (mf->dynsymCmd != NULL) {
        FileD fd = mf->fd;
        size_t off = mf->hOff + mf->dynsymCmd->indirectsymoff;
        size_t size = mf->dynsymCmd->nindirectsyms * sizeof(uint32_t);
        uint32_t *indSyms = (uint32_t*)readFromFile(fd, &off, size);
        if (indSyms == NULL)
            return MACHO64_NO_MEM;

        mf->indirectSymtab = indSyms;
        return MACHO64_OK;
    } else
        return MACHO64_NO_DYSYMTAB_CMD;
}

/***
 * Description:
 *  Function initializes segments field in @mf.
 * Input:
 *  @mf - point to target Macho64File structure with initialized header and lcom
 * Output:
 *  Success:
 *      MACHO64_OK
 *  Fail:
 *      MACHO64_INV_ARG
 */
static
MACHO64_ERROR macho64ParseSegCom(Macho64File *mf)
{
    if (mf == NULL || mf->header == NULL || mf->lcom == NULL)
        return MACHO64_INV_ARG;

    uint32_t i = 0;
    size_t cmdsize = 0;
    LoadCommand *lc = (LoadCommand*)(void*)mf->lcom;
    for (i = 0; i < mf->header->ncmds; ++i) {
        lc = (LoadCommand*) ((size_t)lc + cmdsize);
        cmdsize = lc->cmdsize;
        if (lc->cmd == LC_SEGMENT_64) {
            char *name = ((Macho64Seg*)(void*)lc)->segname;
            if (strcmp("__PAGEZERO", name) == 0)
                mf->segments[PAGEZERO_NSEG] = (Macho64Seg*)(void*)lc;
            if (strcmp("__TEXT", name) == 0)
                mf->segments[TEXT_NSEG] = (Macho64Seg*)(void*)lc;
            if (strcmp("__DATA_CONST", name) == 0)
                mf->segments[DATA_CONST_NSEG] = (Macho64Seg*)(void*)lc;
            if (strcmp("__DATA", name) == 0)
                mf->segments[DATA_NSEG] = (Macho64Seg*)(void*)lc;
            if (strcmp("__OBJC", name) == 0)
                mf->segments[OBJC_NSEG] = (Macho64Seg*)(void*)lc;
            if (strcmp("__IMPORT", name) == 0)
                mf->segments[IMPORT_NSEG] = (Macho64Seg*)(void*)lc;
            if (strcmp("__LINKEDIT", name) == 0)
                mf->segments[LINKEDIT_NSEG] = (Macho64Seg*)(void*)lc;
            if (strcmp("", name) == 0)
                mf->segments[UNNAMED_NSEG] = (Macho64Seg*)(void*)lc;
        }
    }

    return MACHO64_OK;
}

static
MACHO64_ERROR macho64ParseFuncStarts(Macho64File *mf)
{
    if (mf == NULL || mf->header == NULL || mf->lcom == NULL)
        return MACHO64_INV_ARG;

    FOREACH_LOAD_COMMAND(mf,
        if (lcom->cmd == LC_FUNCTION_STARTS) {
            mf->funcStarts = (MachoLinkEditData*)lcom;
        }
    );

    return MACHO64_OK;
}

static
MACHO64_ERROR macho64ParseDylibCom(Macho64File *mf)
{
    if (mf == NULL || mf->header == NULL || mf->lcom == NULL)
        return MACHO64_INV_ARG;

    FOREACH_LOAD_COMMAND(mf,
        if (lcom->cmd == LC_LOAD_DYLIB) {
            ++(mf->numDyLibCom);
        }
    );

    if (mf->numDyLibCom == 0) {
        mf->dylibCom = NULL;
        return MACHO64_OK;
    }

    mf->dylibCom = (MachoDylibCommand**)
        Malloc(sizeof(MachoDylibCommand*) * mf->numDyLibCom);

    uint32_t j = 0;
    FOREACH_LOAD_COMMAND(mf,
        if (lcom->cmd == LC_LOAD_DYLIB) {
            mf->dylibCom[j++] = (MachoDylibCommand*)lcom;
        }
    );

    return MACHO64_OK;
}

static
MACHO64_ERROR macho64ParseCodeSign(Macho64File *mf)
{
    if (mf == NULL || mf->header == NULL || mf->lcom == NULL)
        return MACHO64_INV_ARG;

    FOREACH_LOAD_COMMAND(mf,
        if (lcom->cmd == LC_CODE_SIGNATURE) {
            mf->sign = (MachoLinkEditData*)lcom;
            return MACHO64_OK;
        }
    );

    return MACHO64_OK;
}

MACHO64_ERROR _macho64Parse(Macho64File *mf, uint64_t off)
{
    if (mf == NULL)
        return MACHO64_INV_ARG;

    mf->hOff = off;

    MACHO64_ERROR err = macho64ParseHeader(mf);
    if (err) {
        LOG_ERROR("Cannot parse mach-o header");
        return err;
    }

    err = macho64ParseLCommands(mf);
    if (err) {
        LOG_ERROR("Cannot parse mach-o load commands");
        return err;
    }

    err = macho64ParseSymtabCom(mf);
    if (err) {
        if (IS_MACHO64_FILE_OBJ(mf)) {
            LOG_ERROR("Cannot parse mach-o symtab command");
            return err;
        } else
            LOG_WARNING("Cannot parse mach-o symtab command");
    } else {
        err = macho64ParseSymTab(mf);
        if (err) {
            LOG_ERROR("Cannot parse mach-o symbols table");
            return err;
        } else
            macho64ParseSortSymTab(mf);

        err = macho64ParseSymNameTab(mf);
        if (err) {
            LOG_ERROR("Cannot parse mach-o symbol name tabls.");
            return err;
        }
    }

    err = macho64ParseDysymtabCom(mf);
    if (err) {
        LOG_WARNING("Cannot parse mach-o dysyntab commd");
    } else {
        err = macho64ParseInderectSymtab(mf);
        if (err) {
            LOG_ERROR("Cannot parse mach-o inderect symbol table");
            return err;
        }
    }

    err = macho64ParseSegCom(mf);
    if (err) {
        if (IS_MACHO64_FILE_EXEC(mf)) {
            LOG_ERROR("Cannot parse mach-o segment commands.");
            return err;
        } else
            LOG_WARNING("Cannot parse mach-o segment commands.");
    }

    macho64ParseFuncStarts(mf);
    macho64ParseDylibCom(mf);
    macho64ParseCodeSign(mf);

    return MACHO64_OK;
}

Macho64File *macho64Parse(const char *fn)
{
    if (fn == NULL)
        return NULL;

    FileD fd = open(fn, O_RDONLY);
    if (IS_INV_FD(fd)) {
        PERROR("open()");
        return NULL;
    }

    Macho64File *mf = (Macho64File*) Calloc(1, sizeof(Macho64File));
    if (mf == NULL) {
        LOG_ERROR("Cannot allocate %zu bytes", sizeof(Macho64File));
        goto eexit_0;
    }

    mf->fd = fd;
    size_t nameLen = strlen(fn) + 1;
    if ((mf->fn = (char*) Calloc(nameLen, sizeof(char))) == NULL) {
        LOG_ERROR("Cannot allocate %zu bytes", nameLen);
        goto eexit_1;
    }

    strncpy(mf->fn, fn, nameLen);

    if (_macho64Parse(mf, 0))
        goto eexit_1;

    return mf;

eexit_1:
    macho64Free(mf);
    return NULL;
eexit_0:
    close(fd);
    return NULL;
}

void macho64Clean(Macho64File *mf)
{
    if (mf == NULL)
        return;

    if (mf->fn) {
        Free(mf->fn);
        mf->fn = NULL;
    }

    if (mf->fd != INV_FD) {
        close(mf->fd);
        mf->fd = INV_FD;
    }

    if (mf->header) {
        Free(mf->header);
        mf->header = NULL;
    }

    if (mf->lcom) {
        Free(mf->lcom);
        mf->lcom = NULL;
    }

    if (mf->symtab) {
        Free(mf->symtab);
        mf->symtab = NULL;
    }

    if (mf->sortSymtab) {
        Free(mf->sortSymtab);
        mf->sortSymtab = NULL;
    }

    if (mf->indirectSymtab) {
        Free(mf->indirectSymtab);
        mf->indirectSymtab = NULL;
    }

    if (mf->symNameTab) {
        Free(mf->symNameTab);
        mf->symNameTab = NULL;
    }

    if (mf->dylibCom) {
        Free(mf->dylibCom);
        mf->dylibCom = NULL;
    }
}

void macho64Free(Macho64File *mf)
{
    macho64Clean(mf);
    Free(mf);
}

MACHO64_ERROR macho64Check(const Macho64File *mf)
{
    if (mf == NULL)
        return MACHO64_INV_ARG;

    if (mf->header == NULL) {
        STDERROR_PRINT_DEBUG("Uninitialized field mf->header.");
        return MACHO64_NO_HEADER;
    }

    if (mf->lcom == NULL) {
        STDERROR_PRINT_DEBUG("Uninitialized field mf->lcom.");
        return MACHO64_NO_LOAD_COMMAND;
    }

    if (mf->symtabCmd == NULL) {
        if (IS_MACHO64_FILE_OBJ(mf)) {
            LOG_ERROR("Uninitialized field mf->symtabCmd.");
            return MACHO64_NO_SYMTAB_CMD;
        } else
            LOG_WARNING("Uninitialized field mf->symtabCmd.");
    } else {
        if (mf->symtab == NULL) {
            if (IS_MACHO64_FILE_EXEC(mf)) {
                LOG_ERROR("Uninitialized field mf->symtab.");
                return MACHO64_NO_SYMTAB;
            } else
                LOG_WARNING("Uninitialized field mf->symtab.");
        } else {
            if (mf->sortSymtab == NULL) {
                if (IS_MACHO64_FILE_EXEC(mf)) {
                    LOG_ERROR("Uninitialized field mf->symtab.");
                    return MACHO64_NO_SORT_SYMTAB;
                } else
                    LOG_WARNING("Uninitialized field mf->symtab.");
            }

            if (mf->symNameTab == NULL) {
                if (IS_MACHO64_FILE_EXEC(mf)) {
                    LOG_ERROR("Uninitialized field mf->symNameTab.");
                    return MACHO64_NO_SYM_NAME_TAB;
                } else
                    LOG_WARNING("Uninitialized field mf->symNameTab.");
            }
        }
    }

    if (mf->dynsymCmd == NULL) {
        LOG_WARNING("Uninitialized field mf->dynsymCmd.\n");
        //return MACHO64_NO_DYSYMTAB_CMD;
    } else
        if (mf->indirectSymtab == NULL) {
            LOG_ERROR("Uninitialized field mf->indirectSymtab.");
            return MACHO64_NO_INDIRECT_SYM_TAB;
        }

    if (mf->segments[TEXT_NSEG] == NULL && mf->segments[UNNAMED_NSEG] == NULL) {
        LOG_ERROR("Uninitialized field mf->segments.");
        return MACHO64_NO_SEGMENTS;
    }

    return MACHO64_OK;
}

MACHO64_ERROR macho64PrintIndirectSymTab(const Macho64File *mf)
{
    if (macho64Check(mf))
        return MACHO64_INV_ARG;

    uint32_t i = 0;
    uint32_t num = mf->dynsymCmd->nindirectsyms;
    printf("Indirect symbol table:\n");
    printf("\tIndx\tData\tSymbol\n");
    for (i = 0; i < num; ++i) {
        uint32_t indx = mf->indirectSymtab[i];
        if (indx == INDIRECT_SYMBOL_ABS)
            printf("\t%u.\t%s: 0x%x\n", i, "INDIRECT_SYMBOL_ABS", indx);
        else {
            uint32_t n_strx = mf->symtab[indx].n_un.n_strx;
            char *symname = &(mf->symNameTab[n_strx]);
            printf("\t%u.\t%u\t%s\n", i, indx, symname);
        }

    }

    return MACHO64_OK;
}

Macho64Sym *macho64GetSymByIndx(const Macho64File *mf, uint64_t indx)
{
    if (macho64Check(mf) || indx == (uint64_t)-1)
        return NULL;

    return &mf->symtab[indx];
}

Macho64Sym *macho64GetSymByName(const Macho64File *mf, const char *name)
{
    if (macho64Check(mf) || name == NULL)
        return NULL;

    uint32_t i = 0;
    uint32_t num = mf->symtabCmd->nsyms;
    for (i = 0; i < num; ++i) {
        uint32_t indx = mf->symtab[i].n_un.n_strx;
        char *curName = &(mf->symNameTab[indx]);
        if (!IS_MACHO64_SYM_DEBUG(mf->symtab[i]) &&
                    (strcmp(name, curName) == 0)) {
            return &(mf->symtab[i]);
        }
    }

    LOG_ERROR("There is no symbol %s.", name);
    return NULL;
}

Macho64Sym *macho64GetSSymByAddr(const Macho64File *mf, uint64_t addr)
{
    if (mf == NULL || mf->sortSymtab == NULL || addr == (uint64_t)-1)
        return NULL;

    size_t i = 0;
    size_t sym_num = macho64GetAmountSSym(mf);
    for (i = 0; i < sym_num; ++i)
        if (!IS_MACHO64_SYM_DEBUG(mf->sortSymtab[i])) {
            if (mf->sortSymtab[i].n_value == addr) {
                return &mf->sortSymtab[i];
            }
        }

    return NULL;
}

char *macho64GetSymName(const Macho64File *mf, const Macho64Sym *ms)
{
    if (mf == NULL || mf->symNameTab == NULL || ms == NULL)
        return NULL;

    uint32_t indx = ms->n_un.n_strx;
    return &(mf->symNameTab[indx]);
}

uint64_t macho64GetSymIndxByName(const Macho64File *mf, const char *name)
{
    if(macho64Check(mf) || name == NULL)
        return MACHO64_INV_ARG;

    uint32_t i = 0;
    uint32_t num = mf->symtabCmd->nsyms;
    for (i = 0; i < num; ++i) {
        uint32_t indx = mf->symtab[i].n_un.n_strx;
        char *curName = &(mf->symNameTab[indx]);
        if (!IS_MACHO64_SYM_DEBUG(mf->symtab[i]) &&
                    strcmp(name, curName) == 0)
            return i;
    }

    return MACHO64_NO_SYMBOL;
}

int macho64CmpSym(const void * a, const void * b)
{
    int64_t distance = (int64_t)(((const Macho64Sym*)a)->n_value - ((const Macho64Sym*)b)->n_value);
    if (distance > 0)
        return 1;
    else if (distance < 0)
        return -1;
    else
        return 0;
}

Macho64Sym *macho64GetSSymTab(const Macho64File *mf)
{
    if (macho64Check(mf))
        return NULL;

    return mf->symtab;
}

Macho64Sym *macho64GetSSymSortTab(const Macho64File *mf)
{
    if (macho64Check(mf))
        return NULL;

    return mf->sortSymtab;
}

uint64_t macho64GetSSymTabFileoff(const Macho64File *mf)
{
    if (macho64Check(mf))
        return MACHO64_INV_ARG;

    return mf->symtabCmd->symoff;
}

uint64_t macho64GetAmountSSym(const Macho64File *mf)
{
    if (mf == NULL || mf->symtabCmd == NULL)
        return MACHO64_INV_ARG;

    return mf->symtabCmd->nsyms;
}

uint64_t macho64GetSSymAddr(const Macho64Sym *ms)
{
    if (ms == NULL)
        return MACHO64_INV_ARG;

    return ms->n_value;
}

uint64_t macho64GetAddrSymByName(const Macho64File *elf64, const char *name)
{
    if (macho64Check(elf64) || name == NULL) {
        LOG_ERROR("Invalid arguments");
        return MACHO64_INV_ARG;
    }

    // TODO: work only for static symbols, for dynamic need signal
    return macho64GetSymByName(elf64, name)->n_value;
}

void macho64SetSSymAddr(Macho64Sym *ms, uint64_t vaddr)
{
    ms->n_value = vaddr;
}

static uint64_t macho64GetVendOfSect(const Macho64File *mf, unsigned num)
{
    if (macho64Check(mf) || num == (unsigned)(-1))
        return MACHO64_INV_ARG;

    uint64_t i = 0;
    uint64_t base = 0;
    Macho64Seg *seg = NULL;
    if (IS_MACHO64_FILE_EXEC(mf))
        seg = mf->segments[TEXT_NSEG];
    else if (IS_MACHO64_FILE_OBJ(mf)) {
        // Into the object file there is only one segment
        seg = mf->segments[UNNAMED_NSEG];
    } else {
        LOG_ERROR("Unknown file type.");
        return MACHO64_NO_FILE_TYPE;
    }

    Macho64Sect *sect = (Macho64Sect*)((size_t)seg + sizeof(Macho64Seg));
    for (i = 0; i <= seg->nsects; ++i) {
        if (i == num)
            return sect->addr + sect->size;

        sect = (Macho64Sect*)((size_t)sect + sizeof(Macho64Sect));
    }

    base = i;
    // This point is reached only if file executable or object
    if (IS_MACHO64_FILE_EXEC(mf))
        seg = mf->segments[DATA_NSEG];
    else {
        // Into the object file there are no more segments
        return MACHO64_NO_FILE_TYPE;
    }

    sect = (Macho64Sect*)((size_t)seg + sizeof(Macho64Seg));
    for (i = base; i <= base + seg->nsects; ++i) {
        if (i == num)
            return sect->addr + sect->size;

        sect = (Macho64Sect*)((size_t)sect + sizeof(Macho64Sect));
    }

    return MACHO64_NO_SECTION;
}

uint64_t macho64GetFuncSize(const Macho64File *mf, const Macho64Sym *ms)
{
    if (macho64Check(mf) || ms == NULL)
        return MACHO64_INV_ARG;

    uint64_t i = 0;
    uint64_t size = 0;
    uint64_t start = ms->n_value;
    /***
     * Find our symbol of function. Fist of all, we find a symbol, which addr
     * is equal to our function. Secondly, it should be a symbol of function,
     * because compiler could create another stab symbol for the function
     * (n_type = N_STAB)
     */
    for (i = 0; mf->sortSymtab[i].n_value < start ||
                !IS_MACHO64_SYM_FUNC(mf->sortSymtab[i]); ++i);

    if (i < mf->symtabCmd->nsyms-1) {
        /***
         *  Find next symbol of function. There could be labels into functions,
         *  so we should skip their
         */
        do {
            ++i;
            if (mf->sortSymtab[i].n_sect != ms->n_sect  &&
                !IS_MACHO64_SYM_DEBUG(mf->sortSymtab[i])   &&
                !IS_MACHO64_UNDEF_SYM(mf->sortSymtab[i]))
                break;

        } while (!IS_MACHO64_SYM_FUNC(mf->sortSymtab[i]));

        if (mf->sortSymtab[i].n_sect == ms->n_sect)
            size = mf->sortSymtab[i].n_value - start;
        else {
            uint64_t vend = macho64GetVendOfSect(mf, ms->n_sect);
            if (IS_MACHO64_ERROR(vend)) {
                LOG_ERROR("Cannot get vend of section %hhu.", ms->n_sect);
                return MACHO64_NO_SECTION;
            }

            size = vend - start;
        }
    } else {
        uint64_t vend = macho64GetVendOfSect(mf, ms->n_sect);
        if (IS_MACHO64_ERROR(vend)) {
            LOG_ERROR("Cannot get vend of section %hhu.", ms->n_sect);
            return MACHO64_NO_SECTION;
        }

        size = vend - start;
    }

    return size;
}

uint64_t macho64GetGDataSize(const Macho64File *mf, const Macho64Sym *ms)
{
    if (macho64Check(mf) || ms == NULL)
        return MACHO64_INV_ARG;

    uint64_t i = 0;
    uint64_t size = 0;
    uint64_t start = ms->n_value;
    /***
     * Find our symbol of global data. Fist of all, we find a symbol, which
     * addr is equal to our data. Secondly, it should be a symbol of
     * data, because compiler could create another stab symbol for the
     * data (n_type = N_STAB)
     */
    for (i = 0; mf->sortSymtab[i].n_value < start ||
                !IS_MACHO64_SYM_GDATA(mf->sortSymtab[i]); ++i);

    if (i < mf->symtabCmd->nsyms-1) {
        // Skip stabs and labels
        do {
            ++i;
            if (mf->sortSymtab[i].n_sect != ms->n_sect)
                break;

        } while (!IS_MACHO64_SYM_GDATA(mf->sortSymtab[i]));

        if (mf->sortSymtab[i].n_sect == ms->n_sect)
            size = mf->sortSymtab[i].n_value - start;
        else {
            uint64_t vend = macho64GetVendOfSect(mf, ms->n_sect);
            if (IS_MACHO64_ERROR(vend)) {
                LOG_ERROR("Cannot get vend of section %hhu.", ms->n_sect);
                return MACHO64_NO_SECTION;
            }

            size = vend - start;
        }
    } else {
        uint64_t vend = macho64GetVendOfSect(mf, ms->n_sect);
        if (IS_MACHO64_ERROR(vend)) {
            LOG_ERROR("Cannot get vend of section %hhu.", ms->n_sect);
            return MACHO64_NO_SECTION;
        }

        size = vend - start;
    }

    return size;
}

uint64_t macho64GetSSymSize(const Macho64File *mf, const Macho64Sym *ms)
{
    if (macho64Check(mf) || ms == NULL)
        return MACHO64_INV_ARG;

    if (IS_MACHO64_SYM_LABEL((*ms)))
        return 0;

    if (IS_MACHO64_SYM_FUNC((*ms)))
        return macho64GetFuncSize(mf, ms);

    if (IS_MACHO64_SYM_GDATA((*ms)))
        return macho64GetGDataSize(mf, ms);

    return MACHO64_NO_SYMBOL;
}

uint64_t macho64GetSSymFileoff(const Macho64File *mf, const Macho64Sym *sym)
{
    if (macho64Check(mf) || sym == NULL)
        return MACHO64_INV_ARG;

    uint64_t addr = macho64GetSSymAddr(sym);
    if (IS_MACHO64_ERROR(addr)) {
        LOG_ERROR("Cannot get addr of a symbol.");
        return MACHO64_NO_SYMBOL;
    }

    Macho64Seg *text = NULL;
    // !TODO: change logic from segment to section
    if (IS_MACHO64_FILE_EXEC(mf))
        text = mf->segments[TEXT_NSEG];
    else if (IS_MACHO64_FILE_OBJ(mf)) {
        // Into the object file there is only one segment
        text = mf->segments[UNNAMED_NSEG];
    } else {
        LOG_ERROR("Unknown file type.");
        return MACHO64_NO_FILE_TYPE;
    }

    uint64_t diff = text->vmaddr - text->fileoff;
    return (addr - diff);
}

uint64_t macho64GetAmountSeg(const Macho64File *mf)
{
    if (macho64Check(mf))
        return MACHO64_INV_ARG;

    uint64_t i = 0;
    uint64_t num = 0;
    for (i = 0; i < MAX_NSEG; ++i)
        if (mf->segments[i])
            ++num;

    return num;
}

Macho64Seg *macho64GetDataSeg(const Macho64File *mf)
{
    if (macho64Check(mf))
        return NULL;

    if (IS_MACHO64_FILE_EXEC(mf))
        return mf->segments[DATA_NSEG];
    else if (IS_MACHO64_FILE_OBJ(mf)) {
        LOG_ERROR("There is no __DATA segment into the object file.");
        return NULL;
    } else {
        LOG_ERROR("Unknown file type.");
        return NULL;
    }
}

Macho64Seg *macho64GetLastSeg(const Macho64File *mf)
{
    if (macho64Check(mf))
        return NULL;

    int64_t i = 0;
    for (i = MAX_NSEG - 1; i >= 0; --i)
        if (mf->segments[i])
            return mf->segments[i];

    return NULL;
}

uint64_t macho64GetSegSize(const Macho64Seg *seg)
{
    if (seg == NULL)
        return MACHO64_INV_ARG;

    return seg->vmsize;
}

uint64_t macho64GetSegAddr(const Macho64Seg *seg)
{
    if (seg == NULL)
        return MACHO64_INV_ARG;

    return seg->vmaddr;
}

uint64_t macho64GetSegFileoff(const Macho64Seg *seg)
{
    if (seg == NULL)
        return MACHO64_INV_ARG;

    return seg->fileoff;
}

Macho64Sect *macho64GetSectByName(const Macho64File *mf, const char *name)
{
    if(macho64Check(mf) || name == NULL)
        return NULL;

    uint64_t i = 0;
    for (i = 0; i < MAX_NSEG; ++i) {
        Macho64Seg *seg = mf->segments[i];
        if (seg) {
            Macho64Sect *sect = (Macho64Sect*)((size_t)seg + sizeof(Macho64Seg));
            uint64_t j = 0;
            for (j = 0; j <= seg->nsects; ++j) {
                if (strcmp(sect->sectname, name) == 0)
                    return sect;

                sect = (Macho64Sect*)((size_t)sect + sizeof(Macho64Sect));
            }
        }
    }

    return NULL;
}

Macho64Sect *macho64GetSectByAddr(const Macho64File *mf, uint64_t addr)
{
    if (macho64Check(mf))
        return NULL;

    uint64_t i = 0;
    for (i = 0; i < MAX_NSEG; ++i) {
        Macho64Seg *seg = mf->segments[i];
        if (seg) {
            uint64_t seg_addr = macho64GetSegAddr(seg);
            uint64_t seg_size = macho64GetSegSize(seg);
            if (addr >= seg_addr && addr < (seg_addr + seg_size)) {
                Macho64Sect *sect = (Macho64Sect*)((size_t)seg + sizeof(Macho64Seg));
                uint64_t j = 0;
                for (j = 0; j <= seg->nsects; ++j) {
                    uint64_t sect_addr = macho64GetSectAddr(sect);
                    uint64_t sect_size = macho64GetSectSize(sect);

                    if (addr >= sect_addr && addr < (sect_addr + sect_size))
                        return sect;

                    sect = (Macho64Sect*)((size_t)sect + sizeof(Macho64Sect));
                }
            }
        }
    }

    return NULL;
}

Macho64Sect *macho64GetSectByIndx(const Macho64File *mf, uint64_t indx)
{
    if (macho64Check(mf) || indx == (uint64_t)-1)
        return NULL;

    uint64_t sectNum = 1;
    Macho64Sect *sect = NULL;
    FOREACH_LOAD_COMMAND(mf,
        if (lcom->cmd == LC_SEGMENT_64) {
            const Macho64Seg *seg = (const Macho64Seg*)lcom;
            if ((sectNum + seg->nsects) <= indx) {
                sectNum += seg->nsects;
            } else {
                sect = (Macho64Sect*)((size_t)seg + sizeof(Macho64Seg));
                return sect + (indx - sectNum);
            }
        }
    );

    return NULL;
}

Macho64Sect *macho64GetAllSect(const Macho64Seg *seg)
{
    if (seg == NULL)
        return NULL;

    Macho64Sect *sect = (Macho64Sect*)((size_t)seg + sizeof(Macho64Seg));

    return sect;
}

Macho64Sect *macho64GetLastSect(const Macho64Seg *seg)
{
    if (seg == NULL)
        return NULL;

    Macho64Sect *sect = (Macho64Sect*)((size_t)seg + sizeof(Macho64Seg));
    uint64_t num = seg->nsects - 1;
    sect = (Macho64Sect*)((size_t)sect + (num * sizeof(Macho64Sect)));

    return sect;
}

Macho64Sect *macho64GetLastLoadableSect(const Macho64File *mf)
{
    if (macho64Check(mf))
        return NULL;

    uint64_t i = MAX_NSEG - 1;
    for (; i > 0; --i) {
        Macho64Seg *seg = mf->segments[i];
        int64_t j = seg->nsects;
        Macho64Sect *sect = macho64GetAllSect(seg);
        for (; j >= 0; --j)
            if (sect[i].addr)
                return &sect[i];
    }

    LOG_WARNING("There is no loadable section");
    return NULL;
}

void *macho64ReadSect(const Macho64File *mf, const Macho64Sect *sect)
{
    if (mf == NULL || IS_INV_FD(mf->fd) || sect == NULL) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    FileD fd = mf->fd;
    uint64_t size = macho64GetSectSize(sect);
    uint64_t off = macho64GetSectFileoff(sect);

    void *section = readFromFile(fd, (size_t*)&off, size);
    if (section == NULL) {
        LOG_ERROR("Cannot read from file");
        return NULL;
    }

    return section;
}

uint64_t macho64GetAmountSect(const Macho64File *mf)
{
    if (macho64Check(mf)) {
        LOG_ERROR("Invalid argument");
        return MACHO64_INV_ARG;
    }

    uint64_t i = 0;
    uint64_t num = 0;
    for (i = 0; i < MAX_NSEG; ++i)
        if (mf->segments[i] != NULL)
            num += mf->segments[i]->nsects;

    return num;
}

const char* macho64GetSectName(const Macho64File *mf, const Macho64Sect *sect)
{
    if (macho64Check(mf) || sect == NULL)
        return NULL;

    return sect->sectname;
}

uint64_t macho64GetSectSize(const Macho64Sect *sect)
{
    if (sect == NULL)
        return MACHO64_INV_ARG;

    return sect->size;
}

uint64_t macho64GetSectAddr(const Macho64Sect *sect)
{
    if (sect == NULL)
        return MACHO64_INV_ARG;

    return sect->addr;
}

uint64_t macho64GetSectFileoff(const Macho64Sect *sect)
{
    if (sect == NULL)
        return MACHO64_INV_ARG;

    return sect->offset;
}

uint64_t macho64GetSectRelocFileoff(const Macho64Sect *sect)
{
    return sect->reloff;
}

uint64_t macho64GetSectRelocNum(const Macho64Sect *sect)
{
    return sect->nreloc;
}

uint64_t macho64GetRelocAddr(const Macho64Sect *sect, const MachoRelocInfo *rel)
{
    uint64_t vaddr = macho64GetSectAddr(sect);
    return (vaddr + (uint64_t)rel->r_address);
}

void macho64ChangeRelocAddr(MachoRelocInfo *rel, int32_t diff)
{
    rel->r_address += diff;
}

uint64_t macho64GetRelocForAddr(const Macho64File *mf, const Macho64Sect *sect, uint64_t addr)
{
    // !TODO: add parse relocations
    if (mf == NULL || sect == NULL || addr == (size_t)-1)
        return MACHO64_INV_ARG;

    FileD fd = mf->fd;
    uint64_t i = 0;           // loop counter
    uint64_t vaddr_sect = 0;  // vaddr of @sect
    uint64_t reloc_off = sect->reloff;
    uint64_t num_reloc = sect->nreloc;
    uint64_t reloc_size = sizeof(MachoRelocInfo) * num_reloc;
    MachoRelocInfo *rel_info = (MachoRelocInfo*)readFromFile(fd, (size_t*)&reloc_off, reloc_size);
    if (rel_info == NULL) {
        LOG_ERROR("Cannot read from file");
        return MACHO64_NO_MEM;
    }

    if ((uint32_t)rel_info->r_address & R_SCATTERED) {
        LOG_ERROR("It's scattered relocation info");
        goto eexit_0;
    }

    vaddr_sect = macho64GetSectAddr(sect);
    if (IS_MACHO64_ERROR(vaddr_sect)) {
        LOG_ERROR("macho64GetSectionVaddr()");
        goto eexit_0;
    }

    for (i = 0; i < num_reloc; ++i) {
        LOG("rel_info->r_address %d\n", rel_info[i].r_address);
        if ((vaddr_sect + (uint64_t)rel_info[i].r_address) == addr) {
            if (rel_info->r_symbolnum == R_ABS) {
                // Absolute symbol, which need no relocation
                Free(rel_info);
                return 0;
            }

            if (rel_info[i].r_extern == 0) {
                // In this case r_symbolnum - section number
                return 0;
            } else if (rel_info[i].r_extern == 1) {
                // In this case r_symbolnum - index into the symbol table
                Macho64Sym *sym = macho64GetSymByIndx(mf, rel_info[i].r_symbolnum);

                Free(rel_info);
                return sym->n_value;
            }
        }
    }

    LOG_WARNING("There is no relocation info for this addr\n");

    Free(rel_info);
    return 0;

eexit_0:
    Free(rel_info);
    return MACHO64_NO_RELOCATION;
}

uint64_t macho64GetDSymIndxByName(const Macho64File *mf, const char *name)
{
    if(macho64Check(mf) || name == NULL)
        return MACHO64_INV_ARG;

    uint64_t i = mf->dynsymCmd->iundefsym;
    uint64_t num = mf->symtabCmd->nsyms;
    for (; i < num; ++i) {
        uint64_t indx = mf->symtab[i].n_un.n_strx;
        char *curName = &(mf->symNameTab[indx]);
        if (strcmp(name, curName) == 0) {
            return i;
        }
    }

    return MACHO64_NO_SYMBOL;
}

uint64_t macho64GetImportSymbolPosInSectByIndx( const Macho64File *mf
                                              , const Macho64Sect *importSect
                                              , uint64_t indx
                                              )
{
    if (importSect == NULL || mf == NULL) {
        return MACHO64_INV_ARG;
    }

    uint64_t i = 0;
    uint64_t start = importSect->reserved1;
    uint64_t end = start + (importSect->size >> importSect->align);
    for (i = start; i < end; ++i)
        if (mf->indirectSymtab[i] == indx)
            break;

    if (i == end)
        return MACHO64_NO_INDIRECT_SYM_TAB;

    // Get target index in import table
    return (i -= start);
}

void *macho64GetRelocDataAddr(const Macho64File *mf, const char *func)
{
    if(macho64Check(mf) || func == NULL)
        return NULL;

    uint64_t *rel_addr = NULL;
    uint64_t indx = macho64GetDSymIndxByName(mf, func);
    if (IS_MACHO64_ERROR(indx)) {
        LOG_ERROR("Cannot get index of the symbol %s", func);
        return NULL;
    }

    /***
     * reserved1 in __Data.__la_symbol_ptr section contains a start position
     * of Lazy Symbols indexes in indirect tables
     */
    Macho64Sect *__la = macho64GetSectByName(mf, "__la_symbol_ptr");
    if (__la == NULL) {
        LOG_ERROR("Cannot get the section __la_symbol_ptr");
        return NULL;
    }

    uint64_t i = __la->reserved1;
    uint64_t num = mf->dynsymCmd->nindirectsyms;
    for (i = __la->reserved1; i < num; ++i)
        if (mf->indirectSymtab[i] == indx)
            break;

    // Get target index in import table
    i -= __la->reserved1;

    // Get seed for work with randomize adress space
    Macho64Sym *_macho64GetRelocDataAddr = macho64GetSymByName(mf, "_macho64GetRelocDataAddr");
    if (_macho64GetRelocDataAddr == NULL)  {
        LOG_ERROR("Cannot get the symbol _macho64GetRelocDataAddr");
        return NULL;
    }

    uint64_t _macho64GetRelocDataAddr_addr = macho64GetSSymAddr(_macho64GetRelocDataAddr);
    if (IS_MACHO64_ERROR(_macho64GetRelocDataAddr_addr)) {
        LOG_ERROR("Cannot get an addr of symbol _macho64GetRelocDataAddr");
        return NULL;
    }

    uint64_t seed = (uint64_t)macho64GetRelocDataAddr - _macho64GetRelocDataAddr_addr;

    // Get real virtual address of import table start
    uint64_t *real_vaddr = (uint64_t*)(__la->addr + seed);

    rel_addr = (uint64_t*)(real_vaddr[i]);

    return rel_addr;
}

const char *macho64GetDylibName(const MachoDylibCommand *dl)
{
    return (char*)(((size_t)dl) + dl->dylib.name.offset);
}

