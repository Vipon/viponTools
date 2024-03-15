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

#include "mem.h"
#include "string.h"
#include "endian.h"
#include "fatMacho64Parse.h"

#include <inttypes.h>

Arch fatMacho64ParseArch = ARCH;

/***
 * @brief parse 32 bit fat arch descriptor
 *
 * @param[in,out] ff pointer to FatMacho64File
 *
 * @return MACHO64_OK or MACHO64_NO_MEM
*/
static MACHO64_ERROR macho64FatParseArch32(FatMacho64File *ff)
{
    FileD fd = ff->fd;
    size_t off = sizeof(FatHeader);
    size_t size = sizeof(FatArch) * (ff->fatHead.nfat_arch);
    FatArch *fatArch = (FatArch*)readFromFile(fd, &off, size);
    if (fatArch == NULL)
        return MACHO64_NO_MEM;

    ff->fatArch = fatArch;

#ifdef __ORDER_LITTLE_ENDIAN__
    uint32_t i = 0;
    for (i = 0; i < ff->fatHead.nfat_arch; ++i) {
        fatArch[i].cputype    = (cpu_type_t) be32toh(fatArch[i].cputype);
        fatArch[i].cpusubtype = (cpu_subtype_t) be32toh(fatArch[i].cpusubtype);
        fatArch[i].offset     = be32toh(fatArch[i].offset);
        fatArch[i].size       = be32toh(fatArch[i].size);
        fatArch[i].align      = be32toh(fatArch[i].align);
    }
#endif /* __ORDER_LITTLE_ENDIAN__ */

    return MACHO64_OK;
}

/***
 * @brief parse 64 bit fat arch descriptor
 *
 * @param[in,out] ff pointer to FatMacho64File
 *
 * @return MACHO64_OK or MACHO64_NO_MEM
*/
static MACHO64_ERROR macho64FatParseArch64(FatMacho64File *ff)
{
    FileD fd = ff->fd;
    size_t off = sizeof(FatHeader);
    size_t size = sizeof(FatArch64) * (ff->fatHead.nfat_arch);
    FatArch64 *fatArch = (FatArch64*)readFromFile(fd, &off, size);
    if (fatArch == NULL)
        return MACHO64_NO_MEM;

    ff->fatArch64 = fatArch;

#ifdef __ORDER_LITTLE_ENDIAN__
    uint32_t i = 0;
    for (i = 0; i < ff->fatHead.nfat_arch; ++i) {
        fatArch[i].cputype    = (cpu_type_t) be32toh(fatArch[i].cputype);
        fatArch[i].cpusubtype = (cpu_subtype_t) be32toh(fatArch[i].cpusubtype);
        fatArch[i].offset     = be64toh(fatArch[i].offset);
        fatArch[i].size       = be64toh(fatArch[i].size);
        fatArch[i].align      = be32toh(fatArch[i].align);
        fatArch[i].reserved   = be32toh(fatArch[i].reserved);
    }
#endif /* __ORDER_LITTLE_ENDIAN__ */

    return MACHO64_OK;
}

/***
 * @brief Inits FatHeader of a FatMacho64File
 *
 * @param[in] mf FatMacho64File file descriptor
 *
 * @return MACHO64_OK or
 *         MACHO64_INV_ARG, MACHO64_NO_MEM, MACHO64_NO_FAT_HEADER
 */
static MACHO64_ERROR macho64FatParseHeader(FatMacho64File *ff)
{
    if (ff == NULL || IS_INV_FD(ff->fd))
        return MACHO64_INV_ARG;

    size_t off = 0;
    FatHeader *fatHead = (FatHeader*)(ff->faddr + off);

    // fat header is always in big-endian
    ff->fatHead.magic = be32toh(fatHead->magic);
    ff->fatHead.nfat_arch = be32toh(fatHead->nfat_arch);
    switch (ff->fatHead.magic) {
    case FAT_MAGIC:
    case FAT_CIGAM:
        return macho64FatParseArch32(ff);
    case FAT_MAGIC_64:
    case FAT_CIGAM_64:
        return macho64FatParseArch64(ff);
    default:
        ff->fatHead.magic = 0;
        ff->fatHead.nfat_arch = 0;
        return MACHO64_NO_FAT_HEADER;
    }
}

FatMacho64File *fatMacho64Parse(const char *fn)
{
    if (fn == NULL)
        return NULL;

    FileD fd = open(fn, O_RDONLY);
    if (IS_INV_FD(fd)) {
        PERROR("open()");
        return NULL;
    }

    size_t fs = get_file_size(fd);
    if (fs == (size_t) -1) {
        return NULL;
    }

    uint8_t *faddr = map_file(fd, fs, PROT_READ);
    if (faddr == NULL) {
        return NULL;
    }

    FatMacho64File *ff = (FatMacho64File*) Calloc(1, sizeof(FatMacho64File));
    if (ff == NULL) {
        LOG_ERROR("Cannot allocate %zu bytes", sizeof(FatMacho64File));
        goto eexit_0;
    }

    ff->fd = fd;
    ff->fs = fs;
    ff->faddr = faddr;
    size_t nameLen = strlen(fn) + 1;
    if ((ff->fn = (char*) Calloc(nameLen, sizeof(char))) == NULL) {
        LOG_ERROR("Cannot allocate %zu bytes", nameLen);
        goto eexit_1;
    }

    strncpy(ff->fn, fn, nameLen);

    if (macho64FatParseHeader(ff)) {
        LOG_ERROR("Cannot parse mach-o fat header");
        goto eexit_1;
    }

    uint32_t nArch = ff->fatHead.nfat_arch;
    ff->mf = (Macho64File*) Calloc(nArch, sizeof(Macho64File));
    uint32_t i = 0;
    for (i = 0; i < nArch; ++i) {
        ff->mf[i].fd = ff->fd;
        ff->mf[i].fn = ff->fn;
        ff->mf[i].fs = ff->fs;
        ff->mf[i].faddr = ff->faddr;
        if (ff->fatArch) {
            if (_macho64Parse(ff->mf + i, ff->fatArch[i].offset))
                goto eexit_1;
        } else if (ff->fatArch64) {
            if (_macho64Parse(ff->mf + i, ff->fatArch64[i].offset))
                goto eexit_1;
        } else
            goto eexit_1;
    }

    return ff;
eexit_1:
    fatMacho64Free(ff);
    return NULL;
eexit_0:
    close(fd);
    return NULL;
}

void fatMacho64Free(FatMacho64File *ff)
{
    if (ff == NULL) {
        return;
    }
    if (ff->fn) {
        Free(ff->fn);
    }
    if (ff->fd != INV_FD) {
        close(ff->fd);
    }
    if (ff->fatArch) {
        Free(ff->fatArch);
    }
    if (ff-> mf) {
        uint32_t i = 0;
        for (i = 0; i < ff->fatHead.nfat_arch; ++i) {
            ff->mf[i].fd = INV_FD;
            ff->mf[i].fn = NULL;

            macho64Clean(ff->mf + i);
        }

        Free(ff->mf);
    }

    vt_memset_s(ff, sizeof(FatMacho64File), 0xff, sizeof(FatMacho64File));
    Free(ff);
}

MACHO64_ERROR fatMacho64Check(const FatMacho64File *mf)
{
    if (mf == NULL)
        return MACHO64_INV_ARG;

    if (mf->fatHead.nfat_arch) {
        uint32_t num = mf->fatHead.nfat_arch;
        uint32_t i = 0;
        for (i = 0; i < num; ++i) {
            MACHO64_ERROR err = macho64Check(mf->mf + i);
            if (err) {
                return err;
            }
        }
    } else {
        return MACHO64_NO_FAT_HEADER;
    }

    return MACHO64_OK;
}

#define FOR_EACH_ARCH(ff, code)         \
    uint32_t n = ff->fatHead.nfat_arch; \
    uint32_t i = 0;                     \
    for (i = 0; i < n; ++i) {           \
        Macho64File* mf = ff->mf + i;   \
        code;                           \
    }                                   \

Macho64File* fatMacho64GetMacho64ByArch(FatMacho64File *ff, Arch arch)
{
    FOR_EACH_ARCH(ff,
        if (mf->arch == arch)
            return mf;
    )

    return NULL;
}

const Macho64File* fatMacho64GetConstMacho64ByArch(const FatMacho64File *ff, Arch arch)
{
    FOR_EACH_ARCH(ff,
        if (mf->arch == arch)
            return mf;
    )

    return NULL;
}

Macho64Sym *fatMacho64GetSymByName(const FatMacho64File *ff, const char *name)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return NULL;

    return macho64GetSymByName(mf, name);
}

char *fatMacho64GetSymName(const FatMacho64File *ff, const Macho64Sym *ms)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return NULL;

    return macho64GetSymName(mf, ms);
}

Macho64Sym *fatMacho64GetSSymTab(const FatMacho64File *ff)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return NULL;

    return macho64GetSSymTab(mf);
}

uint64_t fatMacho64GetAmountSSym(const FatMacho64File *ff)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return MACHO64_INV_ARG;

    return macho64GetAmountSSym(mf);
}

uint64_t fatMacho64GetAddrSymByName(const FatMacho64File *ff, const char *name)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return MACHO64_INV_ARG;

    return macho64GetAddrSymByName(mf, name);
}

uint64_t fatMacho64GetSSymSize(const FatMacho64File *ff, const Macho64Sym *ms)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return MACHO64_INV_ARG;

    return macho64GetSSymSize(mf, ms);
}

uint64_t fatMacho64GetSSymFileoff(const FatMacho64File *ff, const Macho64Sym *sym)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return MACHO64_INV_ARG;

    return macho64GetSSymFileoff(mf, sym);
}

uint64_t fatMacho64GetAmountSeg(const FatMacho64File *ff)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return MACHO64_INV_ARG;

    return macho64GetAmountSeg(mf);
}

Macho64Sect *fatMacho64GetSectByName(const FatMacho64File *ff, const char *name)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return NULL;

    return macho64GetSectByName(mf, name);
}

Macho64Sect *fatMacho64GetLastLoadableSect(const FatMacho64File *ff)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return NULL;

    return macho64GetLastLoadableSect(mf);
}

void *fatMacho64ReadSect(const FatMacho64File *ff, const Macho64Sect *sect)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return NULL;

    return macho64ReadSect(mf, sect);
}

uint64_t fatMacho64GetAmountSect(const FatMacho64File *ff)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return MACHO64_INV_ARG;

    return macho64GetAmountSect(mf);
}

const char* fatMacho64GetSectName(const FatMacho64File *ff, const Macho64Sect *sect)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return NULL;

    return macho64GetSectName(mf, sect);
}

