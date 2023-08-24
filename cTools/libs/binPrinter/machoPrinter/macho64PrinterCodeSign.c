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
#include "endian.h"
#include "macho64Printer.h"

#include <stdio.h>
#include <inttypes.h>
#ifdef OPENSSL
# include <openssl/pkcs7.h>
#endif /* OPENSSL */
#include <Kernel/kern/cs_blobs.h>

static
void macho64PrintRequirements(const CS_SuperBlob *sb)
{
    uint32_t numReq = be32toh(sb->count);
    printf("%17s: 0x%"PRIx32"\n", "magic", be32toh(sb->magic));
    printf("%17s: 0x%"PRIx32"\n", "length", be32toh(sb->length));
    printf("%17s: %"PRIu32"\n", "count", numReq);

    uint32_t i = 0;
    for (i = 0; i < numReq; ++i) {
        uint32_t offReq = be32toh(sb->index[i].offset);
        uint32_t typeReq = be32toh(sb->index[i].type);
        const CS_GenericBlob *req = (const CS_GenericBlob *)(((const uint8_t*)sb) + offReq);
        printf("%17s[%"PRIu32"]: (type: 0x%"PRIx32", offset: %"PRIu32")\n"
                , "Requirement", i, typeReq, offReq);
        printf("%17s: 0x%"PRIx32"\n", "magic", be32toh(req->magic));
        printf("%17s: 0x%"PRIx32"\n", "length", be32toh(req->length));
        printf("%17s%.*s\n", "", be32toh(req->length), req->data);
    }
}

static
const char* getHashType(uint8_t hashType)
{
    switch (hashType) {
    case CS_HASHTYPE_SHA1:
        return "SHA1";
    case CS_HASHTYPE_SHA256:
        return "SHA256";
    case CS_HASHTYPE_SHA256_TRUNCATED:
        return "SHA256_TRUNCATED";
    case CS_HASHTYPE_SHA384:
        return "SHA384";
    default:
        return "UNKNOWN";
    }
}

static
void macho64PrintCodeDirectory(const CS_CodeDirectory* cd)
{
    printf("%17s: 0x%"PRIx32"\n", "magic", be32toh(cd->magic));
    printf("%17s: %"PRIu32"\n", "length", be32toh(cd->length));
    uint32_t version = be32toh(cd->version);
    printf("%17s: %"PRIu32".%"PRIu32".%"PRIu32"\n", "version"
                                                  , version >> 16
                                                  , (version >> 8) & 0xFF
                                                  , version & 0xFF);
    printf("%17s: 0x%"PRIx32"\n", "flags", be32toh(cd->flags));
    uint32_t hashOffset = be32toh(cd->hashOffset);
    const uint8_t *hashBase = ((const uint8_t*)cd) + hashOffset;
    printf("%17s: %"PRIu32"\n", "hashOffset", hashOffset);
    uint32_t identOffset = be32toh(cd->identOffset);
    const char *identifier = ((const char*)cd) + identOffset;
    printf("%17s: %"PRIu32" (%s)\n", "identOffset", identOffset, identifier);
    uint32_t nSpecialSlots = be32toh(cd->nSpecialSlots);
    printf("%17s: %"PRIu32"\n", "nSpecialSlots", nSpecialSlots);
    uint32_t nCodeSlots = be32toh(cd->nCodeSlots);
    printf("%17s: %"PRIu32"\n", "nCodeSlots", nCodeSlots);
    printf("%17s: 0x%"PRIx32"\n", "codeLimit", be32toh(cd->codeLimit));
    uint8_t hashSize = cd->hashSize;
    printf("%17s: %"PRIu8"\n", "hashSize", hashSize);
    printf("%17s: %s\n", "hashType", getHashType(cd->hashType));
    printf("%17s: %"PRIu8"\n", "platform", cd->platform);
    printf("%17s: %"PRIu32"\n", "pageSize", 1 << cd->pageSize);
    printf("%17s: %"PRIu32"\n", "spare2", be32toh(cd->spare2));

    switch (version) {
    case CS_SUPPORTSSCATTER:
        printf("%17s: %"PRIu32"\n", "scatterOffset", be32toh(cd->scatterOffset));
        break;
    case CS_SUPPORTSTEAMID:
        printf("%17s: %"PRIu32"\n", "teamOffset", be32toh(cd->teamOffset));
        break;
    case CS_SUPPORTSCODELIMIT64:
        printf("%17s: %"PRIu32"\n", "spare3", be32toh(cd->spare3));
        printf("%17s: %"PRIu64"\n", "codeLimit64", be64toh(cd->codeLimit64));
        break;
    case CS_SUPPORTSEXECSEG:
        printf("%17s: 0x%"PRIx64"\n", "execSegBase", be64toh(cd->execSegBase));
        printf("%17s: 0x%"PRIx64"\n", "execSegLimit", be64toh(cd->execSegLimit));
        printf("%17s: 0x%"PRIx64"\n", "execSegFlags", be64toh(cd->execSegFlags));
        break;
    case CS_SUPPORTSRUNTIME:
        printf("%17s: %"PRIu32"\n", "runtime", be32toh(cd->runtime));
        printf("%17s: 0x%"PRIx32"\n", "preEncryptOffset", be32toh(cd->preEncryptOffset));
        break;
    case CS_SUPPORTSLINKAGE:
        printf("%17s: %"PRIu8"\n", "linkageHashType", cd->linkageHashType);
        printf("%17s: %"PRIu8"\n", "linkageApplicationType", cd->linkageApplicationType);
        printf("%17s: %"PRIu16"\n", "linkageApplicationSubType"
                                  , htobe16(cd->linkageApplicationSubType));
        printf("%17s: 0x%"PRIx32"\n", "linkageOffset", be32toh(cd->linkageOffset));
        printf("%17s: %"PRIu32"\n", "linkageSize", be32toh(cd->linkageSize));
        break;
    default:
        break;
    }

    uint32_t i = 0;
    for (i = nSpecialSlots; i > 0; --i) {
        printf("%17s[%d]: ", "HashSlot", -i);
        uint8_t j = 0;
        for (j = 0; j < hashSize; ++j) {
            printf("%.2"PRIx8, *(hashBase - i*hashSize + j));
        }
        NEW_LINE;
    }

    // hash sum of loadable pages
    for (i = 0; i < nCodeSlots; ++i) {
        printf("%17s[%d]: ", "HashSlot", i);
        uint8_t j = 0;
        for (j = 0; j < hashSize; ++j) {
            printf("%.2"PRIx8, *(hashBase + i*hashSize + j));
        }
        NEW_LINE;
    }
}

void macho64PrintCodeSign(const Macho64File *mf)
{
    printf("Code Signature:\n");
    if (mf->sign == NULL) {
        NEW_LINE;
        return;
    }

    /***
     * Code signing is an essential part of build process. It is required for
     * installing and distributing an iOS app. After signing, the
     * LC_CODE_SIGNATURE load command is appended to the mach-o binary.
     * Since code signing is the last step of the build process
     * (you shouldn't alter anything after signing), this load command is
     * always at the end of a binary.
     *
     * In a universal binary, each architecture is signed independently.
    */
    FileD fd = mf->fd;
    size_t foff = mf->hOff + mf->sign->dataoff;
    size_t fsize = mf->sign->datasize;
    // everything in the structure are in big endian
    CS_SuperBlob *sb = (CS_SuperBlob*)readFromFile(fd, &foff, fsize);

    printf("Super Blob:\n");
    // should be CSMAGIC_EMBEDDED_SIGNATURE = 0xfade0cc0
    printf("%9s: 0x%x\n", "magic", be32toh(sb->magic));
    printf("%9s: %d\n", "length", be32toh(sb->length));
    printf("%9s: %d\n", "count", be32toh(sb->count));

    uint32_t i = 0;
    for (i = 0; i < be32toh(sb->count); ++i) {
        CS_BlobIndex *b = sb->index + i;
        uint32_t type = be32toh(b->type);
        uint32_t off = be32toh(b->offset);
        printf("%13s[%u]: (type: 0x%.8x, offset: %d)\n", "Blob", i, type, off);

        CS_GenericBlob *gb = (CS_GenericBlob*)(((uint8_t*)sb) + off);
        uint32_t magic = be32toh(gb->magic);
        uint32_t length = be32toh(gb->length);
        switch (magic) {
        case CSMAGIC_BLOBWRAPPER:
            printf("%17s: 0x%"PRIx32"\n", "magic", magic);
            printf("%17s: 0x%"PRIx32"\n", "length", length);
#ifdef OPENSSL
            const unsigned char *pksc7In = (const unsigned char *)(gb->data);
            PKCS7* pkcs7 = d2i_PKCS7(NULL, &pksc7In, length - 8);
            assert(pkcs7 != NULL);
            BIO *bio = BIO_new(BIO_s_file());
            BIO_set_fp(bio, stdout, BIO_NOCLOSE);
            PKCS7_print_ctx(bio, pkcs7, 4, NULL);
            BIO_free(bio);
            PKCS7_free(pkcs7);
#endif /* OPENSSL */
            break;
        case CSMAGIC_REQUIREMENTS:
            macho64PrintRequirements((CS_SuperBlob*)gb);
            break;
        case CSMAGIC_CODEDIRECTORY:
            macho64PrintCodeDirectory((CS_CodeDirectory*)gb);
            break;
        case CSMAGIC_EMBEDDED_ENTITLEMENTS:
            printf("%17s: 0x%"PRIx32"\n", "magic", magic);
            printf("%17s: %.*s\n", "Entitlements", length, gb->data);
            break;
        default:
            printf("Skipping block with magic: 0x%x\n", magic);
            break;
        }
    }

    NEW_LINE;

    Free(sb);
}

