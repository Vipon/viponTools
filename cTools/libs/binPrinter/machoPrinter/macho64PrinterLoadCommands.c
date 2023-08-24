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

#include "macho64Printer.h"

#include <time.h>
#include <inttypes.h>

static
void macho64PrintLinkEditData(const MachoLinkEditData *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"       , "cmd"      , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n", "cmdsize"  , lc->cmdsize);
    printf("%9s: %"PRIu32"\n", "dataoff"  , lc->dataoff);
    printf("%9s: %"PRIu32"\n", "datasize" , lc->datasize);
}

static
void macho64PrintLComSegment(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComSymtab(const Macho64File *mf, const LoadCommand *lc)
{
    UNUSED(mf);

    const SymtabCommand *symtab = (const SymtabCommand *)lc;
    uint32_t cmdNum = symtab->cmd & (~LC_REQ_DYLD);

    printf("%9s: %s\n"       , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n", "cmdsize", symtab->cmdsize);
    printf("%9s: %"PRIu32"\n", "symoff" , symtab->symoff);
    printf("%9s: %"PRIu32"\n", "nsyms"  , symtab->nsyms);
    printf("%9s: %"PRIu32"\n", "stroff" , symtab->stroff);
    printf("%9s: %"PRIu32"\n", "strsize", symtab->strsize);
}

static
void macho64PrintLComSymseg(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComThread(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComUnixthread(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComLoadfvmlib(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComIdfvmlib(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComIdent(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComFvmfile(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComPrepage(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComDysymtab(const Macho64File *mf, const LoadCommand *lc)
{
    UNUSED(mf);
    const DysymtabCommand *dynsym = (const DysymtabCommand*)lc;

    uint32_t cmdNum = dynsym->cmd & (~LC_REQ_DYLD);
    printf("%14s: %s\n"       , "cmd"           , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%14s: %"PRIu32"\n", "cmdsize"       , dynsym->cmdsize);
    printf("%14s: %"PRIu32"\n", "ilocalsym"     , dynsym->ilocalsym);
    printf("%14s: %"PRIu32"\n", "nlocalsym"     , dynsym->nlocalsym);
    printf("%14s: %"PRIu32"\n", "iextdefsym"    , dynsym->iextdefsym);
    printf("%14s: %"PRIu32"\n", "nextdefsym"    , dynsym->nextdefsym);
    printf("%14s: %"PRIu32"\n", "iundefsym"     , dynsym->iundefsym);
    printf("%14s: %"PRIu32"\n", "nundefsym"     , dynsym->nundefsym);
    printf("%14s: %"PRIu32"\n", "tocoff"        , dynsym->tocoff);
    printf("%14s: %"PRIu32"\n", "ntoc"          , dynsym->ntoc);
    printf("%14s: %"PRIu32"\n", "modtaboff"     , dynsym->modtaboff);
    printf("%14s: %"PRIu32"\n", "nmodtab"       , dynsym->nmodtab);
    printf("%14s: %"PRIu32"\n", "extrefsymoff"  , dynsym->extrefsymoff);
    printf("%14s: %"PRIu32"\n", "nextrefsyms"   , dynsym->nextrefsyms);
    printf("%14s: %"PRIu32"\n", "indirectsymoff", dynsym->indirectsymoff);
    printf("%14s: %"PRIu32"\n", "nindirectsyms" , dynsym->nindirectsyms);
    printf("%14s: %"PRIu32"\n", "extreloff"     , dynsym->extreloff);
    printf("%14s: %"PRIu32"\n", "nextrel"       , dynsym->nextrel);
    printf("%14s: %"PRIu32"\n", "locreloff"     , dynsym->locreloff);
    printf("%14s: %"PRIu32"\n", "nlocrel"       , dynsym->nlocrel);
}

static
void macho64PrintLComLoadDylib(const Macho64File *mf, const LoadCommand *lc)
{
    UNUSED(mf);
    const MachoDylibCommand *dl = (const MachoDylibCommand*)lc;
    uint32_t cmdNum = dl->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"                   , "cmd"
                                         , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"            , "cmdsize"
                                         , dl->cmdsize);
    printf("%9s: %s (offset %"PRIu32")\n", "name"
                                         , (char*)(((size_t)dl) + dl->dylib.name.offset)
                                         , dl->dylib.name.offset);
    time_t time = dl->dylib.timestamp;
    printf("%9s: %s"                     , "timestamp", asctime(gmtime(&time)));
    printf("%9s: %"PRIu32".%"PRIu32".%"PRIu32"\n"
                                         , "dylib ver"
                                         , dl->dylib.current_version >> 16
                                         , (dl->dylib.current_version >> 8) & 0xFF
                                         , dl->dylib.current_version & 0xFF);
    printf("%9s: %"PRIu32".%"PRIu32".%"PRIu32"\n"
                                         , "comp ver"
                                         , dl->dylib.compatibility_version >> 16
                                         , (dl->dylib.compatibility_version >> 8) & 0xFF
                                         , dl->dylib.compatibility_version & 0xFF);
}

static
void macho64PrintLComIdDylib(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComLoadDylinker(const Macho64File *mf, const LoadCommand *lc)
{
    UNUSED(mf);

    const MachoDylinkerCom *dylinker = (const MachoDylinkerCom*)lc;
    uint32_t cmdNum = dylinker->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"       , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n", "cmdsize", dylinker->cmdsize);
    printf("%9s: %s\n"       , "name"   , (char*)(((size_t)dylinker) + dylinker->name.offset));
}

static
void macho64PrintLComIdDylinker(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComPreboundDylib(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComRoutines(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComSubFramework(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComSubUmbrella(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComSubClient(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComSubLib(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComTwolevelHints(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComPrebindCksum(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComLoadWeakDylib(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComSegment64(const Macho64File *mf, const LoadCommand *lc)
{
    UNUSED(mf);

    const Macho64Seg *seg = (const Macho64Seg*)lc;
    uint32_t cmdNum = seg->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"       , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n", "cmdsize", seg->cmdsize);

    macho64PrintSegment(seg);
}

static
void macho64PrintLComRoutines64(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComUUID(const Macho64File *mf, const LoadCommand *lc)
{
    UNUSED(mf);
    const MachoUUIDComm *uuid = (const MachoUUIDComm*)lc;

    uint32_t cmdNum = uuid->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"       , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n", "cmdsize", uuid->cmdsize);

    printf("%9s: "           , "uuid");
    uint8_t i = 0;
    for (i = 0; i < sizeof(uuid->uuid); ++i) {
        printf("%.2X", uuid->uuid[i]);
    }
    NEW_LINE;
}

static
void macho64PrintLComRpath(const Macho64File *mf, const LoadCommand *lc)
{
    UNUSED(mf);
    const MachoRpathComm *rpath = (const MachoRpathComm*)lc;

    uint32_t cmdNum = rpath->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"       , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n", "cmdsize", rpath->cmdsize);
    printf("%9s: %s\n"       , "path"   , (char*)(((size_t)rpath) + rpath->path.offset));
}

static
void macho64PrintLComCodeSig(const Macho64File *mf, const LoadCommand *lc)
{
    UNUSED(mf);
    macho64PrintLinkEditData((const MachoLinkEditData*)lc);
}

static
void macho64PrintLComSegSplitInfo(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComReexportDylib(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComLazyLoadDylib(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComEncryptionInfo(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComDyldInfo(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComLoadUpwardDylib(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComVerMinMacosx(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComVerMinIphoneos(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComFunctionStarts(const Macho64File *mf, const LoadCommand *lc)
{
    UNUSED(mf);

    const MachoLinkEditData *funcStarts = (const MachoLinkEditData*)lc;
    macho64PrintLinkEditData(funcStarts);
}

static
void macho64PrintLComDyldEnv(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComMain(const Macho64File *mf, const LoadCommand *lc)
{
    UNUSED(mf);

    const MachoEntryPointCom *entry = (const MachoEntryPointCom*)lc;
    uint32_t cmdNum = entry->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"       , "cmd"      , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n", "cmdsize"  , entry->cmdsize);
    printf("%9s: %"PRIu64"\n", "entryoff" , entry->entryoff);
    printf("%9s: %"PRIu64"\n", "stacksize", entry->stacksize);
}

static
void macho64PrintLComDataInCode(const Macho64File *mf, const LoadCommand *lc)
{
    UNUSED(mf);

    const MachoLinkEditData *dataInCode = (const MachoLinkEditData*)lc;
    macho64PrintLinkEditData(dataInCode);
}

static
void macho64PrintLComSourceVer(const Macho64File *mf, const LoadCommand *lc)
{
    UNUSED(mf);
    const MachoSourceVer *ver = (const MachoSourceVer*)lc;

    uint32_t cmdNum = ver->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"       , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n", "cmdsize", ver->cmdsize);
    // A.B.C.D.E packed as a24.b10.c10.d10.e10
    printf("%9s: %"PRIu64".%"PRIu64".%"PRIu64".%"PRIu64".%"PRIu64"\n"
                             , "version", ver->version >> 40
                                        , (ver->version >> 30) & 0x3ff
                                        , (ver->version >> 20) & 0x3ff
                                        , (ver->version >> 10) & 0x3ff
                                        , ver->version & 0x3ff);
}

static
void macho64PrintLComDylibCodeSignDrs(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComEncryptInfo64(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComLinkerOpt(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComLinkerOptHint(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComVerMinTvos(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComVerMinWatchos(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

static
void macho64PrintLComNote(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}

const Macho64Platform MACHO64_PLATFORM[] = {
    { 0                         , ""                },
    { PLATFORM_MACOS            , "MACOS"           },
    { PLATFORM_IOS              , "IOS"             },
    { PLATFORM_TVOS             , "TVOS"            },
    { PLATFORM_WATCHOS          , "WATCHOS"         },
    { PLATFORM_BRIDGEOS         , "BRIDGEOS"        },
    { PLATFORM_MACCATALYST      , "MACCATALYST"     },
    { PLATFORM_IOSSIMULATOR     , "IOSSIMULATOR"    },
    { PLATFORM_TVOSSIMULATOR    , "TVOSSIMULATOR"   },
    { PLATFORM_WATCHOSSIMULATOR , "WATCHOSSIMULATOR"},
    { PLATFORM_DRIVERKIT        , "DRIVERKIT"       },
};

const Macho64Tool MACHO64_TOOL[] = {
    { 0         , ""      },
    { TOOL_CLANG, "CLANG" },
    { TOOL_SWIFT, "SWIFT" },
    { TOOL_LD   , "LD"    },
};

static
void macho64PrintLComBuildVer(const Macho64File *mf, const LoadCommand *lc)
{
    UNUSED(mf);
    const MachoBuildVer *ver = (const MachoBuildVer*)lc;
    uint32_t cmdNum = ver->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"       , "cmd"     , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n", "cmdsize" , ver->cmdsize);
    printf("%9s: %s\n"       , "platform", MACHO64_PLATFORM[ver->platform].str);
    // X.Y.Z is encoded in nibbles xxxx.yy.zz
    printf("%9s: %"PRIu32".%"PRIu32".%"PRIu32"\n"
                             , "minos"   , ver->minos >> 16
                                         , (ver->minos >> 8) & 0xff
                                         , ver->minos & 0xff);
    printf("%9s: %"PRIu32".%"PRIu32".%"PRIu32"\n"
                             , "sdk"     , ver->sdk >> 16
                                         , (ver->sdk >> 8) & 0xff
                                         , ver->sdk & 0xff);
    printf("%9s: %"PRIu32"\n", "ntools"  , ver->ntools);

    uint32_t i = 0;
    const MachoBuildToolVer *toolVer =
        (const MachoBuildToolVer*)(((size_t)ver) + sizeof(MachoBuildVer));
    for (i = 0; i < ver->ntools; ++i) {
        printf("%9s: %s\n"   , "tool"    , MACHO64_TOOL[toolVer[i].tool].str);
        printf("%9s: %"PRIu32".%"PRIu32".%"PRIu32"\n"
                             , "version" , toolVer[i].version >> 16
                                         , (toolVer[i].version >> 8) & 0xff
                                         , toolVer[i].version & 0xff);
    }
}

static
void macho64PrintLComDyldExportsTrie(const Macho64File *mf, const LoadCommand *lc)
{
    UNUSED(mf);

    const MachoLinkEditData *dylibExportsTrie = (const MachoLinkEditData*)lc;
    macho64PrintLinkEditData(dylibExportsTrie);
}

static
void macho64PrintLComDyldChainedFixups(const Macho64File *mf, const LoadCommand *lc)
{
    UNUSED(mf);

    const MachoLinkEditData *dylibChainedFixups = (const MachoLinkEditData*)lc;
    macho64PrintLinkEditData(dylibChainedFixups);
}

static
void macho64PrintLComFilesetEntry(const Macho64File *mf, const LoadCommand *lc)
{
    uint32_t cmdNum = lc->cmd & (~LC_REQ_DYLD);
    printf("%9s: %s\n"         , "cmd"    , MACHO64_LOAD_COMMAND[cmdNum].str);
    printf("%9s: %"PRIu32"\n"  , "cmdsize", lc->cmdsize);
    UNUSED(mf);
    UNUSED(lc);
}


const Macho64LoadCommand MACHO64_LOAD_COMMAND[] = {
    { 0, NULL, NULL },
    {LC_SEGMENT                 , "LC_SEGMENT"                 , macho64PrintLComSegment          },
    {LC_SYMTAB                  , "LC_SYMTAB"                  , macho64PrintLComSymtab           }, // done
    {LC_SYMSEG                  , "LC_SYMSEG"                  , macho64PrintLComSymseg           }, // obsolete
    {LC_THREAD                  , "LC_THREAD"                  , macho64PrintLComThread           },
    {LC_UNIXTHREAD              , "LC_UNIXTHREAD"              , macho64PrintLComUnixthread       },
    {LC_LOADFVMLIB              , "LC_LOADFVMLIB"              , macho64PrintLComLoadfvmlib       },
    {LC_IDFVMLIB                , "LC_IDFVMLIB"                , macho64PrintLComIdfvmlib         },
    {LC_IDENT                   , "LC_IDENT"                   , macho64PrintLComIdent            }, // obsolete
    {LC_FVMFILE                 , "LC_FVMFILE"                 , macho64PrintLComFvmfile          },
    {LC_PREPAGE                 , "LC_PREPAGE"                 , macho64PrintLComPrepage          },
    {LC_DYSYMTAB                , "LC_DYSYMTAB"                , macho64PrintLComDysymtab         }, // done
    {LC_LOAD_DYLIB              , "LC_LOAD_DYLIB"              , macho64PrintLComLoadDylib        }, // done
    {LC_ID_DYLIB                , "LC_ID_DYLIB"                , macho64PrintLComIdDylib          },
    {LC_LOAD_DYLINKER           , "LC_LOAD_DYLINKER"           , macho64PrintLComLoadDylinker     }, // done
    {LC_ID_DYLINKER             , "LC_ID_DYLINKER"             , macho64PrintLComIdDylinker       },
    {LC_PREBOUND_DYLIB          , "LC_PREBOUND_DYLIB"          , macho64PrintLComPreboundDylib    },
    {LC_ROUTINES                , "LC_ROUTINES"                , macho64PrintLComRoutines         },
    {LC_SUB_FRAMEWORK           , "LC_SUB_FRAMEWORK"           , macho64PrintLComSubFramework     },
    {LC_SUB_UMBRELLA            , "LC_SUB_UMBRELLA"            , macho64PrintLComSubUmbrella      },
    {LC_SUB_CLIENT              , "LC_SUB_CLIENT"              , macho64PrintLComSubClient        },
    {LC_SUB_LIBRARY             , "LC_SUB_LIBRARY"             , macho64PrintLComSubLib           },
    {LC_TWOLEVEL_HINTS          , "LC_TWOLEVEL_HINTS"          , macho64PrintLComTwolevelHints    },
    {LC_PREBIND_CKSUM           , "LC_PREBIND_CKSUM"           , macho64PrintLComPrebindCksum     },
    {LC_LOAD_WEAK_DYLIB         , "LC_LOAD_WEAK_DYLIB"         , macho64PrintLComLoadWeakDylib    },
    {LC_SEGMENT_64              , "LC_SEGMENT_64"              , macho64PrintLComSegment64        }, // done
    {LC_ROUTINES_64             , "LC_ROUTINES_64"             , macho64PrintLComRoutines64       },
    {LC_UUID                    , "LC_UUID"                    , macho64PrintLComUUID             }, // done
    {LC_RPATH                   , "LC_RPATH"                   , macho64PrintLComRpath            }, // done
    {LC_CODE_SIGNATURE          , "LC_CODE_SIGNATURE"          , macho64PrintLComCodeSig          }, // done
    {LC_SEGMENT_SPLIT_INFO      , "LC_SEGMENT_SPLIT_INFO"      , macho64PrintLComSegSplitInfo     },
    {LC_REEXPORT_DYLIB          , "LC_REEXPORT_DYLIB"          , macho64PrintLComReexportDylib    },
    {LC_LAZY_LOAD_DYLIB         , "LC_LAZY_LOAD_DYLIB"         , macho64PrintLComLazyLoadDylib    },
    {LC_ENCRYPTION_INFO         , "LC_ENCRYPTION_INFO"         , macho64PrintLComEncryptionInfo   },
    {LC_DYLD_INFO               , "LC_DYLD_INFO"               , macho64PrintLComDyldInfo         },
    {LC_LOAD_UPWARD_DYLIB       , "LC_LOAD_UPWARD_DYLIB"       , macho64PrintLComLoadUpwardDylib  },
    {LC_VERSION_MIN_MACOSX      , "LC_VERSION_MIN_MACOSX"      , macho64PrintLComVerMinMacosx     },
    {LC_VERSION_MIN_IPHONEOS    , "LC_VERSION_MIN_IPHONEOS"    , macho64PrintLComVerMinIphoneos   },
    {LC_FUNCTION_STARTS         , "LC_FUNCTION_STARTS"         , macho64PrintLComFunctionStarts   }, // done
    {LC_DYLD_ENVIRONMENT        , "LC_DYLD_ENVIRONMENT"        , macho64PrintLComDyldEnv          },
    {LC_MAIN                    , "LC_MAIN"                    , macho64PrintLComMain             }, // done
    {LC_DATA_IN_CODE            , "LC_DATA_IN_CODE"            , macho64PrintLComDataInCode       }, // done
    {LC_SOURCE_VERSION          , "LC_SOURCE_VERSION"          , macho64PrintLComSourceVer        }, // done
    {LC_DYLIB_CODE_SIGN_DRS     , "LC_DYLIB_CODE_SIGN_DRS"     , macho64PrintLComDylibCodeSignDrs },
    {LC_ENCRYPTION_INFO_64      , "LC_ENCRYPTION_INFO_64"      , macho64PrintLComEncryptInfo64    },
    {LC_LINKER_OPTION           , "LC_LINKER_OPTION"           , macho64PrintLComLinkerOpt        },
    {LC_LINKER_OPTIMIZATION_HINT, "LC_LINKER_OPTIMIZATION_HINT", macho64PrintLComLinkerOptHint    },
    {LC_VERSION_MIN_TVOS        , "LC_VERSION_MIN_TVOS"        , macho64PrintLComVerMinTvos       },
    {LC_VERSION_MIN_WATCHOS     , "LC_VERSION_MIN_WATCHOS"     , macho64PrintLComVerMinWatchos    },
    {LC_NOTE                    , "LC_NOTE"                    , macho64PrintLComNote             },
    {LC_BUILD_VERSION           , "LC_BUILD_VERSION"           , macho64PrintLComBuildVer         }, // done
    {LC_DYLD_EXPORTS_TRIE       , "LC_DYLD_EXPORTS_TRIE"       , macho64PrintLComDyldExportsTrie  },
    {LC_DYLD_CHAINED_FIXUPS     , "LC_DYLD_CHAINED_FIXUPS"     , macho64PrintLComDyldChainedFixups},
    {LC_FILESET_ENTRY           , "LC_FILESET_ENTRY"           , macho64PrintLComFilesetEntry     },
};

void macho64PrintLComs(const Macho64File *mf)
{
    FOREACH_LOAD_COMMAND(mf,
        printf("Load Command: %u\n", i);
        uint32_t cmdNum = lcom->cmd & (~LC_REQ_DYLD);
        MACHO64_LOAD_COMMAND[cmdNum].print(mf, lcom);
    );

    NEW_LINE;
}

