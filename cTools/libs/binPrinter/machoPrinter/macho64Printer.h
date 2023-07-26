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

#ifndef __MACHO_64_PRINTER_H
#define __MACHO_64_PRINTER_H

#include "comdef.h"
#include "macho64Parse.h"

typedef struct {
    uint32_t    flag;
    const char  *str;
} Macho64Flag;

typedef Macho64Flag Macho64HeaderFlag;
extern const Macho64HeaderFlag MACHO64_HEADERS_FLAGS[];

typedef void (*macho64PrintLoadCommand)(const Macho64File *mf, const LoadCommand *lc);
typedef struct {
    uint32_t                cmd;
    const char              *str;
    macho64PrintLoadCommand print;
} Macho64LoadCommand;

extern const Macho64LoadCommand MACHO64_LOAD_COMMAND[];

typedef Macho64Flag Macho64SegmentFlag;
extern const Macho64SegmentFlag MACHO64_SEGMENT_FLAGS[];

typedef Macho64Flag Macho64SectionFlag;
extern const Macho64SectionFlag MACHO64_SECTION_FLAGS[];

typedef Macho64Flag Macho64StubType;
extern const Macho64StubType MACHO64_STUB_TYPE[];

typedef Macho64Flag Macho64SymType;
extern const Macho64SymType MACHO64_SYM_TYPE[];

typedef Macho64Flag Macho64Platform;
extern const Macho64Platform MACHO64_PLATFORM[];

typedef Macho64Flag Macho64Tool;
extern const Macho64Tool MACHO64_TOOL[];

EXPORT_FUNC
void macho64PrintHeader(const Macho64File *mf);

EXPORT_FUNC
void macho64PrintSymbol(const Macho64File *mf, const Macho64Sym *ms);

EXPORT_FUNC
void macho64PrintSymbols(const Macho64File *mf);

EXPORT_FUNC
void macho64PrintSection(const Macho64Sect *sect);

EXPORT_FUNC
void macho64PrintSectionsInSegment(const Macho64Seg *seg);

EXPORT_FUNC
void macho64PrintSections(const Macho64File *mf);

EXPORT_FUNC
void macho64PrintSegment(const Macho64Seg *seg);

EXPORT_FUNC
void macho64PrintSegments(const Macho64File *mf);

EXPORT_FUNC
void macho64PrintFuncStarts(const Macho64File *mf);

EXPORT_FUNC
void macho64PrintLComs(const Macho64File *mf);

EXPORT_FUNC
void macho64PrintCodeSign(const Macho64File *mf);

#endif /* __MACHO_64_PRINTER_H */

