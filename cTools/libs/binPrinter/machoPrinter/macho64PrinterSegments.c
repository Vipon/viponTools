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
#include "macho64Printer.h"

#include <stdio.h>
#include <stdbool.h>
#include <inttypes.h>

const Macho64SegmentFlag MACHO64_SEGMENT_FLAGS[] = {
    { SG_HIGHVM             , "HIGHVM"              },
    { SG_FVMLIB             , "FVMLIB"              },
    { SG_NORELOC            , "NORELOC"             },
    { SG_PROTECTED_VERSION_1, "PROTECTED_VERSION_1" },
    { SG_READ_ONLY          , "READ_ONLY"           },
};

static
void macho64PrintSegmentFlags(const Macho64Seg *seg)
{
    int i = sizeof(MACHO64_SEGMENT_FLAGS) / sizeof(Macho64SegmentFlag) - 1;
    bool notFirstFlags = false;
    for (;i >= 0; --i) {
        if (seg->flags & MACHO64_SEGMENT_FLAGS[i].flag) {
            if (notFirstFlags)
                printf(" | ");
            printf("%s", MACHO64_SEGMENT_FLAGS[i].str);
            notFirstFlags = true;
        }
    }
}

void macho64PrintSegment(const Macho64Seg *seg)
{
    printf("%9s: %s\n"         , "segname" , seg->segname);
    printf("%9s: 0x%"PRIx64"\n", "vmaddr"  , seg->vmaddr);
    printf("%9s: 0x%"PRIx64"\n", "vmsize"  , seg->vmsize);
    printf("%9s: %"PRIu64"\n"  , "fileoff" , seg->fileoff);
    printf("%9s: %"PRIu64"\n"  , "filesize", seg->filesize);
    if (seg->maxprot) {
    printf("%9s: %s\n"         , "maxprot" , getMProtStr(seg->maxprot));
    } else {
    printf("%9s: 0\n"          ,"maxprot");
    }
    if (seg->initprot) {
    printf("%9s: %s\n"         , "initprot", getMProtStr(seg->initprot));
    } else {
    printf("%9s: 0\n"          , "initprot");
    }
    printf("%9s: %"PRIu32"\n"   , "nsects"  , seg->nsects);
    printf("%9s: 0x%"PRIx32     , "flags"   , seg->flags);
    if (seg->flags) {
        SPACE;
        macho64PrintSegmentFlags(seg);
    }
    NEW_LINE;
    macho64PrintSectionsInSegment(seg);
}

void macho64PrintSegments(const Macho64File *mf)
{
    uint32_t segNum = 0;
    FOREACH_LOAD_COMMAND(mf,
        if (lcom->cmd == LC_SEGMENT_64) {
            Macho64Seg *seg = (Macho64Seg*)lcom;

            printf("Segment %"PRIu32":\n", segNum);
            ++segNum;
            macho64PrintSegment(seg);
        }
    )
}

