/***
 * MIT License
 *
 * Copyright (c) 2024 Konychev Valerii
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
#include "macho64.h"
#include "macho64Printer.h"
#include <inttypes.h>
#include <stdbool.h>

const Macho64ImportsFormat MACHO64_IMPORTS_FORMAT[] = {
    {0, ""},
    {DYLD_CHAINED_IMPORT , "chained_import"},
    {DYLD_CHAINED_IMPORT_ADDEND , "chained_import_addend"},
    {DYLD_CHAINED_IMPORT_ADDEND64 , "chained_import_addend64"},
};

static void
macho64PrintFixupsHeader(const macho_fixups_headers_t *fixups_head)
{
    printf(" fixups_headers:\n");
    printf("%18s: 0x%.8"PRIx32"\n", "fixups_version", fixups_head->fixups_version);
    printf("%18s: 0x%.8"PRIx32"\n", "starts_offset", fixups_head->starts_offset);
    printf("%18s: 0x%.8"PRIx32"\n", "imports_offset", fixups_head->imports_offset);
    printf("%18s: 0x%.8"PRIx32"\n", "symbols_offset", fixups_head->symbols_offset);
    printf("%18s: 0x%.8"PRIx32"\n", "imports_count", fixups_head->imports_count);
    printf("%18s: %s\n", "imports_format",
        MACHO64_IMPORTS_FORMAT[fixups_head->imports_format].str);
    printf("%18s: %s\n", "symbols_format",
        fixups_head->symbols_format ? "zlib compressed" : "uncompressed");
}

const Macho64PointerFormat MACHO64_POINTER_FORMAT[] = {
    {0, ""},
    {DYLD_CHAINED_PTR_ARM64E, "arm64e" },
    {DYLD_CHAINED_PTR_64, "64" },
    {DYLD_CHAINED_PTR_32, "32" },
    {DYLD_CHAINED_PTR_32_CACHE, "32_cache" },
    {DYLD_CHAINED_PTR_32_FIRMWARE, "32_firmware" },
    {DYLD_CHAINED_PTR_64_OFFSET, "64_offset" },
    {DYLD_CHAINED_PTR_ARM64E_KERNEL, "arm64e_kernel" },
    {DYLD_CHAINED_PTR_64_KERNEL_CACHE, "64_kernel_cache" },
    {DYLD_CHAINED_PTR_ARM64E_USERLAND, "arm64e_userland" },
    {DYLD_CHAINED_PTR_ARM64E_FIRMWARE, "arm64e_firmware" },
    {DYLD_CHAINED_PTR_X86_64_KERNEL_CACHE, "x86_64_kernel_cache" },
};

static void
macho64PrintStartsInSegment64_offset( const Macho64File *mf
                                    , const macho_fixups_headers_t *fixups_head
                                    , const macho_starts_in_segment_t *starts_in_segment
                                    )
{
    uint16_t i = 0;
    FileD fd = mf->fd;
    const macho_chained_import_t *import = (const macho_chained_import_t*)(
        (const uint8_t*)fixups_head + fixups_head->imports_offset
    );
    const char *names = (const char*)(
        (const uint8_t*)fixups_head + fixups_head->symbols_offset
    );

    for (i = 0; i < starts_in_segment->page_count; ++i) {
        printf("%24s[%"PRIu16"]: 0x%.8"PRIx16"\n", "page_start", i,
            starts_in_segment->page_start[i]);
        size_t bind_off = mf->hOff
                          + starts_in_segment->segment_offset
                          + starts_in_segment->page_size * i
                          + starts_in_segment->page_start[i];

        printf("%17s %-11s %-6s %s\n", "", "address", "type", "target");
        size_t next_off = 0;
        do {
            bind_off += next_off;
            macho_ptr_64_bind_t *bind_info = (macho_ptr_64_bind_t*)
                    readFromFile(fd, &bind_off, sizeof(macho_ptr_64_bind_t));

            if (bind_info->bind == 1) {
                // !TODO: figure out purpose of bind_info->addend
                // library ordinals start from 1
                uint32_t dsym_indx = bind_info->ordinal;
                const char *sys_name = names + import[dsym_indx].name_offset;
                uint8_t lib_num = import[dsym_indx].lib_ordinal - 1;
                const char *lib_name = macho64GetDylibName(mf->dylibCom[lib_num]);
                printf("%17s 0x%.9"PRIx64" %-6s %s:%s\n", ""
                                               , mf->base_addr + bind_off
                                               , "bind"
                                               , lib_name
                                               , sys_name);
            } else {
                macho_ptr_64_rebase_t *rebase_info =
                    (macho_ptr_64_rebase_t*)bind_info;
                // !TODO: strange logic with high8
                uint64_t rebase_target = (uint64_t)(rebase_info->high8) << 56
                                       | rebase_info->target;
                printf("%17s 0x%.9"PRIx64" %-6s 0x%.9"PRIx64"\n", ""
                                               , mf->base_addr + bind_off
                                               , "rebase"
                                               , rebase_target);
            }

            next_off = bind_info->next * 4;
            Free(bind_info);
        } while (next_off);
    }
}

static void
macho64PrintStartsInSegment( const Macho64File *mf
                           , const macho_fixups_headers_t *fixups_head
                           , const macho_starts_in_segment_t *starts_in_segment
                           )
{
    UNUSED(mf);
    printf(" starts_in_segment:\n");
    printf("%20s: 0x%.8"PRIx32"\n", "size",
        starts_in_segment->size);
    printf("%20s: 0x%.8"PRIx16"\n", "page_size",
        starts_in_segment->page_size);
    printf("%20s: %s\n", "pointer_format",
        MACHO64_POINTER_FORMAT[starts_in_segment->pointer_format].str);
    printf("%20s: 0x%.8"PRIx64"\n", "segment_offset",
        starts_in_segment->segment_offset);
    printf("%20s: 0x%.8"PRIx32"\n", "max_valid_pointer",
        starts_in_segment->max_valid_pointer);
    printf("%20s: 0x%.8"PRIx16"\n", "page_count",
        starts_in_segment->page_count);

    switch (starts_in_segment->pointer_format) {
    case DYLD_CHAINED_PTR_ARM64E:
        break;
    case DYLD_CHAINED_PTR_64:
        break;
    case DYLD_CHAINED_PTR_32:
        break;
    case DYLD_CHAINED_PTR_32_CACHE:
        break;
    case DYLD_CHAINED_PTR_32_FIRMWARE:
        break;
    case DYLD_CHAINED_PTR_64_OFFSET:
        macho64PrintStartsInSegment64_offset(mf, fixups_head, starts_in_segment);
        break;
    case DYLD_CHAINED_PTR_ARM64E_OFFSET:
        break;
    case DYLD_CHAINED_PTR_64_KERNEL_CACHE:
        break;
    case DYLD_CHAINED_PTR_ARM64E_USERLAND:
        break;
    case DYLD_CHAINED_PTR_ARM64E_FIRMWARE:
        break;
    case DYLD_CHAINED_PTR_X86_64_KERNEL_CACHE:
        break;
    default:
        break;
    }
}

static void
macho64PrintChainedImports( const Macho64File *mf
                          , const macho_fixups_headers_t *fixups_head
                          )
{
    const macho_chained_import_t *import = (const macho_chained_import_t*)(
        (const uint8_t*)fixups_head + fixups_head->imports_offset
    );

    uint32_t i = 0;
    const char *names = (const char*)(
        (const uint8_t*)fixups_head + fixups_head->symbols_offset
    );
    for (i = 0; i < fixups_head->imports_count; ++i) {
        // library ordinals start from 1
        uint8_t lib_num = import[i].lib_ordinal - 1;
        printf("%.8u %9s: %s\n", i, "lib",
            macho64GetDylibName(mf->dylibCom[lib_num]));
        printf("%18s: %s\n", "symbol", names + import[i].name_offset);
        printf("%18s: %s\n", "weak_import", import[i].weak_import ? "true" : "false");
    }
}

static void
macho64PrintImports( const Macho64File *mf
                   , const macho_fixups_headers_t *fixups_head
                   )
{
    printf(" imports:\n");
    switch (fixups_head->imports_format) {
    case DYLD_CHAINED_IMPORT:
        macho64PrintChainedImports(mf, fixups_head);
        break;
    case DYLD_CHAINED_IMPORT_ADDEND:
        break;
    case DYLD_CHAINED_IMPORT_ADDEND64:
        break;
    default:
        break;
    }
}

void
macho64PrintFixups(const Macho64File *mf)
{
    printf("fixups:\n");
    const MachoLinkEditData *dylibChainedFixups = NULL;
    FOREACH_LOAD_COMMAND(mf,
        if (lcom->cmd == LC_DYLD_CHAINED_FIXUPS) {
            dylibChainedFixups = (const MachoLinkEditData*)lcom;
        }
    );

    if (dylibChainedFixups == NULL) {
        STDERROR_PRINT("There is no load command: LC_DYLD_CHAINED_FIXUPS\n");
        return;
    }

    FileD fd = mf->fd;
    size_t foff = mf->hOff + dylibChainedFixups->dataoff;
    size_t fsize = dylibChainedFixups->datasize;
    uint8_t *fixups = readFromFile(fd, &foff, fsize);
    uint8_t *p = fixups;
    if (fixups == NULL) {
        STDERROR_PRINT("Cannot read fixups from file\n");
        return;
    }

    macho_fixups_headers_t *fixups_head = (macho_fixups_headers_t*)p;
    p += fixups_head->starts_offset;
    macho64PrintFixupsHeader(fixups_head);

    macho_starts_in_image_t *starts_in_image = (macho_starts_in_image_t*)p;
    printf("starts_in_image:\n");
    printf("    seg_count %d\n", starts_in_image->seg_count);
    uint32_t i = 0;
    for (i = 0; i < starts_in_image->seg_count; ++i) {
        printf("    seg_info_offset[%"PRIu32"] 0x%.8"PRIx32"\n", i,
            starts_in_image->seg_info_offset[i]);
    }

    for (i = 0; i < starts_in_image->seg_count; ++i) {
        // index of element in seg_info_offset array equal to described segment
        // number
        if (starts_in_image->seg_info_offset[i]) {
            p = (uint8_t*)starts_in_image + starts_in_image->seg_info_offset[i];
            macho_starts_in_segment_t *starts_in_segment =
                (macho_starts_in_segment_t*)p;
            macho64PrintStartsInSegment(mf, fixups_head, starts_in_segment);
        }
    }

    macho64PrintImports(mf, fixups_head);

    Free(fixups);
    NEW_LINE;
}

