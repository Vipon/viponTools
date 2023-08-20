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
#include "elf64Printer.h"

#include <inttypes.h>

static
void elf64PrintHeaderMagic(const Elf64Ehdr *h)
{
    printMem(h->e_ident, EI_NIDENT);
}

static const
char* elf64GetMachineClassStr(uint8_t class)
{
    switch (class) {
    case ELFCLASS32:
        return "ELF32";
    case ELFCLASS64:
        return "ELF64";
    default:
        return "UNKNOWN";
    }
}

static
void elf64PrintHeaderMachineClass(const Elf64Ehdr *h)
{
    printf("%s", elf64GetMachineClassStr(h->e_ident[EI_CLASS]));
}

static const
char* elf64GetDataFormatStr(uint8_t format)
{
    switch (format) {
    case ELFDATA2LSB:
        return "2's complement, little endian";
    case ELFDATA2MSB:
        return "2's complement, big endian";
    default:
        return "UNKNOWN";
    }
}

static
void elf64PrintHeaderDataFormat(const Elf64Ehdr *h)
{
    printf("%s", elf64GetDataFormatStr(h->e_ident[EI_DATA]));
}

static const
char* elf64GetVersionStr(uint8_t version)
{
    switch (version) {
    case EV_CURRENT:
        return "1 (current)";
    default:
        return "UNKNOWN";
    }
}

static
void elf64PrintHeaderVersion(const Elf64Ehdr *h)
{
    printf("%s", elf64GetVersionStr(h->e_ident[EI_VERSION]));
}

static const
char* elf64GetOSABIStr(uint8_t version)
{
    switch (version) {
    case ELFOSABI_SYSV:
        return "UNIX - System V";
    case ELFOSABI_HPUX:
        return "HP-UX operating system";
    case ELFOSABI_NETBSD:
        return "NetBSD";
    case ELFOSABI_LINUX:
        return "GNU/Linux";
    case ELFOSABI_HURD:
        return "GNU/Hurd";
    case ELFOSABI_86OPEN:
        return "86Open common IA32 ABI";
    case ELFOSABI_SOLARIS:
        return "Solaris";
    case ELFOSABI_AIX:
        return "AIX";
    case ELFOSABI_IRIX:
        return "IRIX";
    case ELFOSABI_FREEBSD:
        return "FreeBSD";
    case ELFOSABI_TRU64:
        return "TRU64 UNIX";
    case ELFOSABI_MODESTO:
        return "Novell Modesto";
    case ELFOSABI_OPENBSD:
        return "OpenBSD";
    case ELFOSABI_OPENVMS:
        return "Open VMS";
    case ELFOSABI_NSK:
        return "HP Non-Stop Kernel";
    case ELFOSABI_AROS:
        return "Amiga Research OS";
    case ELFOSABI_FENIXOS:
        return "FenixOS";
    case ELFOSABI_CLOUDABI:
        return "Nuxi CloudABI";
    case ELFOSABI_OPENVOS:
        return "Stratus Technologies OpenVOS";
    case ELFOSABI_ARM_AEABI:
        return "ARM EABI";
    case ELFOSABI_ARM:
        return "ARM";
    case ELFOSABI_STANDALONE:
        return "Standalone (embedded) application";
    default:
        return "UNKNOWN";
    }
}

static
void elf64PrintHeaderOSABI(const Elf64Ehdr *h)
{
    printf("%s", elf64GetOSABIStr(h->e_ident[EI_OSABI]));
}

static
void elf64PrintHeaderABIVersion(const Elf64Ehdr *h)
{
    printf("%d", h->e_ident[EI_ABIVERSION]);
}

static const
char *elf64GetFileTypeStr(uint16_t type)
{
    switch (type) {
    case ET_NONE:
        return "NONE";
    case ET_REL:
        return "REL";
    case ET_EXEC:
        return "EXEC (Executable file)";
    case ET_DYN:
        return "DYN";
    case ET_CORE:
        return "CORE";
    case ET_LOOS:
        return "LOOS";
    case ET_HIOS:
        return "HIOS";
    case ET_LOPROC:
        return "LOPROC";
    case ET_HIPROC:
        return "HIPROC";
    default:
        return "UNKNOWN";
    }
}

static
void elf64PrintHeaderFileType(const Elf64Ehdr *h)
{
    printf("%s", elf64GetFileTypeStr(h->e_type));
}

static const
char *elf64GetMachineTypeStr(uint16_t type)
{
    switch (type) {
    case EM_NONE:
        return "Unknown machine.";
    case EM_M32:
        return "AT&T WE32100.";
    case EM_SPARC:
        return "Sun SPARC.";
    case EM_386:
        return "Intel i386.";
    case EM_68K:
        return "Motorola 68000.";
    case EM_88K:
        return "Motorola 88000.";
    case EM_IAMCU:
        return "Intel MCU.";
    case EM_860:
        return "Intel i860.";
    case EM_MIPS:
        return "MIPS R3000 Big-Endian only.";
    case EM_S370:
        return "IBM System/370.";
    case EM_MIPS_RS3_LE:
        return "MIPS R3000 Little-Endian.";
    case EM_PARISC:
        return "HP PA-RISC.";
    case EM_VPP500:
        return "Fujitsu VPP500.";
    case EM_SPARC32PLUS:
        return "SPARC v8plus.";
    case EM_960:
        return "Intel 80960.";
    case EM_PPC:
        return "PowerPC 32-bit.";
    case EM_PPC64:
        return "PowerPC 64-bit.";
    case EM_S390:
        return "IBM System/390.";
    case EM_V800:
        return "NEC V800.";
    case EM_FR20:
        return "Fujitsu FR20.";
    case EM_RH32:
        return "TRW RH-32.";
    case EM_RCE:
        return "Motorola RCE.";
    case EM_ARM:
        return "ARM.";
    case EM_SH:
        return "Hitachi SH.";
    case EM_SPARCV9:
        return "SPARC v9 64-bit.";
    case EM_TRICORE:
        return "Siemens TriCore embedded processor.";
    case EM_ARC:
        return "Argonaut RISC Core.";
    case EM_H8_300:
        return "Hitachi H8/300.";
    case EM_H8_300H:
        return "Hitachi H8/300H.";
    case EM_H8S:
        return "Hitachi H8S.";
    case EM_H8_500:
        return "Hitachi H8/500.";
    case EM_IA_64:
        return "Intel IA-64 Processor.";
    case EM_MIPS_X:
        return "Stanford MIPS-X.";
    case EM_COLDFIRE:
        return "Motorola ColdFire.";
    case EM_68HC12:
        return "Motorola M68HC12.";
    case EM_MMA:
        return "Fujitsu MMA.";
    case EM_PCP:
        return "Siemens PCP.";
    case EM_NCPU:
        return "Sony nCPU.";
    case EM_NDR1:
        return "Denso NDR1 microprocessor.";
    case EM_STARCORE:
        return "Motorola Star*Core processor.";
    case EM_ME16:
        return "Toyota ME16 processor.";
    case EM_ST100:
        return "STMicroelectronics ST100 processor.";
    case EM_TINYJ:
        return "Advanced Logic Corp. TinyJ processor.";
    case EM_X86_64:
        return "Advanced Micro Devices X86-64";
    case EM_PDSP:
        return "Sony DSP Processor.";
    case EM_FX66:
        return "Siemens FX66 microcontroller.";
    case EM_ST9PLUS:
        return "STMicroelectronics ST9+ 8/16 microcontroller.";
    case EM_ST7:
        return "STmicroelectronics ST7 8-bit microcontroller.";
    case EM_68HC16:
        return "Motorola MC68HC16 microcontroller.";
    case EM_68HC11:
        return "Motorola MC68HC11 microcontroller.";
    case EM_68HC08:
        return "Motorola MC68HC08 microcontroller.";
    case EM_68HC05:
        return "Motorola MC68HC05 microcontroller.";
    case EM_SVX:
        return "Silicon Graphics SVx.";
    case EM_ST19:
        return "STMicroelectronics ST19 8-bit mc.";
    case EM_VAX:
        return "Digital VAX.";
    case EM_CRIS:
        return "Axis Communications 32-bit embedded processor.";
    case EM_JAVELIN:
        return "Infineon Technologies 32-bit embedded processor.";
    case EM_FIREPATH:
        return "Element 14 64-bit DSP Processor.";
    case EM_ZSP:
        return "LSI Logic 16-bit DSP Processor.";
    case EM_MMIX:
        return "Donald Knuth's educational 64-bit proc.";
    case EM_HUANY:
        return "Harvard University machine-independent object files.";
    case EM_PRISM:
        return "SiTera Prism.";
    case EM_AVR:
        return "Atmel AVR 8-bit microcontroller.";
    case EM_FR30:
        return "Fujitsu FR30.";
    case EM_D10V:
        return "Mitsubishi D10V.";
    case EM_D30V:
        return "Mitsubishi D30V.";
    case EM_V850:
        return "NEC v850.";
    case EM_M32R:
        return "Mitsubishi M32R.";
    case EM_MN10300:
        return "Matsushita MN10300.";
    case EM_MN10200:
        return "Matsushita MN10200.";
    case EM_PJ:
        return "picoJava.";
    case EM_OPENRISC:
        return "OpenRISC 32-bit embedded processor.";
    case EM_ARC_A5:
        return "ARC Cores Tangent-A5.";
    case EM_XTENSA:
        return "Tensilica Xtensa Architecture.";
    case EM_VIDEOCORE:
        return "Alphamosaic VideoCore processor.";
    case EM_TMM_GPP:
        return "Thompson Multimedia General Purpose Processor.";
    case EM_NS32K:
        return "National Semiconductor 32000 series.";
    case EM_TPC:
        return "Tenor Network TPC processor.";
    case EM_SNP1K:
        return "Trebia SNP 1000 processor.";
    case EM_ST200:
        return "STMicroelectronics ST200 microcontroller.";
    case EM_IP2K:
        return "Ubicom IP2xxx microcontroller family.";
    case EM_MAX:
        return "MAX Processor.";
    case EM_CR:
        return "National Semiconductor CompactRISC microprocessor.";
    case EM_F2MC16:
        return "Fujitsu F2MC16.";
    case EM_MSP430:
        return "Texas Instruments embedded microcontroller msp430.";
    case EM_BLACKFIN:
        return "Analog Devices Blackfin (DSP) processor.";
    case EM_SE_C33:
        return "S1C33 Family of Seiko Epson processors.";
    case EM_SEP:
        return "Sharp embedded microprocessor.";
    case EM_ARCA:
        return "Arca RISC Microprocessor.";
    case EM_UNICORE:
        return "Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University";
    case EM_AARCH64:
        return "AArch64 (64-bit ARM)";
    case EM_RISCV:
        return "RISC-V";
    case EM_ALPHA_STD:
        return "Digital Alpha (standard value).";
    case EM_ALPHA:
        return "Alpha (written in the absence of an ABI)";
    default:
        return "UNKNOWN";
    }
}

static
void elf64PrintHeaderMachineType(const Elf64Ehdr *h)
{
    printf("%s", elf64GetMachineTypeStr(h->e_machine));
}

void elf64PrintHeader(const Elf64File *ef)
{
    Elf64Ehdr *h = ef->header;
    printf("ELF Header:\n");
    printf("  %s:   ", "Magic");
    elf64PrintHeaderMagic(h);
    NEW_LINE;

    printf("  %s:%29s", "Class", "");
    elf64PrintHeaderMachineClass(h);
    NEW_LINE;

    printf("  %s:%30s", "Data", "");
    elf64PrintHeaderDataFormat(h);
    NEW_LINE;

    printf("  %s:%27s", "Version", "");
    elf64PrintHeaderVersion(h);
    NEW_LINE;

    printf("  %s:%28s", "OS/ABI", "");
    elf64PrintHeaderOSABI(h);
    NEW_LINE;

    printf("  %s:%23s", "ABI Version", "");
    elf64PrintHeaderABIVersion(h);
    NEW_LINE;

    printf("  %s:%30s", "Type", "");
    elf64PrintHeaderFileType(h);
    NEW_LINE;

    printf("  %s:%27s", "Machine", "");
    elf64PrintHeaderMachineType(h);
    NEW_LINE;

    printf("  %s:%27s0x%"PRIx32, "Version", "", h->e_version);
    NEW_LINE;

    printf("  %s:%15s0x%"PRIx64, "Entry point address", "", h->e_entry);
    NEW_LINE;

    printf("  %s:%10s%"PRIu64" %s", "Start of program headers", "", h->e_phoff, "(bytes into file)");
    NEW_LINE;

    printf("  %s:%10s%"PRIu64" %s", "Start of section headers", "", h->e_shoff, "(bytes into file)");
    NEW_LINE;

    printf("  %s:%29s0x%"PRIx32, "Flags", "", h->e_flags);
    NEW_LINE;

    printf("  %s:%15s%"PRIu32" %s", "Size of this header", "", h->e_ehsize, "(bytes)");
    NEW_LINE;

    printf("  %s:%11s%"PRIu32" %s", "Size of program headers", "", h->e_phentsize, "(bytes)");
    NEW_LINE;

    printf("  %s:%9s%"PRIu32, "Number of program headers", "", h->e_phnum);
    NEW_LINE;

    printf("  %s:%11s%"PRIu32" %s", "Size of section headers", "", h->e_shentsize, "(bytes)");
    NEW_LINE;

    printf("  %s:%9s%"PRIu32, "Number of section headers", "", h->e_shnum);
    NEW_LINE;

    printf("  %s:%1s%"PRIu32, "Section header string table index", "", h->e_shstrndx);
    NEW_LINE;
    NEW_LINE;
}

