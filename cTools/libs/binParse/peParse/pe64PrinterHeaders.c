#include "comdef.h"
#include "pe64Printer.h"

#include <time.h>
#include <stdio.h>
#include <inttypes.h>

static void pe64PrintDosMagic(const DosHeader *dosHeader)
{
    if (dosHeader == NULL)
        return;

    const unsigned char *magic = (const unsigned char *)&dosHeader->e_magic;
    printf("%.2s", magic);
}

static void pePrintDosNtHeaderOff(const DosHeader *dosHeader)
{
    if (dosHeader == NULL)
        return;

    printf("%.8lx", dosHeader->e_lfanew);
}

void pe64PrintDosHeader(const PE64File *pe)
{
    if (pe == NULL)
        return;

    printf("DOS Header:\n");
    printf("Magic:\t\t\t");
    pe64PrintDosMagic(pe->dosHeader);
    NEW_LINE
    printf("NT Header Offset:\t");
    pePrintDosNtHeaderOff(pe->dosHeader);

    NEW_LINE;
    NEW_LINE;
}

static void pe64PrintNTMagic(const NTHeader64 *ntHeader)
{
    if (ntHeader == NULL)
        return;

    const char *sig = (const char*)&ntHeader->Signature;
    printf("%s", sig);
}

static void pe64PrintMachineId(const FileHeader *fileHeader)
{
    if (fileHeader == NULL)
        return;

    switch (fileHeader->Machine) {
    case IMAGE_FILE_MACHINE_UNKNOWN:
        printf("UNKNOWN");
        break;
    case IMAGE_FILE_MACHINE_AMD64:
        printf("AMD64");
        break;
    case IMAGE_FILE_MACHINE_ARM:
        printf("ARM");
        break;
    case IMAGE_FILE_MACHINE_ARM64:
        printf("ARM64");
        break;
    case IMAGE_FILE_MACHINE_ARMNT:
        printf("ARMNT");
        break;
    case IMAGE_FILE_MACHINE_I386:
        printf("I386");
        break;
    case IMAGE_FILE_MACHINE_IA64:
        printf("IA64");
        break;
    case IMAGE_FILE_MACHINE_R3000:
        printf("R3000");
        break;
    case IMAGE_FILE_MACHINE_R4000:
        printf("R4000");
        break;
    case IMAGE_FILE_MACHINE_R10000:
        printf("R10000");
        break;
    case IMAGE_FILE_MACHINE_WCEMIPSV2:
        printf("WCEMIPSV2");
        break;
    case IMAGE_FILE_MACHINE_ALPHA:
        printf("ALPHA");
        break;
    case IMAGE_FILE_MACHINE_SH3:
        printf("SH3");
        break;
    case IMAGE_FILE_MACHINE_SH3DSP:
        printf("SH3DSP");
        break;
    case IMAGE_FILE_MACHINE_SH3E:
        printf("SH3E");
        break;
    case IMAGE_FILE_MACHINE_SH4:
        printf("SH4");
        break;
    case IMAGE_FILE_MACHINE_SH5:
        printf("SH5");
        break;
    case IMAGE_FILE_MACHINE_THUMB:
        printf("THUMB");
        break;
    case IMAGE_FILE_MACHINE_AM33:
        printf("AM33");
        break;
    case IMAGE_FILE_MACHINE_POWERPC:
        printf("POWERPC");
        break;
    case IMAGE_FILE_MACHINE_POWERPCFP:
        printf("POWERPCFP");
        break;
    case IMAGE_FILE_MACHINE_MIPS16:
        printf("MIPS16");
        break;
    case IMAGE_FILE_MACHINE_ALPHA64:
        printf("ALPHA64");
        break;
    case IMAGE_FILE_MACHINE_MIPSFPU:
        printf("MIPSFPU");
        break;
    case IMAGE_FILE_MACHINE_MIPSFPU16:
        printf("MIPSFPU16");
        break;
    case IMAGE_FILE_MACHINE_TRICORE:
        printf("TRICORE");
        break;
    case IMAGE_FILE_MACHINE_CEF:
        printf("CEF");
        break;
    case IMAGE_FILE_MACHINE_EBC:
        printf("EBC");
        break;
    case IMAGE_FILE_MACHINE_M32R:
        printf("M32R");
        break;
    case IMAGE_FILE_MACHINE_CEE:
        printf("CEE");
        break;
    default:
        break;
    }
}

static void pe64PrintSectNum(const FileHeader *fileHeader)
{
    if (fileHeader == NULL)
        return;

    printf("%.2hu", fileHeader->NumberOfSections);
}

static void pe64PrintTimeStamp(const FileHeader *fileHeader)
{
    if (fileHeader == NULL)
        return;

    time_t time = fileHeader->TimeDateStamp;
    printf("%s", asctime(localtime(&time)));
}

static void pe64PrintSymTabPtr(const FileHeader *fileHeader)
{
    if (fileHeader == NULL)
        return;

    printf("%.8lx", fileHeader->PointerToSymbolTable);
}

static void pe64PrintSymNum(const FileHeader *fileHeader)
{
    if (fileHeader == NULL)
        return;

    printf("%.8lx", fileHeader->NumberOfSymbols);
}

static void pe64PrintOptHeaderSize(const FileHeader *fileHeader)
{
    if (fileHeader == NULL)
        return;

    printf("%.4hx", fileHeader->SizeOfOptionalHeader);
}

static void pe64PrintCharacteristics(const FileHeader *fileHeader)
{
    if (fileHeader == NULL)
        return;

    WORD type = fileHeader->Characteristics;
    printf("%.4hx\n", type);

    if (type) {
        TAB;
        TAB;
        TAB;
    }
    if (type & IMAGE_FILE_BYTES_REVERSED_HI)
        printf("REVERSED_HI ");
    if (type & IMAGE_FILE_UP_SYSTEM_ONLY)
        printf("SYS_ONLY ");
    if (type & IMAGE_FILE_DLL)
        printf("DLL ");
    if (type & IMAGE_FILE_SYSTEM)
        printf("SYS ");
    if (type & IMAGE_FILE_NET_RUN_FROM_SWAP)
        printf("NET_SWAP ");
    if (type & IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP)
        printf("REMOVABLE_SWAP ");
    if (type & IMAGE_FILE_DEBUG_STRIPPED)
        printf("DEBUG_STRIPPED ");
    if (type & IMAGE_FILE_32BIT_MACHINE)
        printf("32BIT ");
    if (type & IMAGE_FILE_BYTES_REVERSED_LO)
        printf("REVERSED_LO ");
    if (type & IMAGE_FILE_LARGE_ADDRESS_AWARE)
        printf("LADDR ");
    if (type & IMAGE_FILE_AGGRESIVE_WS_TRIM)
        printf("AGGRESIVE_WS_TRIM ");
    if (type & IMAGE_FILE_LOCAL_SYMS_STRIPPED)
        printf("SYMS_STRIPPED ");
    if (type & IMAGE_FILE_LINE_NUMS_STRIPPED)
        printf("LINE_STRIPPED ");
    if (type & IMAGE_FILE_EXECUTABLE_IMAGE)
        printf("EXE ");
    if (type & IMAGE_FILE_RELOCS_STRIPPED)
        printf("RELOCS_STRIPPED ");
}

void pe64PrintFileHeader(const PE64File *pe)
{
    if (pe == NULL)
        return;

    printf("File Header:\n");
    printf("Machine:\t\t");
    pe64PrintMachineId(pe->fileHeader);
    NEW_LINE;
    printf("Number of Sections:\t");
    pe64PrintSectNum(pe->fileHeader);
    NEW_LINE;
    printf("Time stamp:\t\t");
    pe64PrintTimeStamp(pe->fileHeader);
    printf("Symbol Table Pointer:\t");
    pe64PrintSymTabPtr(pe->fileHeader);
    NEW_LINE;
    printf("Number of Symbols:\t");
    pe64PrintSymNum(pe->fileHeader);
    NEW_LINE;
    printf("Size of Opt Header:\t");
    pe64PrintOptHeaderSize(pe->fileHeader);
    NEW_LINE;
    printf("Characteristics:\t");
    pe64PrintCharacteristics(pe->fileHeader);
    NEW_LINE;
}

static void pe64PrintOptHeaderMagic(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    switch (optHeader->Magic) {
    case IMAGE_NT_OPTIONAL_HDR32_MAGIC:
        printf("32-bit");
        break;
    case IMAGE_NT_OPTIONAL_HDR64_MAGIC:
        printf("64-bit");
        break;
    case IMAGE_ROM_OPTIONAL_HDR_MAGIC:
        printf("ROM");
        break;
    default:
        break;
    }
}

static void pe64PrintLinkerVersion(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.2hhu.%.2hhu", optHeader->MajorLinkerVersion, optHeader->MinorLinkerVersion);
}

static void pe64PrintCodeSize(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.8lx", optHeader->SizeOfCode);
}

static void pe64PrintInitializedDataSize(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.8lx", optHeader->SizeOfInitializedData);
}

static void pe64PrintUninitializedDataSize(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.8lx", optHeader->SizeOfUninitializedData);
}

static void pe64PrintEntryPoint(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.8lx", optHeader->AddressOfEntryPoint);
}

static void pe64PrintCodeBase(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.8lx", optHeader->BaseOfCode);
}

static void pe64PrintImageBase(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.16llx", optHeader->ImageBase);
}

static void pe64PrintSectionAlignment(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.8lx", optHeader->SectionAlignment);
}

static void pe64PrintFileAlignment(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.8lx", optHeader->FileAlignment);
}

static void pe64PrintOSVersion(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.2hu.%.2hu", optHeader->MajorOperatingSystemVersion, optHeader->MinorOperatingSystemVersion);
}

static void pe64PrintImageVersion(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.2hu.%.2hu", optHeader->MajorImageVersion, optHeader->MinorImageVersion);
}

static void pe64PrintSubsystemVersion(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.2hu.%.2hu", optHeader->MajorSubsystemVersion, optHeader->MinorSubsystemVersion);
}

static void pe64PrintWin32VersionValue(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.8lx", optHeader->Win32VersionValue);
}

static void pe64PrintImageSize(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.8lx", optHeader->SizeOfImage);
}

static void pe64PrintHeadersSize(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.8lx", optHeader->SizeOfHeaders);
}

static void pe64PrintCheckSum(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.8lx", optHeader->CheckSum);
}

static void pe64PrintSubsystem(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    WORD Subsystem = optHeader->Subsystem;
    printf("%.4hx: ", Subsystem);
    switch (Subsystem) {
    case IMAGE_SUBSYSTEM_UNKNOWN:
        printf("UNKNOWN");
        break;
    case IMAGE_SUBSYSTEM_NATIVE:
        printf("NATIVE");
        break;
    case IMAGE_SUBSYSTEM_WINDOWS_GUI:
        printf("WINDOWS_GUI");
        break;
    case IMAGE_SUBSYSTEM_WINDOWS_CUI:
        printf("WINDOWS_CUI");
        break;
    case IMAGE_SUBSYSTEM_OS2_CUI:
        printf("OS2_CUI");
        break;
    case IMAGE_SUBSYSTEM_POSIX_CUI:
        printf("POSIX_CUI");
        break;
    case IMAGE_SUBSYSTEM_NATIVE_WINDOWS:
        printf("NATIVE_WINDOWS");
        break;
    case IMAGE_SUBSYSTEM_WINDOWS_CE_GUI:
        printf("WINDOWS_CE_GUI");
        break;
    case IMAGE_SUBSYSTEM_EFI_APPLICATION:
        printf("EFI_APPLICATION");
        break;
    case IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER:
        printf("EFI_BOOT_SERVICE_DRIVER");
        break;
    case IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER:
        printf("EFI_RUNTIME_DRIVER");
        break;
    case IMAGE_SUBSYSTEM_EFI_ROM:
        printf("EFI_ROM");
        break;
    case IMAGE_SUBSYSTEM_XBOX:
        printf("XBOX");
        break;
    case IMAGE_SUBSYSTEM_WINDOWS_BOOT_APPLICATION:
        printf("WINDOWS_BOOT_APPLICATION");
        break;
    }
}

static void pe64PrintDllCharacteristics(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    WORD DllCharacteristics = optHeader->DllCharacteristics;
    printf("%.4hx\n", DllCharacteristics);

    if (DllCharacteristics) {
        TAB;
        TAB;
        TAB;
    }
    if (DllCharacteristics & IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA)
        printf("HIGH_ENTROPY_VA ");
    if (DllCharacteristics & IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE)
        printf("DYNAMIC_BASE ");
    if (DllCharacteristics & IMAGE_DLLCHARACTERISTICS_FORCE_INTEGRITY)
        printf("FORCE_INTEGRITY ");
    if (DllCharacteristics & IMAGE_DLLCHARACTERISTICS_NX_COMPAT)
        printf("NX_COMPAT ");
    if (DllCharacteristics & IMAGE_DLLCHARACTERISTICS_NO_ISOLATION)
        printf("NO_ISOLATION ");
    if (DllCharacteristics & IMAGE_DLLCHARACTERISTICS_NO_SEH)
        printf("NO_SEH ");
    if (DllCharacteristics & IMAGE_DLLCHARACTERISTICS_NO_BIND)
        printf("NO_BIND ");
    if (DllCharacteristics & IMAGE_DLLCHARACTERISTICS_APPCONTAINER)
        printf("APPCONTAINER ");
    if (DllCharacteristics & IMAGE_DLLCHARACTERISTICS_WDM_DRIVER)
        printf("WDM_DRIVER ");
    if (DllCharacteristics & IMAGE_DLLCHARACTERISTICS_GUARD_CF)
        printf("GUARD_CF ");
    if (DllCharacteristics & IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE)
        printf("TERMINAL_SERVER_AWARE ");
}

static void pe64PrintStackSizeReserve(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.16llx", optHeader->SizeOfStackReserve);
}

static void pe64PrintStackSizeCommit(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.16llx", optHeader->SizeOfStackCommit);
}

static void pe64PrintHeapSizeReserve(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.16llx", optHeader->SizeOfHeapReserve);
}

static void pe64PrintHeapSizeCommit(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.16llx", optHeader->SizeOfHeapCommit);
}

static void pe64PrintLoaderFlags(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.8lx", optHeader->LoaderFlags);
}

static void pe64PrintNumOfRvaAndSizes(const OptHeader64 *optHeader)
{
    if (optHeader == NULL)
        return;

    printf("%.8lx", optHeader->NumberOfRvaAndSizes);
}

void pe64PrintDataDir(const DataDir *dataDir)
{
    if (dataDir == NULL)
        return;

    printf("%.8lx %.8lx", dataDir->VirtualAddress, dataDir->Size);
}

void pe64PrintOptHeader(const PE64File *pe)
{
    if (pe == NULL)
        return;

    printf("Optional Header:\n");
    printf("Application Type:\t");
    pe64PrintOptHeaderMagic(pe->optHeader);
    NEW_LINE;
    printf("Linker Version:\t\t");
    pe64PrintLinkerVersion(pe->optHeader);
    NEW_LINE;
    printf("Code Size:\t\t");
    pe64PrintCodeSize(pe->optHeader);
    NEW_LINE;
    printf("Initialized Data Size:\t");
    pe64PrintInitializedDataSize(pe->optHeader);
    NEW_LINE;
    printf("Uninitialized Data Size:");
    pe64PrintUninitializedDataSize(pe->optHeader);
    NEW_LINE;
    printf("Entry Point:\t\t");
    pe64PrintEntryPoint(pe->optHeader);
    NEW_LINE;
    printf("Code Base:\t\t");
    pe64PrintCodeBase(pe->optHeader);
    NEW_LINE;
    printf("Image Base:\t\t");
    pe64PrintImageBase(pe->optHeader);
    NEW_LINE;
    printf("Section Alignment:\t");
    pe64PrintSectionAlignment(pe->optHeader);
    NEW_LINE;
    printf("File Alignment:\t\t");
    pe64PrintFileAlignment(pe->optHeader);
    NEW_LINE;
    printf("OS Version:\t\t");
    pe64PrintOSVersion(pe->optHeader);
    NEW_LINE;
    printf("Image Version:\t\t");
    pe64PrintImageVersion(pe->optHeader);
    NEW_LINE;
    printf("Subsystem Version:\t");
    pe64PrintSubsystemVersion(pe->optHeader);
    NEW_LINE;
    printf("Win32 Version Val:\t");
    pe64PrintWin32VersionValue(pe->optHeader);
    NEW_LINE;
    printf("Image Size:\t\t");
    pe64PrintImageSize(pe->optHeader);
    NEW_LINE;
    printf("Headers Size:\t\t");
    pe64PrintHeadersSize(pe->optHeader);
    NEW_LINE;
    printf("CheckSum:\t\t");
    pe64PrintCheckSum(pe->optHeader);
    NEW_LINE;
    printf("Subsystem:\t\t");
    pe64PrintSubsystem(pe->optHeader);
    NEW_LINE;
    printf("DllCharacteristics:\t");
    pe64PrintDllCharacteristics(pe->optHeader);
    NEW_LINE;
    printf("Stack Size Reserve:\t");
    pe64PrintStackSizeReserve(pe->optHeader);
    NEW_LINE;
    printf("Stack Size Commit:\t");
    pe64PrintStackSizeCommit(pe->optHeader);
    NEW_LINE;
    printf("Heap Size Reserve:\t");
    pe64PrintHeapSizeReserve(pe->optHeader);
    NEW_LINE;
    printf("Heap Size Commit:\t");
    pe64PrintHeapSizeCommit(pe->optHeader);
    NEW_LINE;
    printf("Loader Flags:\t\t");
    pe64PrintLoaderFlags(pe->optHeader);
    NEW_LINE;
    printf("NumberOfRvaAndSizes:\t");
    pe64PrintNumOfRvaAndSizes(pe->optHeader);
    NEW_LINE;

    printf("Data Directory:\t\tType\t\tAddr\t Size\n");
    printf("\t\t\t--------------- -------- --------\n");
    DWORD i = 0;
    DWORD NumberOfRvaAndSizes = pe->optHeader->NumberOfRvaAndSizes;
    for (i = 0; i < NumberOfRvaAndSizes; ++i) {
        switch (i) {
        case IMAGE_DIRECTORY_ENTRY_EXPORT:
            printf("\t\t\tEXPORT:\t\t");
            break;
        case IMAGE_DIRECTORY_ENTRY_IMPORT:
            printf("\t\t\tIMPORT:\t\t");
            break;
        case IMAGE_DIRECTORY_ENTRY_RESOURCE:
            printf("\t\t\tRESOURCE:\t");
            break;
        case IMAGE_DIRECTORY_ENTRY_EXCEPTION:
            printf("\t\t\tEXCEPTION:\t");
            break;
        case IMAGE_DIRECTORY_ENTRY_SECURITY:
            printf("\t\t\tSECURITY:\t");
            break;
        case IMAGE_DIRECTORY_ENTRY_BASERELOC:
            printf("\t\t\tBASERELOC:\t");
            break;
        case IMAGE_DIRECTORY_ENTRY_DEBUG:
            printf("\t\t\tDEBUG:\t\t");
            break;
        case IMAGE_DIRECTORY_ENTRY_ARCHITECTURE:
            printf("\t\t\tARCHITECTURE:\t");
            break;
        case IMAGE_DIRECTORY_ENTRY_GLOBALPTR:
            printf("\t\t\tGLOBALPTR:\t");
            break;
        case IMAGE_DIRECTORY_ENTRY_TLS:
            printf("\t\t\tTLS:\t\t");
            break;
        case IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG:
            printf("\t\t\tLOAD_CONFIG:\t");
            break;
        case IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT:
            printf("\t\t\tBOUND_IMPORT:\t");
            break;
        case IMAGE_DIRECTORY_ENTRY_IAT:
            printf("\t\t\tIAT:\t\t");
            break;
        case IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT:
            printf("\t\t\tDELAY_IMPORT:\t");
            break;
        case IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR:
            printf("\t\t\tCOM_DESCRIPTOR:\t");
            break;
        default:
            printf("\t\t\tRESERVED:\t");
            break;
        }
        pe64PrintDataDir(pe->optHeader->DataDirectory + i);
        NEW_LINE;
    }
}

void pe64PrintNtHeader(const PE64File *pe)
{
    if (pe == NULL)
        return;

    printf("NT Header:\n");
    printf("Magic:\t\t\t");
    pe64PrintNTMagic(pe->ntHeader);
    NEW_LINE;
    pe64PrintFileHeader(pe);
    pe64PrintOptHeader(pe);

    NEW_LINE;
}

