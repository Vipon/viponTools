# binParser
Crossplatform binary parser. Doesn't matter OS you prefer to use or binary type you need to parse.

## Support binary formats and OS
The followed table shows support platforms and binary formats. macho files format also includes fat binaries.

|          | MacOsX | Win | Linux |
|----------|:------:|:---:|:-----:|
|macho64   |   X    |  X  |   X   |
|fatMacho64|   X    |  X  |   X   |
|elf64     |   X    |  X  |   X   |
|PE64      |   X    |  X  |   X   |

## Command Line Arguments
| Short Arg | Long Arg        | Description |
|----------:|:----------------|:------------|
|        -h |     --header    | print all headers |
|        -s |    --symbols    | print all symbols |
|        -S |    --sections   | print all section |
|           |    --segments   | print all segments |
|           |  --func-starts  | macho: print info about function starts |
|        -l |     --lcom      | macho: print load commands |
|           |   --code-sign   | macho: print code sign |
|           |   --fat-header  | macho: print fat header information if it's |
|        -m |     --mcpu      | set up cpu type for parser, used for fat binaries |
|           |  --dos-header   | pe: print dos header |
|        -d | --delay-imports | pe: print delay imports |
|        -e |    --exports    | pe: print exports |
|           |  --file-header  | pe: print file header |
|           |  --opt-header   | pe: print opt header |
|           |    --dynamic    | elf: print .dynamic section |
|        -r |    --relocs     | elf: print relocations |
|           |  --version-info | elf: print symbols version info from sections:.gnu.version, .gnu.version_r |

## Examples
### Print aarch64 symbols of fat macho64
```
binParser --mcpu=aarch64 -s [PATH_TO_FAT_MACHO64]
```

