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
|        -S |    --sections   | print all section |
|           |   --code-sign   | macho: print code sign |
|        -d | --delay-imports | pe: print delay imports |
|           |  --dos-header   | pe: print dos header |
|           |    --dynamic    | elf: print .dynamic section |
|        -e |    --exports    | pe: print exports |
|           |   --fat-header  | macho: print fat header information if it's |
|           |  --file-header  | pe: print file header |
|           |    --fixups     | macho: print all fixups information |
|           |  --func-starts  | macho: print info about function starts |
|        -h |     --header    | print all headers |
|        -i |    --imports    | pe: print imports |
|        -l |     --lcom      | macho: print load commands |
|        -m |     --mcpu      | set up cpu type for parser, used for fat binaries |
|           |  --opt-header   | pe: print opt header |
|        -r |    --relocs     | print relocations |
|           |    --segments   | print all segments |
|        -s |    --symbols    | print all symbols |
|           |  --version-info | elf: print symbols version info from sections:.gnu.version, .gnu.version_r |

## Examples
### Print aarch64 symbols of fat macho64
```
binParser --mcpu=aarch64 -s [PATH_TO_FAT_MACHO64]
```

