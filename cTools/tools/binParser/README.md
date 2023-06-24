# binParser
Crossplatform binary parser. Doesn't matter OS you prefer to use or binary type you need to parse.

## Support binary formats and OS
The followed table shows support platforms and binary formats. macho files format also includes fat binaries.

|       | MacOsX | Win | Linux |
|-------|:------:|:---:|:-----:|
|macho64|   X    |  X  |   X   |
|elf64  |        |     |       |
|PE64   |        |  X  |       |

## Command Line Arguments
| Short Arg | Long Arg      | Description |
|----------:|:--------------|:------------|
|        -h |   --header    | print all headers |
|        -s |   --symbols   | print all symbols |
|        -S |  --sections   | print all section |
|           |  --segments   | print all segments |
|           | --func-starts | macho: print info about function starts |
|        -l |    --lcom     | macho: print load commands |
|           | --fat-header  | macho: print fat header information if it's |
|        -m |    --mcpu     | set up cpu type for parser, used for fat binaries |

## Examples
### Print aarch64 symbols of fat macho64
```
binParser --mcpu=aarch64 -s [PATH_TO_FAT_MACHO64]
```

