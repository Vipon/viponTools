# Library args
## Description
Declarative type wrapper for argp.h

## Getting started
### Minimal parser
Minimal parser equal to standard minimal parser argp.h lib. The minimal parser
works with _--help_ and _--usage_ arguments.
```
#include "args.h"

int main(int argc, char **argv)
{
    ARG_PARSE(argc, argv);

    return 0;
}
```

### Add new argument
For adding new agument just use: ADD_ARG.
```
/***
 * hand - pointer to handler for argument
 * Name - full argument name (--Name)
 * Key - short argument name (-Key)
 * Arg - extra hint for help. If Arg="VALUE"
 *  help will print --Name=VALUE
 * Flags - argp option flags. See more:
 *  https://www.gnu.org/software/libc/manual/html_node/Argp-Option-Flags.html
 * Doc - description of argument
 */
ADD_ARG(hand, Name, Key, Arg, Flags, Doc);
```

_Name, Key, Arg, Flags, Doc_ are the same as [struct argp\_option](https://www.gnu.org/software/libc/manual/html_node/Argp-Option-Vectors.html).

### Add program description for --help
For adding extra description just use: ADD_DOC.
```
static char doc[] = "Extra program description";
ADD_DOC(doc);
```

### Add arguments description for --usage
For adding extra description just use: ADD_ARGS_DOC.
```
static char argsDoc[] = "ARG0 ARG1";
ADD_ARGS_DOC(argsDoc);
```

### Full example
```
#include "args.h"
#include <stdbool.h>

static char doc[] = "Test args library";
static char argsDoc[] = "ARG0 ARG1";

bool isVerbose = false;
static void argVerbose(char *arg)
{
    isVerbose = true;
}

int main(int argc, char **argv)
{
    ADD_DOC(doc);
    ADD_ARGS_DOC(argsDoc);
    ADD_ARG(argVerbose, "verbose", 'v', 0 , 0, "Produce verbose output");
    ARG_PARSE(argc, argv);

    return 0;
}

/***
 * Result for --help
 * Usage: argsTest [OPTION...] ARG0 ARG1
 * Test args library
 *
 * -v, --verbose              Produce verbose output
 * -?, --help                 Give this help list
 *     --usage                Give a short usage message
 */
```
