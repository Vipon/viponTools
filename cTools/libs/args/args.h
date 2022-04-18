/***
 * MIT License
 *
 * Copyright (c) 2022 Konychev Valerii
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

#ifndef _ARGS_H
#define _ARGS_H

#include <argp.h>

typedef void(*ARG_HAND)(char*);

/***
 * hand - pointer to handler for argument
 * Name - full argument name (--Name)
 * Key - short argument name (-Key)
 * Arg - extra hint for help. If Arg="VALUE"
 *  help will print --Name=VALUE
 * Flags - argp option flags. See
 *  https://www.gnu.org/software/libc/manual/html_node/Argp-Option-Flags.html
 * Doc - description of argument
 */
#define ADD_ARG(hand, Name, Key, Arg, Flags, Doc) { \
    struct argp_option loc_opt = {                  \
        .name  = Name,                              \
        .key   = Key,                               \
        .arg   = Arg,                               \
        .flags = Flags,                             \
        .doc   = Doc,                               \
    };                                              \
    addArg(hand, &loc_opt);                         \
}
int addArg(ARG_HAND hand, const struct argp_option* opt);

#define ADD_DOC(doc) addDoc(doc);
void addDoc(const char* doc);

#define ADD_ARGS_DOC(argsDoc) addArgsDoc(argsDoc);
void addArgsDoc(const char* argsDoc);

#define ARG_PARSE(argc, argv) argParse(argc, argv);
error_t argParse(int argc, char** argv);

#endif /* _ARGS_H */

