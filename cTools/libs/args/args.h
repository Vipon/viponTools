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

typedef void(*ARG_HAND)(const char*);

typedef struct Arg {
    // full argument name (--Name)
    const char  *name;

    // short argument name (-Key)
    int         key;
    // extra hint for help. If Arg="VALUE"
    // help will print --Name=VALUE (type of arg)
    const char  *arg;

    // argp option flags. See
    // https://www.gnu.org/software/libc/manual/html_node/Argp-Option-Flags.html
    int         flags;

    // description of argument
    const char  *doc;

    int         group;

    // handler for argument
    ARG_HAND    hand;
} Arg;

#define ADD_ARG(h, ...) { \
    Arg loc_arg = {       \
        .hand = h,        \
        __VA_ARGS__       \
    };                    \
    addArg(&loc_arg);     \
}
int addArg(const Arg* arg);

#define ADD_DOC(doc) addDoc(doc);
void addDoc(const char* doc);

#define ADD_ARGS_DOC(argsDoc) addArgsDoc(argsDoc);
void addArgsDoc(const char* argsDoc);

#define SET_NUM_ARGS(num) setNumArgs(num);
void setNumArgs(unsigned num);

typedef void(*ARGS_HAND)(const char*, unsigned num);
#define SET_ARGS_HAND(hand) setArgsHand(hand);
void setArgsHand(ARGS_HAND hand);

#define ADD_VERSION(version) addVersion(version);
void addVersion(const char* version);

#define ARG_PARSE(argc, argv) argParse(argc, argv);
error_t argParse(int argc, char** argv);

#endif /* _ARGS_H */

