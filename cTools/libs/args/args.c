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

#include "mem.h"
#include "args.h"
#include "string.h"
#include "comdef.h"

static size_t NUM_OPTS = 0;
static size_t OPTS_LEN = 0;
static struct argp_option* opts = NULL;
static ARG_HAND *hands = NULL;
static struct argp argp = {0};

int addArg(ARG_HAND hand, const struct argp_option* opt)
{
    if (NUM_OPTS == OPTS_LEN) {
        struct argp_option* new_opts = Calloc(NUM_OPTS + 10, sizeof(struct argp_option));
        ARG_HAND *new_hands = Calloc(NUM_OPTS + 10, sizeof(ARG_HAND));
        if (new_opts == NULL || new_hands == NULL) {
            return -1;
        }

        memcpy(new_opts, opts, NUM_OPTS * sizeof(struct argp_option));
        memcpy(new_hands, hands, NUM_OPTS * sizeof(ARG_HAND));
        Free(opts);
        Free(hands);
        opts = new_opts;
        argp.options = opts;
        hands = new_hands;
        OPTS_LEN += 10;
    }

    memcpy(opts + NUM_OPTS, opt, sizeof(struct argp_option));
    hands[NUM_OPTS] = hand;
    ++NUM_OPTS;

    return 0;
}

void addDoc(const char* doc)
{
    // Program documentation.
    argp.doc = doc;
}

void addArgsDoc(const char* argsDoc)
{
    // A description of the arguments we accept.
    argp.args_doc = argsDoc;
}

static ARG_HAND getHandle(int key)
{
    size_t i = 0;
    for (i = 0; i < NUM_OPTS; ++i) {
        if (opts[i].key == key) {
            return hands[i];
        }
    }

    return NULL;
}

static error_t
parseOpt(int key, char *arg, struct argp_state *state)
{
    UNUSED(state);
    if (key == ARGP_KEY_END || key == ARGP_KEY_NO_ARGS) {
        return 0;
    }

    ARG_HAND hand = getHandle(key);
    if (hand == NULL) {
        return ARGP_ERR_UNKNOWN;
    }

    hand(arg);
    return 0;
}

error_t argParse(int argc, char** argv)
{
    // Add last elem argp_option
    ADD_ARG(NULL, 0, 0, 0, 0, 0);
    argp.parser = parseOpt;
    return argp_parse( &argp
                     , argc
                     , argv
                     //unsigned flags
                     , 0
                     //int *arg_index
                     , 0
                     // void* input
                     , NULL
                     );

    return 0;
}

