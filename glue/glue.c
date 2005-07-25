/*
Haskell CDK Interface
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This code is under a 3-clause BSD license; see COPYING for details.
*/

#include <cdk/cdk.h>

CDKSCREEN *initialize_cdk(void) {
    WINDOW *cursesWin = 0;
    cursesWin = initscr();
    return initCDKScreen(cursesWin);
}

