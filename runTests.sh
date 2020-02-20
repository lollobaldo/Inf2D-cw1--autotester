#!/bin/sh

GREEN=`echo -e '\033[92m'`
DARK_GREEN=`echo -e '\033[1;32m'`
RED=`echo -e '\033[1;31m'`
CYAN=`echo -e '\033[96m'`
BLUE=`echo -e '\033[94m'`
YELLOW=`echo -e '\033[93m'`
PURPLE=`echo -e '\033[95m'`
RESET=`echo -e '\033[0m'`

test_passed="s/✔\|√/${DARK_GREEN}&${RESET}/g;"
test_failed="s/✗\|X/${RED}&${RESET}/g;"
# test_finished="s/Breath-first/$red&$reset/g;"

exec runhaskell -i../:./ Tester.hs $(whoami) $1 |\
    sed "$test_passed\
         $test_failed\
         $test_sumup"