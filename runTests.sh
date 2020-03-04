#!/bin/sh

GREEN=`echo -e "\033[92m"`
DARK_GREEN=`echo -e "\033[1;32m"`
RED=`echo -e "\033[1;31m"`
CYAN=`echo -e "\033[96m"`
BLUE=`echo -e "\033[94m"`
YELLOW=`echo -e "\033[1;33m"`
PURPLE=`echo -e "\033[95m"`
RESET=`echo -e "\033[0m"`

L_GREEN=`echo "\033[92m"`
L_DARK_GREEN=`echo "\033[1;32m"`
L_RED=`echo "\033[1;31m"`
L_CYAN=`echo "\033[96m"`
L_BLUE=`echo "\033[94m"`
L_YELLOW=`echo "\033[1;33m"`
L_PURPLE=`echo "\033[95m"`
L_RESET=`echo "\033[0m"`

test_passed="s/✔\|√/${DARK_GREEN}&${RESET}/g;"
test_failed="s/✗\|Х/${RED}&${RESET}/g;"
test_timeout="s/∞/${YELLOW}&${RESET}/g;"
test_undefined="s/?/${YELLOW}&${RESET}/g;"
test_error="s/е/${RED}&${RESET}/g;"
heart="s/♥/${RED}&${RESET}/g;"
star="s/✶/${YELLOW}&${RESET}/g;"

l_test_passed="s/✔\|√/${L_DARK_GREEN}&${L_RESET}/g;"
l_test_failed="s/✗\|Х/${L_RED}&${L_RESET}/g;"
l_test_timeout="s/∞/${L_YELLOW}&${L_RESET}/g;"
l_test_undefined="s/?/${L_YELLOW}&${L_RESET}/g;"
l_test_error="s/е/${L_RED}&${L_RESET}/g;"

# remove_e="s/-е //g;"

git stash
git pull
chmod +x ./runTests.sh

source /etc/profile.d/module.sh > /dev/null 2>&1
module add ghc > /dev/null 2>&1

# exec runhaskell -i../:./ Tester.hs $(whoami) $* |\
#     sed "$remove_e" \
#       -e "$test_passed" \
#       -e "$test_failed" \
#       -e "$test_timeout" \
#       -e "$test_undefined" \
#       -e "$remove_e"

if [ "$*" == "--windows" ]
then
  # echo "working"
  exec runhaskell -i../:./ Tester.hs $(whoami) $* |\
    sed "$test_passed\
      $test_failed\
      $test_timeout\
      $test_undefined\
      $test_error\
      $heart\
      $star"
fi

if [ "$*" != "--windows" ]
then
  # echo linux
  exec runhaskell -i../:./ Tester.hs $(whoami) $* |\
    sed "$l_test_passed\
      $l_test_failed\
      $l_test_timeout\
      $l_test_undefined\
      $l_test_error"
fi