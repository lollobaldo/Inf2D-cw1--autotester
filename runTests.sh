#!/bin/sh

GREEN=`echo -e "\033[92m"`
DARK_GREEN=`echo -e "\033[1;32m"`
RED=`echo -e "\033[1;31m"`
CYAN=`echo -e "\033[96m"`
BLUE=`echo -e "\033[94m"`
YELLOW=`echo -e "\033[1;33m"`
PURPLE=`echo -e "\033[95m"`
RESET=`echo -e "\033[0m"`

test_passed="s/✔\|√/${DARK_GREEN}&${RESET}/g;"
test_failed="s/✗\|Х/${RED}&${RESET}/g;"
test_timeout="s/∞/${YELLOW}&${RESET}/g;"
test_undefined="s/?/${YELLOW}&${RESET}/g;"
test_error="s/е/${RED}&${RESET}/g;"

remove_e="s/-е //g;"

git stash
git pull
chmod +x ./runTests.sh

if [ "$*" == "*--windows*" ]
then
  # echo "working"
  exec runhaskell -i../:./ Tester.hs $(whoami) $* |\
    sed "$test_passed\
      $test_failed\
      $test_timeout\
      $test_undefined\
      $test_error"
fi

if [ "$*" != "*--windows*" ]
then
  exec runhaskell -i../:./ Tester.hs $(whoami) $* |\
    sed "$remove_e\
      $test_passed\
      $test_failed\
      $test_timeout\
      $test_undefined\
      $test_error\
      $remove_e"
fi