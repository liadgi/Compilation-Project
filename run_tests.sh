total_tests=0
tests_passed=0
tests_failed=0
for f in $(ls -v AllTests/on_the_flyTests/tests/*.scm); do
#for f in $(ls -v AllTests/*/tests/*.scm); do
#for f in $(ls -v AllTests/*/tests/01-some-constants-and-sequences.scm); do
  if [ -f "out_test.c" ]; then
    rm "out_test.c"
  fi
  echo "Doing test $f:"
  echo "--------------------------------"
  echo "(load \"compiler.scm\") (compile-scheme-file \"$f\" \"out_test.s\")" | scheme -q
  nasm -g -f elf64 out_test.s -o out_test.o
  gcc -m64 out_test.o -o out_test
  our=`./out_test`
  total_tests=$((total_tests+1))
  if [ -f "$(dirname $f)/../sols/$(basename $f)" ]; then 
    chez=`cat "$(dirname $f)/../sols/$(basename $f)"`
  else
    chez=`cat $f | scheme -q`
  fi
  result=""
  if [ "$our" = "$chez" ]; then
    result="#t"
  else
    result=`echo "(equal? \"$our\" \"$chez\")" | scheme -q`
  fi
  
  if [ "$result" = "#t" ]; then 
      echo -e "\033[1;32mTest $f Passed ☺ \033[0m"
      tests_passed=$((tests_passed+1))
  else
      echo -e "\033[1;31m *** RESULTS DIFFER in $f ☹\033[0m"
      echo "*** scheme output: $chez"
      echo "*** our output: $our"
      tests_failed=$((tests_failed+1))
  fi
  echo "--------------------------------"
  echo
  if [ -f "out_test.s" ]; then
    rm "out_test.s"
  fi
  if [ -f "out_test.o" ]; then
    rm "out_test.o"
  fi
  if [ -f "out_test" ]; then
    rm "out_test"
  fi
done

echo "Number of tests: $total_tests"
echo -e "\033[1;32mNumber of passed tests: $tests_passed \033[0m"
echo -e "\033[1;31mNumber of failed tests: $tests_failed \033[0m"
