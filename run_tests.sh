#!/bin/bash
total_tests=0
tests_passed=0
tests_failed=0
#for f in $(ls -v AllTests/on_the_flyTests/tests/*.scm); do
#for f in $(ls -v AllTests/*/tests/*.scm); do
#for f in $(ls -v AllTests/*/*/tests/*.scm); do
#for f in $(ls -v AllTests/*/ChloucheTests/tests/test267.scm); do
#for f in $(ls -v AllTests/*/tests/01-some-constants-and-sequences.scm); do
for rawF in $(ls -v AllTests/*/tests/*.scm); do
#for rawF in $(ls -v AllTests/2016Tests/tests/tests1.scm); do
  f=`basename $rawF .scm`
  if [ -f "$f" ]; then
    rm "$f"
  fi
  
  if [ -f "$f.o" ]; then
    rm "$f.o"
  fi
  
  if [ -f "$f.s" ]; then
    rm "$f.s"
  fi
  
  if [ -f "$f.scm" ]; then
    rm "$f.scm"
  fi
  cp $rawF .
  echo "Doing test $rawF:"
  echo "--------------------------------"
  make -f ./project/Makefile $f
  our=`./$f`
  total_tests=$((total_tests+1))
  if [ -f "$(echo $rawF | cut -d '/' -f-2)/sols/$f.scm" ]; then
    chez=`cat "$(echo $rawF | cut -d '/' -f-2)/sols/$f.scm"`
  else
    chez=`cat $f.scm | scheme -q`
  fi
  result=`echo "(equal? '($our) '($chez))" | scheme -q`
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
  if [ -f "$f" ]; then
    rm "$f"
  fi
  
  if [ -f "$f.o" ]; then
    rm "$f.o"
  fi
  
  if [ -f "$f.s" ]; then
    rm "$f.s"
  fi
  
  if [ -f "$f.scm" ]; then
    rm "$f.scm"
  fi
  
done

echo "Number of tests: $total_tests"
echo -e "\033[1;32mNumber of passed tests: $tests_passed \033[0m"
echo -e "\033[1;31mNumber of failed tests: $tests_failed \033[0m"
