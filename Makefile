%:
	rm -f test.s test
	rm -rf *.o	
	rm -f scheme
	echo '(load "compiler.scm") (compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q
	nasm -g -f elf64 $(MAKECMDGOALS).s -o $(MAKECMDGOALS).o
	gcc -m64 $(MAKECMDGOALS).o -o $(MAKECMDGOALS)



.PHONY: clean

clean: 
	rm -rf *.o test scheme
