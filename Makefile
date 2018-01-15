%:
	rm -f test.s
	echo '(load "compiler.scm") (compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q
	#nasm -f elf64 $(MAKECMDGOALS).s -o $(MAKECMDGOALS).o
	#gcc -m64 $(MAKECMDGOALS).o -o $(MAKECMDGOALS)