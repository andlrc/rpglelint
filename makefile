all:	rpglelint.1perl README
.PHONY:	all

rpglelint.1perl:	rpglelint
	pod2man $< > $@
README:			rpglelint
	pod2text $< > $@

test:
	./runtests
.PHONY:	test
