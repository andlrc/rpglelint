all:	rpglelint.1perl README.pod
.PHONY:	all

rpglelint.1perl:	rpglelint
	pod2man $< > $@
README.pod:		rpglelint
	sed '/=head1/,/=cut/!d' $< > $@

test:
	./runtests
.PHONY:	test
