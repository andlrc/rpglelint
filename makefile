all:	rpglelint.1perl README.pod
.PHONY:	all

rpglelint.1perl:	rpglelint
	pod2man -c "User Commands" -r "$$(./rpglelint -V)" $< > $@
README.pod:		rpglelint
	sed '/=head1/,/=cut/!d' $< > $@

test:
	./runtests
.PHONY:	test
