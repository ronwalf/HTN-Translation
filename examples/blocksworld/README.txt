Running the blocksworld example with Fast-Downward or FF:

$ cd problems
$ htntranslate -p .htn.pddl -l "(achieve-goals)" ../domain.hpddl pfile_???.pddl
$ $(FD-DIR)/src/plan ../domain.htn.pddl pfile_020.htn.pddl $(SEARCH-OPTIONS)
$ ff -o ../domain.htn.pddl -f pfile_020.htn.pddl

The plain 'domain.pddl' file (as opposed to 'domain.hpddl') is a
run-of-the-mill STRIPS formulation of blocksworld.

