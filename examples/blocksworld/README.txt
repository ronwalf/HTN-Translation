Running the blocksworld example with Fast-Downward.  FF-X (FastForward with axioms) would work, but these axioms seem to break it.

$ cd problems
$ htntranslate -p .htn.pddl -l "(achieve-goals)" ../domain.hpddl pfile_???.pddl
$ $(FD-DIR)/src/plan ../domain.htn.pddl pfile_020.htn.pddl $(SEARCH-OPTIONS)

