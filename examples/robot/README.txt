Running the robot navigation example with Fast-Downward or FF:

$ cd problems
$ htntranslate -t ordered -p .htn.pddl -l "(achieve-goals)" ../domain.hpddl pfile_??_??.pddl
$ $(FD-DIR)/src/plan ../domain.htn.pddl pfile_10_10.htn.pddl $(SEARCH-OPTIONS)
$ ff -o ../domain.htn.pddl -f pfile_10_10.htn.pddl


