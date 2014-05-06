Running the towers of hanoi example with Fast-Downward or FF:

$ cd problems
$ htntranslate -t ordered -p .htn.pddl -l "(shiftTower t1 t2 t3)" ../domain.hpddl pfile_??.pddl
$ $(FD-DIR)/src/plan ../domain.htn.pddl pfile_10.htn.pddl $(SEARCH-OPTIONS)
$ ff -o ../domain.htn.pddl -f pfile_10.htn.pddl


