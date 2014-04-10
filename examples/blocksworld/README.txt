Running the blocksworld example with FF-X (FastForward with axioms):

$ cd problems
$ htntranslate -p .htn.pddl -l "(achieve-goals)" -i 3 ../domain.hpddl pfile_???.pddl
$ ff -o ../domain.htn.pddl -f pfile_020.htn.pddl

