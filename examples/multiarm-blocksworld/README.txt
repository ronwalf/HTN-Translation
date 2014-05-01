
Generating new multi-arm blocksworld problems with 4 arms:
$ cd problems
$ runghc ../genBlocks.hs pfile_4_ 4 5 100 5

Running the multi-arm blocksworld example with Fast-Downward:

$ cd problems
$ htntranslate -t adl -p .htn.pddl \
        -l "(achieve-goals arm1)" \
        -l "(achieve-goals arm2)" \
        -l "(achieve-goals arm3)" \
        ../domain.hpddl pfile_3_???.pddl
$ $(FD-DIR)/src/plan ../domain.htn.pddl pfile_3_020.htn.pddl \
      --heuristic "hlm,hff=lm_ff_syn(lm_rhw(
                   reasonable_orders=true,lm_cost_type=2,cost_type=2))" \
      --search "lazy_greedy([hff,hlm],preferred=[hff,hlm])"

If your planner doesn't support derived predicates and quantified effects, you 
need to use the STRIPS translation, but it seems to be much slower:
$ htntranslate -t strips -p .htn.pddl -l "(achieve-goals)" ../domain.hpddl pfile_???.pddl
$ ff -o ../domain.htn.pddl -f pfile_3_020.htn.pddl

