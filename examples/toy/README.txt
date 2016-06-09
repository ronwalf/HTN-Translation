Here are a couple of toy examples and basic instructions on how to run them with FF-X (Fast-Forward with axioms)

htntranslate -i4 basic.hpddl basic-problem.hpddl &&  ff -o basic.pddl -f basic-problem.pddl

htnunlift basic.hpddl basic-problem-ungrounded.hpddl && \
htntranslate -i4 d-basic-problem-ungrounded.hpddl p-basic-problem-ungrounded.hpddl && \
ff-x -o d-basic-problem-ungrounded.pddl -f p-basic-problem-ungrounded.pddl

htntranslate ordering.hpddl ordering-problem.hpddl &&  ff -o ordering.pddl -f ordering-problem.pddl

htntranslate ordering2.hpddl ordering-problem.hpddl &&  ff -o ordering2.pddl -f ordering-problem.pddl

