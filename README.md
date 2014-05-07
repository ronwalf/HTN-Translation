# HTNTranslation

HTNTranslation is a program for translating [Hierarchical Task Network](http://www.aaai.org/Papers/AAAI/1994/AAAI94-173.pdf) problems into [PDDL](http://www.jair.org/media/1129/live-1129-2132-jair.pdf).  This is an extension of the work described in "[Translating HTNs to PDDL](http://www.umiacs.umd.edu/publications/translating-htns-pddl-small-amount-domain-knowledge-can-go-long-way)," handling both totally ordered and partially ordered subtasks.

## Requirements and installation.
HTNTranslation requires a recent copy of [Haskell](http://hackage.haskell.org/platform/), being most recently tested on GHC 7.4.2.  Beyond that, we also require a copy of the [Planning](http://github.com/ronwalf/Planning) Haskell PDDL parsing and representation library. After downloading and unpacking both Planning and HTNTranslation, install them using [Cabal](http://www.haskell.org/cabal/).  For example, to install for just the current user:

    $ cd <planning dir>
    $ cabal install --user
    ...
    $ cd -
    $ cd <htntranslation dir>
    $ cabal install --user
    ...

HTNTranslation installs one executable, `htntranslate`, which is self documenting.  See `examples/toy/README.txt` for a demonstration of how to translate a domain and problem and then find a solution using the [FF](http://fai.cs.uni-saarland.de/hoffmann/ff.html) planner.  

For non-tail recursive problems, it is necessary to specify the '-i' parameter, which specifies how many constants need to be added to the domain for the HTN to run.  This roughly corresponds to the non-tail recursion depth of your HTNs.  `htntranslate` calculates the bound automatically for tail recursive problems.

## Translation types

There are four different translation types currently available - two for totally-ordered (TO) problems and two for partially-ordered (PO) problems.  You can specify the translation type with the `-t` option.

* `strips`: A STRIPS-compatible translation for both TO and PO problems.  This should work with most planners, although planners may be rather slow with it.
* `adl`: An ADL-compatible translation for both TO and PO problems.  This makes use of quantified effects, axioms, and negated preconditions.  Both FF-X and FastDownward are faster with ADL translations
* `ordered`: A STRIPS-compatible translation for TO problems.  This translation is generally faster than the ADL translation for the problems it covers.
* `ordered09`: An implementation of the original translation appearing in IJCAI-09.  The automated translation bounds are overly conservative for this translation, and so you may get slightly better performance by manually specifying the `-i` option.
    

## Syntax
The syntax is based on PDDL, with task names specified much like predicates, and methods specified much like actions.  

### Task specification
In the header of the domain file, all task names should be defined.  From the `toy.hpddl` example included in the distribution:

    (:tasks
      (swap ?x - OBJ ?y - OBJ)
      (donothing)
      (pickup ?x - OBJ)
      (drop ?x - OBJ))

### Method and operator  specification
Methods are specified much like actions, but without effects and with task lists.  Again, from the `toy.hpddl` example:

    (:method swap1
     :parameters (?x - OBJ ?y - OBJ)
     :task (swap ?x ?y)
     :precondition (have ?x)
     :tasks ((drop ?x) (pickup ?y)))

In a break with other HTN systems, there is no syntactic distinction between primitive and non-primitive tasks. We implement primitive tasks by adding a `:task (...)` field to an operator.  The only restriction to what task an operator may implement is that all the variables in the `:task (...)` field must be in the action's parameter list:

    (:action pickup
     :parameters (?x - OBJ)
     :task (pickup ?x)
     :precondition (handempty)
     :effect (have ?x))

### Specifying sub tasks
A method may have one or more `:tasks (...)` field.  The order of the task names in the field specifies the ordering constraints over the subtasks.  So in the above `swap1` example, we must `(drop ?x)` before we `(pickup ?y)`.

We can specify arbitrary partial orders by naming the task fields and adding an `:ordering (..)` constraint.  From the `examples/toy/ordering.hpddl` example:

    (:method t0
     :parameters ()
     :task (t0)
     :tasks (h (c))
     :tasks (i (t1))
     :tasks (j (t2))
     :tasks (k (c))
     :ordering ( (h i) (h j) (i k) (j k) ))

The `t0` method implements the task `(t0)` and has four subtasks, `(c)`, `(t1)`, `(t2)`, and `(c)`.  Here we name each of the subtasks `h` through `k`, and then restrict `h` to come before `i` and `j`, and `i` and `j` to come before `k`.  This lets us interleave the decomposition of `(t1)` and `(t2)`.

### Problem specification
Problems are specified as PDDL notation with the addition of the `:tasks` and `:ordering` fields common to methods.  The `:goal(...)` field is optional.
