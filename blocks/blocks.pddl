(define (domain blocks)
  (:requirements :strips :disjunctive-preconditions)
  (:types BLOCK)
  (:predicates
    (clear ?b - BLOCK)
    (holding ?b - BLOCK)
    (on ?top - BLOCK ?bottom - BLOCK)
    (on-table ?b - BLOCK))

  (:action pickup
    :parameters (?b - BLOCK)
    :precondition (and (clear ?b) (on-table ?b))
    :effect (and
      (not (clear ?b)) 
      (not (on-table ?b))
      (holding ?b)))

  (:action putdown
    :parameters (?b - BLOCK)
    :precondition (holding ?b)
    :effect (and
      (not (holding ?b))
      (on-table ?b) (clear ?b)))

  (:action stack
    :parameters (?bottom - BLOCK ?top - BLOCK)
    :precondition (and
      (holding ?top)
      (clear ?bottom))
    :effect (and
      (not (holding ?top))
      (not (clear ?bottom))
      (on ?top ?bottom)
      (clear ?top)))

  (:action unstack
    :parameters (?bottom - BLOCK ?top - BLOCK)
    :precondition (and
      (clear ?top)
      (on ?top ?bottom))
    :effect (and
      (not (clear ?top))
      (not (on ?top ?bottom))
      (holding ?top)
      (clear ?bottom))))

