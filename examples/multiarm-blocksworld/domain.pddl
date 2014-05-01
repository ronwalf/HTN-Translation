(define (domain blocks)
  (:requirements :strips :disjunctive-preconditions)
  (:types ARM BLOCK)
  (:predicates
    (hand-empty ?a - ARM)
    (clear ?b - BLOCK)
    (holding ?b - BLOCK ?a - ARM)
    (on ?top - BLOCK ?bottom - BLOCK)
    (on-table ?b - BLOCK))

  (:action pickup
    :parameters (?b - BLOCK ?a - ARM)
    :precondition (and (hand-empty ?a) (clear ?b) (on-table ?b))
    :effect (and
      (not (hand-empty ?a))
      (not (clear ?b)) 
      (not (on-table ?b))
      (holding ?b ?a)))

  (:action putdown
    :parameters (?b - BLOCK ?a - ARM)
    :precondition (holding ?b ?a)
    :effect (and
      (hand-empty ?a)
      (not (holding ?b ?a))
      (on-table ?b) (clear ?b)))

  (:action stack
    :parameters (?top - BLOCK ?bottom - BLOCK ?a - ARM)
    :precondition (and
      (holding ?top ?a)
      (clear ?bottom))
    :effect (and
      (hand-empty ?a)
      (not (holding ?top ?a))
      (not (clear ?bottom))
      (on ?top ?bottom)
      (clear ?top)))

  (:action unstack
    :parameters (?top - BLOCK ?bottom - BLOCK ?a - ARM)
    :precondition (and
      (hand-empty ?a)
      (clear ?top)
      (on ?top ?bottom))
    :effect (and
      (not (hand-empty ?a))
      (not (clear ?top))
      (not (on ?top ?bottom))
      (holding ?top ?a)
      (clear ?bottom))))

