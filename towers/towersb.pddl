(define (domain towers)
 (:requirements :strips :disjunctive-preconditions)
 (:predicates
  (on ?r ?o)
  (towerTop ?o ?t)
  (smallerThan ?r ?o))

 (:action move
  :parameters (?r ?o1 ?t1 ?o2 ?t2)
  :precondition (and
    (on ?r ?o1) (towerTop ?r ?t1)
    (towerTop ?o2 ?t2) (smallerThan ?r ?o2))
  :effect (and
    (not (on ?r ?o1)) 
    (on ?r ?o2)
    (not (towerTop ?r ?t1))
    (towerTop ?o1 ?t1) 
    (not (towerTop ?o2 ?t2))
    (towerTop ?r ?t2))))


