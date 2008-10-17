(define (domain towers)
 (:requirements :strips :disjunctive-preconditions)
 (:types OBJ RING - OBJ TOWER - OBJ)
 (:predicates
  (on ?r - RING ?o - OBJ)
  (towerTop ?o - OBJ ?t - TOWER)
  (smallerThan ?r - RING ?o - OBJ))

 (:action move
  :parameters (?r - RING ?o1 - OBJ ?t1 - TOWER ?o2 - OBJ ?t2 - TOWER)
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


