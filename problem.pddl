(define
 (problem basic_problem)
 (:domain basic)
 (:objects
  banjo - OBJ viola - OBJ apple - OBJ)
 (:init
  (have banjo))
 (:goal
  (start_swap apple banjo)
  (have apple)))
 
  
