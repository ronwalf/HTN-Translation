(define
 (problem basic_problem)
 (:domain basic)
 (:objects
  banjo - OBJ viola - OBJ apple - OBJ)
 (:init
  (start_swap apple banjo)
  (have banjo))
 (:goal (and
  (have apple)
  (stackTop stackDigit1)
  )))

 
  
