(define
 (problem pfile_005)
 (:domain blocks)
 (:objects b1 - BLOCK b2 - BLOCK b3 - BLOCK b4 - BLOCK b5 - BLOCK)
 (:init
  (armempty)
  (clear b5)
  (on-table b5)
  (clear b3)
  (on-table b3)
  (clear b4)
  (on-table b2)
  (on b4 b1)
  (on b1 b2))
 (:goal
  (and
   (clear b4)
   (on-table b4)
   (clear b1)
   (on-table b2)
   (on b1 b3)
   (on b3 b5)
   (on b5 b2))))