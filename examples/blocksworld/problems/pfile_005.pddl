(define
 (problem pfile_005)
 (:domain blocks)
 (:objects b1 b2 b3 b4 b5 - BLOCK)
 (:init
  (hand-empty)
  (clear b4)
  (on-table b4)
  (clear b5)
  (on-table b1)
  (on b5 b3)
  (on b3 b2)
  (on b2 b1))
 (:goal (and
         (clear b2)
         (on-table b4)
         (on b2 b4)
         (clear b3)
         (on-table b1)
         (on b3 b5)
         (on b5 b1))))