(define
 (problem pfile_015)
 (:domain blocks)
 (:objects b1
           b2
           b3
           b4
           b5
           b6
           b7
           b8
           b9
           b10
           b11
           b12
           b13
           b14
           b15
           - BLOCK)
 (:init
  (hand-empty)
  (clear b15)
  (on-table b12)
  (on b15 b13)
  (on b13 b2)
  (on b2 b14)
  (on b14 b9)
  (on b9 b4)
  (on b4 b6)
  (on b6 b12)
  (clear b7)
  (on-table b3)
  (on b7 b11)
  (on b11 b10)
  (on b10 b3)
  (clear b8)
  (on-table b1)
  (on b8 b5)
  (on b5 b1))
 (:goal (and
         (clear b3)
         (on-table b14)
         (on b3 b6)
         (on b6 b10)
         (on b10 b11)
         (on b11 b14)
         (clear b15)
         (on-table b7)
         (on b15 b7)
         (clear b1)
         (on-table b5)
         (on b1 b9)
         (on b9 b12)
         (on b12 b13)
         (on b13 b8)
         (on b8 b2)
         (on b2 b4)
         (on b4 b5))))