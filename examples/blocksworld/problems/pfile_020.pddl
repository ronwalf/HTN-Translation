(define
 (problem pfile_020)
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
           b16
           b17
           b18
           b19
           b20
           - BLOCK)
 (:init
  (hand-empty)
  (clear b11)
  (on-table b19)
  (on b11 b20)
  (on b20 b19)
  (clear b15)
  (on-table b18)
  (on b15 b6)
  (on b6 b5)
  (on b5 b17)
  (on b17 b10)
  (on b10 b18)
  (clear b3)
  (on-table b16)
  (on b3 b2)
  (on b2 b7)
  (on b7 b1)
  (on b1 b9)
  (on b9 b16)
  (clear b12)
  (on-table b4)
  (on b12 b14)
  (on b14 b13)
  (on b13 b8)
  (on b8 b4))
 (:goal (and
         (clear b7)
         (on-table b15)
         (on b7 b13)
         (on b13 b15)
         (clear b19)
         (on-table b9)
         (on b19 b9)
         (clear b14)
         (on-table b1)
         (on b14 b3)
         (on b3 b16)
         (on b16 b20)
         (on b20 b18)
         (on b18 b4)
         (on b4 b6)
         (on b6 b2)
         (on b2 b12)
         (on b12 b10)
         (on b10 b5)
         (on b5 b11)
         (on b11 b17)
         (on b17 b8)
         (on b8 b1))))