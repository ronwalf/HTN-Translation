(define
 (problem pfile_3_020)
 (:domain blocks)
 (:objects arm1 arm2 arm3 - ARM
           b1
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
  (hand-empty arm1)
  (hand-empty arm2)
  (hand-empty arm3)
  (clear b4)
  (on-table b16)
  (on b4 b19)
  (on b19 b7)
  (on b7 b16)
  (clear b5)
  (on-table b14)
  (on b5 b8)
  (on b8 b17)
  (on b17 b20)
  (on b20 b1)
  (on b1 b10)
  (on b10 b12)
  (on b12 b14)
  (clear b18)
  (on-table b13)
  (on b18 b2)
  (on b2 b9)
  (on b9 b11)
  (on b11 b15)
  (on b15 b13)
  (clear b3)
  (on-table b6)
  (on b3 b6))
 (:goal (and
         (clear b15)
         (on-table b20)
         (on b15 b6)
         (on b6 b20)
         (clear b16)
         (on-table b16)
         (clear b10)
         (on-table b4)
         (on b10 b12)
         (on b12 b11)
         (on b11 b8)
         (on b8 b14)
         (on b14 b17)
         (on b17 b5)
         (on b5 b4)
         (clear b9)
         (on-table b3)
         (on b9 b19)
         (on b19 b3)
         (clear b13)
         (on-table b2)
         (on b13 b7)
         (on b7 b18)
         (on b18 b2)
         (clear b1)
         (on-table b1))))