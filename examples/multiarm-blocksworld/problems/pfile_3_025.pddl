(define
 (problem pfile_3_025)
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
           b21
           b22
           b23
           b24
           b25
           - BLOCK)
 (:init
  (hand-empty arm1)
  (hand-empty arm2)
  (hand-empty arm3)
  (clear b2)
  (on-table b16)
  (on b2 b21)
  (on b21 b16)
  (clear b23)
  (on-table b15)
  (on b23 b5)
  (on b5 b19)
  (on b19 b15)
  (clear b24)
  (on-table b11)
  (on b24 b11)
  (clear b25)
  (on-table b9)
  (on b25 b3)
  (on b3 b12)
  (on b12 b1)
  (on b1 b18)
  (on b18 b6)
  (on b6 b10)
  (on b10 b22)
  (on b22 b4)
  (on b4 b7)
  (on b7 b14)
  (on b14 b20)
  (on b20 b13)
  (on b13 b9)
  (clear b17)
  (on-table b8)
  (on b17 b8))
 (:goal (and
         (clear b25)
         (on-table b23)
         (on b25 b7)
         (on b7 b23)
         (clear b18)
         (on-table b15)
         (on b18 b13)
         (on b13 b5)
         (on b5 b16)
         (on b16 b10)
         (on b10 b17)
         (on b17 b15)
         (clear b1)
         (on-table b8)
         (on b1 b6)
         (on b6 b3)
         (on b3 b9)
         (on b9 b11)
         (on b11 b20)
         (on b20 b21)
         (on b21 b24)
         (on b24 b14)
         (on b14 b8)
         (clear b12)
         (on-table b4)
         (on b12 b4)
         (clear b22)
         (on-table b2)
         (on b22 b19)
         (on b19 b2))))