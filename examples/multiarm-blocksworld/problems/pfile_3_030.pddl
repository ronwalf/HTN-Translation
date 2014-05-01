(define
 (problem pfile_3_030)
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
           b26
           b27
           b28
           b29
           b30
           - BLOCK)
 (:init
  (hand-empty arm1)
  (hand-empty arm2)
  (hand-empty arm3)
  (clear b10)
  (on-table b30)
  (on b10 b5)
  (on b5 b15)
  (on b15 b3)
  (on b3 b28)
  (on b28 b7)
  (on b7 b8)
  (on b8 b6)
  (on b6 b21)
  (on b21 b12)
  (on b12 b4)
  (on b4 b17)
  (on b17 b19)
  (on b19 b18)
  (on b18 b30)
  (clear b23)
  (on-table b23)
  (clear b1)
  (on-table b20)
  (on b1 b2)
  (on b2 b24)
  (on b24 b14)
  (on b14 b26)
  (on b26 b22)
  (on b22 b27)
  (on b27 b29)
  (on b29 b20)
  (clear b25)
  (on-table b16)
  (on b25 b16)
  (clear b13)
  (on-table b9)
  (on b13 b11)
  (on b11 b9))
 (:goal (and
         (clear b14)
         (on-table b25)
         (on b14 b25)
         (clear b13)
         (on-table b21)
         (on b13 b27)
         (on b27 b18)
         (on b18 b11)
         (on b11 b21)
         (clear b20)
         (on-table b2)
         (on b20 b23)
         (on b23 b7)
         (on b7 b19)
         (on b19 b22)
         (on b22 b26)
         (on b26 b30)
         (on b30 b6)
         (on b6 b9)
         (on b9 b2)
         (clear b29)
         (on-table b1)
         (on b29 b15)
         (on b15 b3)
         (on b3 b8)
         (on b8 b28)
         (on b28 b4)
         (on b4 b5)
         (on b5 b24)
         (on b24 b12)
         (on b12 b10)
         (on b10 b16)
         (on b16 b17)
         (on b17 b1))))