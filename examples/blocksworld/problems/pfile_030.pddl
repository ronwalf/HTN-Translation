(define
 (problem pfile_030)
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
  (hand-empty)
  (clear b10)
  (on-table b29)
  (on b10 b19)
  (on b19 b11)
  (on b11 b6)
  (on b6 b25)
  (on b25 b21)
  (on b21 b18)
  (on b18 b7)
  (on b7 b26)
  (on b26 b12)
  (on b12 b29)
  (clear b17)
  (on-table b20)
  (on b17 b16)
  (on b16 b23)
  (on b23 b24)
  (on b24 b13)
  (on b13 b22)
  (on b22 b14)
  (on b14 b27)
  (on b27 b5)
  (on b5 b28)
  (on b28 b8)
  (on b8 b3)
  (on b3 b30)
  (on b30 b4)
  (on b4 b2)
  (on b2 b15)
  (on b15 b1)
  (on b1 b9)
  (on b9 b20))
 (:goal (and
         (clear b30)
         (on-table b18)
         (on b30 b6)
         (on b6 b22)
         (on b22 b27)
         (on b27 b18)
         (clear b15)
         (on-table b13)
         (on b15 b2)
         (on b2 b7)
         (on b7 b16)
         (on b16 b11)
         (on b11 b19)
         (on b19 b1)
         (on b1 b14)
         (on b14 b17)
         (on b17 b24)
         (on b24 b23)
         (on b23 b29)
         (on b29 b10)
         (on b10 b9)
         (on b9 b8)
         (on b8 b21)
         (on b21 b3)
         (on b3 b20)
         (on b20 b12)
         (on b12 b28)
         (on b28 b26)
         (on b26 b5)
         (on b5 b4)
         (on b4 b25)
         (on b25 b13))))