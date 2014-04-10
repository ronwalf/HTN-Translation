(define
 (problem pfile_025)
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
           - BLOCK)
 (:init
  (hand-empty)
  (clear b20)
  (on-table b15)
  (on b20 b4)
  (on b4 b1)
  (on b1 b23)
  (on b23 b5)
  (on b5 b10)
  (on b10 b11)
  (on b11 b22)
  (on b22 b3)
  (on b3 b16)
  (on b16 b14)
  (on b14 b8)
  (on b8 b21)
  (on b21 b7)
  (on b7 b18)
  (on b18 b24)
  (on b24 b15)
  (clear b12)
  (on-table b2)
  (on b12 b19)
  (on b19 b9)
  (on b9 b17)
  (on b17 b25)
  (on b25 b6)
  (on b6 b13)
  (on b13 b2))
 (:goal (and
         (clear b3)
         (on-table b17)
         (on b3 b19)
         (on b19 b14)
         (on b14 b18)
         (on b18 b24)
         (on b24 b10)
         (on b10 b6)
         (on b6 b22)
         (on b22 b11)
         (on b11 b4)
         (on b4 b20)
         (on b20 b17)
         (clear b2)
         (on-table b16)
         (on b2 b8)
         (on b8 b21)
         (on b21 b25)
         (on b25 b16)
         (clear b1)
         (on-table b15)
         (on b1 b5)
         (on b5 b12)
         (on b12 b13)
         (on b13 b15)
         (clear b23)
         (on-table b9)
         (on b23 b9)
         (clear b7)
         (on-table b7))))