(define
 (problem pfile_035)
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
           b31
           b32
           b33
           b34
           b35
           - BLOCK)
 (:init
  (hand-empty)
  (clear b26)
  (on-table b33)
  (on b26 b33)
  (clear b15)
  (on-table b25)
  (on b15 b11)
  (on b11 b24)
  (on b24 b4)
  (on b4 b3)
  (on b3 b25)
  (clear b12)
  (on-table b18)
  (on b12 b18)
  (clear b22)
  (on-table b17)
  (on b22 b8)
  (on b8 b7)
  (on b7 b34)
  (on b34 b2)
  (on b2 b32)
  (on b32 b16)
  (on b16 b35)
  (on b35 b21)
  (on b21 b23)
  (on b23 b29)
  (on b29 b13)
  (on b13 b9)
  (on b9 b30)
  (on b30 b28)
  (on b28 b1)
  (on b1 b27)
  (on b27 b17)
  (clear b20)
  (on-table b5)
  (on b20 b19)
  (on b19 b6)
  (on b6 b14)
  (on b14 b10)
  (on b10 b31)
  (on b31 b5))
 (:goal (and
         (clear b5)
         (on-table b33)
         (on b5 b6)
         (on b6 b11)
         (on b11 b34)
         (on b34 b17)
         (on b17 b20)
         (on b20 b8)
         (on b8 b26)
         (on b26 b33)
         (clear b9)
         (on-table b25)
         (on b9 b31)
         (on b31 b14)
         (on b14 b32)
         (on b32 b35)
         (on b35 b16)
         (on b16 b24)
         (on b24 b25)
         (clear b1)
         (on-table b12)
         (on b1 b27)
         (on b27 b4)
         (on b4 b13)
         (on b13 b30)
         (on b30 b2)
         (on b2 b18)
         (on b18 b21)
         (on b21 b19)
         (on b19 b22)
         (on b22 b23)
         (on b23 b28)
         (on b28 b15)
         (on b15 b29)
         (on b29 b12)
         (clear b3)
         (on-table b7)
         (on b3 b10)
         (on b10 b7))))