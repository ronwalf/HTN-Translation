(define
 (problem pfile_3_035)
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
           b31
           b32
           b33
           b34
           b35
           - BLOCK)
 (:init
  (hand-empty arm1)
  (hand-empty arm2)
  (hand-empty arm3)
  (clear b26)
  (on-table b34)
  (on b26 b11)
  (on b11 b17)
  (on b17 b1)
  (on b1 b14)
  (on b14 b18)
  (on b18 b13)
  (on b13 b34)
  (clear b23)
  (on-table b31)
  (on b23 b28)
  (on b28 b12)
  (on b12 b31)
  (clear b2)
  (on-table b27)
  (on b2 b25)
  (on b25 b33)
  (on b33 b27)
  (clear b35)
  (on-table b20)
  (on b35 b20)
  (clear b15)
  (on-table b10)
  (on b15 b10)
  (clear b24)
  (on-table b9)
  (on b24 b3)
  (on b3 b32)
  (on b32 b6)
  (on b6 b22)
  (on b22 b9)
  (clear b8)
  (on-table b8)
  (clear b29)
  (on-table b5)
  (on b29 b7)
  (on b7 b16)
  (on b16 b19)
  (on b19 b21)
  (on b21 b4)
  (on b4 b30)
  (on b30 b5))
 (:goal (and
         (clear b25)
         (on-table b29)
         (on b25 b22)
         (on b22 b33)
         (on b33 b13)
         (on b13 b34)
         (on b34 b21)
         (on b21 b10)
         (on b10 b19)
         (on b19 b2)
         (on b2 b11)
         (on b11 b29)
         (clear b5)
         (on-table b23)
         (on b5 b4)
         (on b4 b18)
         (on b18 b26)
         (on b26 b24)
         (on b24 b30)
         (on b30 b27)
         (on b27 b20)
         (on b20 b31)
         (on b31 b7)
         (on b7 b35)
         (on b35 b14)
         (on b14 b23)
         (clear b3)
         (on-table b15)
         (on b3 b17)
         (on b17 b16)
         (on b16 b9)
         (on b9 b28)
         (on b28 b8)
         (on b8 b1)
         (on b1 b15)
         (clear b32)
         (on-table b12)
         (on b32 b12)
         (clear b6)
         (on-table b6))))