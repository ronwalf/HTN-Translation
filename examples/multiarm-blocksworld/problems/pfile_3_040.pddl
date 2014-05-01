(define
 (problem pfile_3_040)
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
           b36
           b37
           b38
           b39
           b40
           - BLOCK)
 (:init
  (hand-empty arm1)
  (hand-empty arm2)
  (hand-empty arm3)
  (clear b10)
  (on-table b39)
  (on b10 b39)
  (clear b38)
  (on-table b38)
  (clear b29)
  (on-table b26)
  (on b29 b12)
  (on b12 b27)
  (on b27 b2)
  (on b2 b34)
  (on b34 b40)
  (on b40 b18)
  (on b18 b23)
  (on b23 b33)
  (on b33 b15)
  (on b15 b7)
  (on b7 b16)
  (on b16 b5)
  (on b5 b26)
  (clear b30)
  (on-table b25)
  (on b30 b22)
  (on b22 b20)
  (on b20 b37)
  (on b37 b25)
  (clear b28)
  (on-table b24)
  (on b28 b4)
  (on b4 b24)
  (clear b32)
  (on-table b6)
  (on b32 b21)
  (on b21 b36)
  (on b36 b14)
  (on b14 b6)
  (clear b1)
  (on-table b3)
  (on b1 b17)
  (on b17 b13)
  (on b13 b35)
  (on b35 b11)
  (on b11 b8)
  (on b8 b31)
  (on b31 b19)
  (on b19 b9)
  (on b9 b3))
 (:goal (and
         (clear b32)
         (on-table b38)
         (on b32 b36)
         (on b36 b39)
         (on b39 b35)
         (on b35 b38)
         (clear b19)
         (on-table b29)
         (on b19 b29)
         (clear b20)
         (on-table b22)
         (on b20 b3)
         (on b3 b6)
         (on b6 b26)
         (on b26 b22)
         (clear b9)
         (on-table b16)
         (on b9 b37)
         (on b37 b23)
         (on b23 b8)
         (on b8 b25)
         (on b25 b7)
         (on b7 b14)
         (on b14 b27)
         (on b27 b33)
         (on b33 b21)
         (on b21 b16)
         (clear b12)
         (on-table b13)
         (on b12 b28)
         (on b28 b13)
         (clear b17)
         (on-table b11)
         (on b17 b10)
         (on b10 b40)
         (on b40 b15)
         (on b15 b11)
         (clear b1)
         (on-table b5)
         (on b1 b18)
         (on b18 b30)
         (on b30 b4)
         (on b4 b31)
         (on b31 b24)
         (on b24 b34)
         (on b34 b5)
         (clear b2)
         (on-table b2))))