(define
 (problem pfile_3_045)
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
           b41
           b42
           b43
           b44
           b45
           - BLOCK)
 (:init
  (hand-empty arm1)
  (hand-empty arm2)
  (hand-empty arm3)
  (clear b45)
  (on-table b45)
  (clear b19)
  (on-table b32)
  (on b19 b44)
  (on b44 b35)
  (on b35 b32)
  (clear b41)
  (on-table b30)
  (on b41 b30)
  (clear b13)
  (on-table b29)
  (on b13 b29)
  (clear b6)
  (on-table b24)
  (on b6 b43)
  (on b43 b24)
  (clear b38)
  (on-table b22)
  (on b38 b4)
  (on b4 b12)
  (on b12 b31)
  (on b31 b42)
  (on b42 b33)
  (on b33 b5)
  (on b5 b11)
  (on b11 b34)
  (on b34 b28)
  (on b28 b21)
  (on b21 b22)
  (clear b36)
  (on-table b9)
  (on b36 b14)
  (on b14 b15)
  (on b15 b25)
  (on b25 b2)
  (on b2 b40)
  (on b40 b37)
  (on b37 b26)
  (on b26 b39)
  (on b39 b9)
  (clear b8)
  (on-table b1)
  (on b8 b18)
  (on b18 b7)
  (on b7 b20)
  (on b20 b27)
  (on b27 b17)
  (on b17 b10)
  (on b10 b23)
  (on b23 b16)
  (on b16 b3)
  (on b3 b1))
 (:goal (and
         (clear b45)
         (on-table b17)
         (on b45 b29)
         (on b29 b8)
         (on b8 b33)
         (on b33 b13)
         (on b13 b14)
         (on b14 b25)
         (on b25 b26)
         (on b26 b44)
         (on b44 b42)
         (on b42 b7)
         (on b7 b5)
         (on b5 b39)
         (on b39 b24)
         (on b24 b9)
         (on b9 b34)
         (on b34 b22)
         (on b22 b27)
         (on b27 b43)
         (on b43 b12)
         (on b12 b38)
         (on b38 b21)
         (on b21 b30)
         (on b30 b10)
         (on b10 b16)
         (on b16 b1)
         (on b1 b3)
         (on b3 b2)
         (on b2 b23)
         (on b23 b17)
         (clear b41)
         (on-table b15)
         (on b41 b6)
         (on b6 b11)
         (on b11 b20)
         (on b20 b35)
         (on b35 b18)
         (on b18 b15)
         (clear b32)
         (on-table b4)
         (on b32 b19)
         (on b19 b28)
         (on b28 b37)
         (on b37 b31)
         (on b31 b36)
         (on b36 b40)
         (on b40 b4))))