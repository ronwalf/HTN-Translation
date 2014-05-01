(define
 (problem pfile_3_050)
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
           b46
           b47
           b48
           b49
           b50
           - BLOCK)
 (:init
  (hand-empty arm1)
  (hand-empty arm2)
  (hand-empty arm3)
  (clear b43)
  (on-table b38)
  (on b43 b10)
  (on b10 b49)
  (on b49 b47)
  (on b47 b15)
  (on b15 b31)
  (on b31 b38)
  (clear b28)
  (on-table b33)
  (on b28 b19)
  (on b19 b33)
  (clear b32)
  (on-table b21)
  (on b32 b21)
  (clear b40)
  (on-table b17)
  (on b40 b16)
  (on b16 b17)
  (clear b5)
  (on-table b3)
  (on b5 b4)
  (on b4 b25)
  (on b25 b20)
  (on b20 b8)
  (on b8 b27)
  (on b27 b45)
  (on b45 b9)
  (on b9 b39)
  (on b39 b3)
  (clear b24)
  (on-table b1)
  (on b24 b14)
  (on b14 b37)
  (on b37 b42)
  (on b42 b12)
  (on b12 b35)
  (on b35 b50)
  (on b50 b6)
  (on b6 b41)
  (on b41 b48)
  (on b48 b44)
  (on b44 b29)
  (on b29 b22)
  (on b22 b7)
  (on b7 b34)
  (on b34 b13)
  (on b13 b18)
  (on b18 b23)
  (on b23 b26)
  (on b26 b2)
  (on b2 b11)
  (on b11 b36)
  (on b36 b30)
  (on b30 b46)
  (on b46 b1))
 (:goal (and
         (clear b48)
         (on-table b48)
         (clear b23)
         (on-table b38)
         (on b23 b38)
         (clear b44)
         (on-table b36)
         (on b44 b2)
         (on b2 b19)
         (on b19 b39)
         (on b39 b1)
         (on b1 b50)
         (on b50 b27)
         (on b27 b36)
         (clear b3)
         (on-table b35)
         (on b3 b4)
         (on b4 b46)
         (on b46 b49)
         (on b49 b24)
         (on b24 b35)
         (clear b9)
         (on-table b26)
         (on b9 b31)
         (on b31 b18)
         (on b18 b11)
         (on b11 b26)
         (clear b43)
         (on-table b21)
         (on b43 b21)
         (clear b47)
         (on-table b15)
         (on b47 b12)
         (on b12 b29)
         (on b29 b16)
         (on b16 b10)
         (on b10 b25)
         (on b25 b8)
         (on b8 b45)
         (on b45 b33)
         (on b33 b28)
         (on b28 b34)
         (on b34 b5)
         (on b5 b30)
         (on b30 b32)
         (on b32 b41)
         (on b41 b15)
         (clear b17)
         (on-table b14)
         (on b17 b37)
         (on b37 b42)
         (on b42 b7)
         (on b7 b40)
         (on b40 b20)
         (on b20 b22)
         (on b22 b13)
         (on b13 b6)
         (on b6 b14))))