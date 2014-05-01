(define
 (problem pfile_3_060)
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
           b51
           b52
           b53
           b54
           b55
           b56
           b57
           b58
           b59
           b60
           - BLOCK)
 (:init
  (hand-empty arm1)
  (hand-empty arm2)
  (hand-empty arm3)
  (clear b15)
  (on-table b43)
  (on b15 b51)
  (on b51 b52)
  (on b52 b53)
  (on b53 b43)
  (clear b22)
  (on-table b42)
  (on b22 b21)
  (on b21 b56)
  (on b56 b50)
  (on b50 b42)
  (clear b58)
  (on-table b30)
  (on b58 b38)
  (on b38 b5)
  (on b5 b4)
  (on b4 b29)
  (on b29 b20)
  (on b20 b54)
  (on b54 b18)
  (on b18 b14)
  (on b14 b35)
  (on b35 b1)
  (on b1 b33)
  (on b33 b47)
  (on b47 b39)
  (on b39 b30)
  (clear b27)
  (on-table b23)
  (on b27 b2)
  (on b2 b16)
  (on b16 b8)
  (on b8 b13)
  (on b13 b23)
  (clear b31)
  (on-table b10)
  (on b31 b25)
  (on b25 b19)
  (on b19 b59)
  (on b59 b37)
  (on b37 b3)
  (on b3 b17)
  (on b17 b41)
  (on b41 b10)
  (clear b36)
  (on-table b9)
  (on b36 b9)
  (clear b11)
  (on-table b7)
  (on b11 b46)
  (on b46 b57)
  (on b57 b34)
  (on b34 b40)
  (on b40 b32)
  (on b32 b24)
  (on b24 b6)
  (on b6 b44)
  (on b44 b26)
  (on b26 b60)
  (on b60 b55)
  (on b55 b45)
  (on b45 b28)
  (on b28 b48)
  (on b48 b12)
  (on b12 b49)
  (on b49 b7))
 (:goal (and
         (clear b6)
         (on-table b58)
         (on b6 b31)
         (on b31 b53)
         (on b53 b52)
         (on b52 b12)
         (on b12 b35)
         (on b35 b40)
         (on b40 b37)
         (on b37 b4)
         (on b4 b58)
         (clear b24)
         (on-table b48)
         (on b24 b2)
         (on b2 b56)
         (on b56 b9)
         (on b9 b48)
         (clear b41)
         (on-table b26)
         (on b41 b47)
         (on b47 b3)
         (on b3 b27)
         (on b27 b30)
         (on b30 b54)
         (on b54 b44)
         (on b44 b16)
         (on b16 b26)
         (clear b19)
         (on-table b22)
         (on b19 b28)
         (on b28 b42)
         (on b42 b7)
         (on b7 b22)
         (clear b39)
         (on-table b15)
         (on b39 b25)
         (on b25 b29)
         (on b29 b45)
         (on b45 b51)
         (on b51 b49)
         (on b49 b60)
         (on b60 b23)
         (on b23 b8)
         (on b8 b15)
         (clear b32)
         (on-table b13)
         (on b32 b46)
         (on b46 b18)
         (on b18 b20)
         (on b20 b38)
         (on b38 b55)
         (on b55 b43)
         (on b43 b10)
         (on b10 b34)
         (on b34 b1)
         (on b1 b50)
         (on b50 b36)
         (on b36 b21)
         (on b21 b13)
         (clear b59)
         (on-table b11)
         (on b59 b17)
         (on b17 b33)
         (on b33 b57)
         (on b57 b11)
         (clear b14)
         (on-table b5)
         (on b14 b5))))