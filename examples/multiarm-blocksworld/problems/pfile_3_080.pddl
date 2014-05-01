(define
 (problem pfile_3_080)
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
           b61
           b62
           b63
           b64
           b65
           b66
           b67
           b68
           b69
           b70
           b71
           b72
           b73
           b74
           b75
           b76
           b77
           b78
           b79
           b80
           - BLOCK)
 (:init
  (hand-empty arm1)
  (hand-empty arm2)
  (hand-empty arm3)
  (clear b16)
  (on-table b79)
  (on b16 b79)
  (clear b74)
  (on-table b76)
  (on b74 b23)
  (on b23 b34)
  (on b34 b24)
  (on b24 b67)
  (on b67 b53)
  (on b53 b51)
  (on b51 b56)
  (on b56 b60)
  (on b60 b64)
  (on b64 b12)
  (on b12 b13)
  (on b13 b76)
  (clear b55)
  (on-table b58)
  (on b55 b1)
  (on b1 b63)
  (on b63 b39)
  (on b39 b32)
  (on b32 b36)
  (on b36 b31)
  (on b31 b40)
  (on b40 b8)
  (on b8 b62)
  (on b62 b69)
  (on b69 b37)
  (on b37 b75)
  (on b75 b9)
  (on b9 b22)
  (on b22 b52)
  (on b52 b58)
  (clear b47)
  (on-table b54)
  (on b47 b30)
  (on b30 b57)
  (on b57 b61)
  (on b61 b45)
  (on b45 b21)
  (on b21 b28)
  (on b28 b41)
  (on b41 b50)
  (on b50 b77)
  (on b77 b4)
  (on b4 b20)
  (on b20 b54)
  (clear b29)
  (on-table b48)
  (on b29 b70)
  (on b70 b48)
  (clear b18)
  (on-table b26)
  (on b18 b25)
  (on b25 b11)
  (on b11 b26)
  (clear b10)
  (on-table b10)
  (clear b2)
  (on-table b7)
  (on b2 b73)
  (on b73 b72)
  (on b72 b14)
  (on b14 b19)
  (on b19 b78)
  (on b78 b6)
  (on b6 b80)
  (on b80 b59)
  (on b59 b3)
  (on b3 b65)
  (on b65 b68)
  (on b68 b38)
  (on b38 b71)
  (on b71 b17)
  (on b17 b15)
  (on b15 b66)
  (on b66 b27)
  (on b27 b33)
  (on b33 b49)
  (on b49 b35)
  (on b35 b43)
  (on b43 b5)
  (on b5 b46)
  (on b46 b44)
  (on b44 b42)
  (on b42 b7))
 (:goal (and
         (clear b44)
         (on-table b58)
         (on b44 b71)
         (on b71 b41)
         (on b41 b8)
         (on b8 b22)
         (on b22 b69)
         (on b69 b58)
         (clear b77)
         (on-table b57)
         (on b77 b5)
         (on b5 b66)
         (on b66 b56)
         (on b56 b25)
         (on b25 b19)
         (on b19 b49)
         (on b49 b30)
         (on b30 b80)
         (on b80 b61)
         (on b61 b31)
         (on b31 b7)
         (on b7 b36)
         (on b36 b52)
         (on b52 b65)
         (on b65 b57)
         (clear b23)
         (on-table b50)
         (on b23 b79)
         (on b79 b48)
         (on b48 b70)
         (on b70 b50)
         (clear b67)
         (on-table b45)
         (on b67 b45)
         (clear b24)
         (on-table b34)
         (on b24 b78)
         (on b78 b34)
         (clear b18)
         (on-table b33)
         (on b18 b13)
         (on b13 b39)
         (on b39 b72)
         (on b72 b21)
         (on b21 b17)
         (on b17 b68)
         (on b68 b40)
         (on b40 b75)
         (on b75 b37)
         (on b37 b38)
         (on b38 b46)
         (on b46 b63)
         (on b63 b16)
         (on b16 b15)
         (on b15 b4)
         (on b4 b53)
         (on b53 b64)
         (on b64 b76)
         (on b76 b27)
         (on b27 b6)
         (on b6 b3)
         (on b3 b10)
         (on b10 b60)
         (on b60 b51)
         (on b51 b74)
         (on b74 b54)
         (on b54 b47)
         (on b47 b33)
         (clear b43)
         (on-table b26)
         (on b43 b29)
         (on b29 b59)
         (on b59 b9)
         (on b9 b26)
         (clear b28)
         (on-table b14)
         (on b28 b35)
         (on b35 b73)
         (on b73 b32)
         (on b32 b42)
         (on b42 b1)
         (on b1 b20)
         (on b20 b55)
         (on b55 b14)
         (clear b62)
         (on-table b12)
         (on b62 b12)
         (clear b11)
         (on-table b2)
         (on b11 b2))))