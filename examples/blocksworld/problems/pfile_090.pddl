(define
 (problem pfile_090)
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
           b81
           b82
           b83
           b84
           b85
           b86
           b87
           b88
           b89
           b90
           - BLOCK)
 (:init
  (hand-empty)
  (clear b66)
  (on-table b80)
  (on b66 b48)
  (on b48 b31)
  (on b31 b64)
  (on b64 b51)
  (on b51 b89)
  (on b89 b78)
  (on b78 b71)
  (on b71 b18)
  (on b18 b40)
  (on b40 b80)
  (clear b90)
  (on-table b72)
  (on b90 b58)
  (on b58 b37)
  (on b37 b55)
  (on b55 b32)
  (on b32 b44)
  (on b44 b87)
  (on b87 b26)
  (on b26 b27)
  (on b27 b84)
  (on b84 b63)
  (on b63 b16)
  (on b16 b86)
  (on b86 b9)
  (on b9 b72)
  (clear b76)
  (on-table b57)
  (on b76 b82)
  (on b82 b81)
  (on b81 b29)
  (on b29 b14)
  (on b14 b43)
  (on b43 b35)
  (on b35 b39)
  (on b39 b20)
  (on b20 b41)
  (on b41 b70)
  (on b70 b57)
  (clear b34)
  (on-table b34)
  (clear b38)
  (on-table b33)
  (on b38 b7)
  (on b7 b36)
  (on b36 b30)
  (on b30 b77)
  (on b77 b13)
  (on b13 b10)
  (on b10 b2)
  (on b2 b67)
  (on b67 b65)
  (on b65 b5)
  (on b5 b68)
  (on b68 b4)
  (on b4 b52)
  (on b52 b60)
  (on b60 b49)
  (on b49 b79)
  (on b79 b50)
  (on b50 b62)
  (on b62 b19)
  (on b19 b74)
  (on b74 b73)
  (on b73 b24)
  (on b24 b6)
  (on b6 b33)
  (clear b22)
  (on-table b17)
  (on b22 b59)
  (on b59 b21)
  (on b21 b45)
  (on b45 b46)
  (on b46 b17)
  (clear b56)
  (on-table b15)
  (on b56 b23)
  (on b23 b69)
  (on b69 b54)
  (on b54 b53)
  (on b53 b83)
  (on b83 b12)
  (on b12 b8)
  (on b8 b85)
  (on b85 b61)
  (on b61 b42)
  (on b42 b88)
  (on b88 b1)
  (on b1 b75)
  (on b75 b11)
  (on b11 b47)
  (on b47 b3)
  (on b3 b25)
  (on b25 b28)
  (on b28 b15))
 (:goal (and
         (clear b81)
         (on-table b74)
         (on b81 b46)
         (on b46 b31)
         (on b31 b74)
         (clear b50)
         (on-table b71)
         (on b50 b66)
         (on b66 b78)
         (on b78 b71)
         (clear b90)
         (on-table b61)
         (on b90 b30)
         (on b30 b89)
         (on b89 b21)
         (on b21 b82)
         (on b82 b9)
         (on b9 b72)
         (on b72 b64)
         (on b64 b79)
         (on b79 b14)
         (on b14 b7)
         (on b7 b61)
         (clear b77)
         (on-table b53)
         (on b77 b76)
         (on b76 b23)
         (on b23 b18)
         (on b18 b73)
         (on b73 b19)
         (on b19 b49)
         (on b49 b26)
         (on b26 b53)
         (clear b54)
         (on-table b45)
         (on b54 b11)
         (on b11 b57)
         (on b57 b69)
         (on b69 b75)
         (on b75 b36)
         (on b36 b51)
         (on b51 b3)
         (on b3 b63)
         (on b63 b34)
         (on b34 b39)
         (on b39 b38)
         (on b38 b45)
         (clear b10)
         (on-table b35)
         (on b10 b12)
         (on b12 b40)
         (on b40 b58)
         (on b58 b70)
         (on b70 b62)
         (on b62 b84)
         (on b84 b32)
         (on b32 b22)
         (on b22 b41)
         (on b41 b59)
         (on b59 b86)
         (on b86 b2)
         (on b2 b35)
         (clear b4)
         (on-table b29)
         (on b4 b27)
         (on b27 b29)
         (clear b87)
         (on-table b25)
         (on b87 b25)
         (clear b37)
         (on-table b24)
         (on b37 b44)
         (on b44 b15)
         (on b15 b55)
         (on b55 b6)
         (on b6 b48)
         (on b48 b85)
         (on b85 b28)
         (on b28 b17)
         (on b17 b42)
         (on b42 b80)
         (on b80 b43)
         (on b43 b56)
         (on b56 b20)
         (on b20 b24)
         (clear b65)
         (on-table b16)
         (on b65 b13)
         (on b13 b33)
         (on b33 b60)
         (on b60 b16)
         (clear b47)
         (on-table b8)
         (on b47 b5)
         (on b5 b68)
         (on b68 b83)
         (on b83 b67)
         (on b67 b52)
         (on b52 b88)
         (on b88 b8)
         (clear b1)
         (on-table b1))))