(define
 (problem pfile_3_100)
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
           b91
           b92
           b93
           b94
           b95
           b96
           b97
           b98
           b99
           b100
           - BLOCK)
 (:init
  (hand-empty arm1)
  (hand-empty arm2)
  (hand-empty arm3)
  (clear b95)
  (on-table b77)
  (on b95 b86)
  (on b86 b16)
  (on b16 b83)
  (on b83 b77)
  (clear b69)
  (on-table b70)
  (on b69 b46)
  (on b46 b25)
  (on b25 b10)
  (on b10 b93)
  (on b93 b7)
  (on b7 b20)
  (on b20 b19)
  (on b19 b42)
  (on b42 b88)
  (on b88 b31)
  (on b31 b2)
  (on b2 b89)
  (on b89 b22)
  (on b22 b57)
  (on b57 b5)
  (on b5 b30)
  (on b30 b29)
  (on b29 b70)
  (clear b50)
  (on-table b53)
  (on b50 b12)
  (on b12 b45)
  (on b45 b8)
  (on b8 b53)
  (clear b67)
  (on-table b28)
  (on b67 b38)
  (on b38 b40)
  (on b40 b85)
  (on b85 b72)
  (on b72 b27)
  (on b27 b14)
  (on b14 b73)
  (on b73 b18)
  (on b18 b74)
  (on b74 b97)
  (on b97 b21)
  (on b21 b36)
  (on b36 b28)
  (clear b26)
  (on-table b17)
  (on b26 b78)
  (on b78 b90)
  (on b90 b98)
  (on b98 b81)
  (on b81 b48)
  (on b48 b6)
  (on b6 b87)
  (on b87 b44)
  (on b44 b56)
  (on b56 b66)
  (on b66 b43)
  (on b43 b23)
  (on b23 b75)
  (on b75 b91)
  (on b91 b58)
  (on b58 b32)
  (on b32 b47)
  (on b47 b62)
  (on b62 b76)
  (on b76 b35)
  (on b35 b63)
  (on b63 b64)
  (on b64 b13)
  (on b13 b65)
  (on b65 b60)
  (on b60 b49)
  (on b49 b71)
  (on b71 b24)
  (on b24 b61)
  (on b61 b94)
  (on b94 b84)
  (on b84 b99)
  (on b99 b39)
  (on b39 b55)
  (on b55 b79)
  (on b79 b33)
  (on b33 b100)
  (on b100 b17)
  (clear b51)
  (on-table b9)
  (on b51 b52)
  (on b52 b15)
  (on b15 b34)
  (on b34 b82)
  (on b82 b11)
  (on b11 b59)
  (on b59 b54)
  (on b54 b9)
  (clear b41)
  (on-table b3)
  (on b41 b37)
  (on b37 b92)
  (on b92 b96)
  (on b96 b3)
  (clear b68)
  (on-table b1)
  (on b68 b80)
  (on b80 b4)
  (on b4 b1))
 (:goal (and
         (clear b60)
         (on-table b91)
         (on b60 b91)
         (clear b88)
         (on-table b88)
         (clear b18)
         (on-table b83)
         (on b18 b7)
         (on b7 b83)
         (clear b73)
         (on-table b76)
         (on b73 b80)
         (on b80 b85)
         (on b85 b29)
         (on b29 b13)
         (on b13 b45)
         (on b45 b72)
         (on b72 b66)
         (on b66 b64)
         (on b64 b90)
         (on b90 b63)
         (on b63 b26)
         (on b26 b30)
         (on b30 b100)
         (on b100 b20)
         (on b20 b76)
         (clear b74)
         (on-table b69)
         (on b74 b24)
         (on b24 b53)
         (on b53 b37)
         (on b37 b49)
         (on b49 b84)
         (on b84 b69)
         (clear b65)
         (on-table b65)
         (clear b94)
         (on-table b59)
         (on b94 b11)
         (on b11 b17)
         (on b17 b51)
         (on b51 b36)
         (on b36 b8)
         (on b8 b96)
         (on b96 b98)
         (on b98 b75)
         (on b75 b93)
         (on b93 b38)
         (on b38 b67)
         (on b67 b19)
         (on b19 b56)
         (on b56 b59)
         (clear b4)
         (on-table b50)
         (on b4 b81)
         (on b81 b15)
         (on b15 b31)
         (on b31 b50)
         (clear b23)
         (on-table b48)
         (on b23 b57)
         (on b57 b58)
         (on b58 b3)
         (on b3 b2)
         (on b2 b1)
         (on b1 b32)
         (on b32 b61)
         (on b61 b48)
         (clear b87)
         (on-table b44)
         (on b87 b28)
         (on b28 b92)
         (on b92 b5)
         (on b5 b46)
         (on b46 b77)
         (on b77 b25)
         (on b25 b14)
         (on b14 b9)
         (on b9 b68)
         (on b68 b6)
         (on b6 b44)
         (clear b71)
         (on-table b40)
         (on b71 b41)
         (on b41 b55)
         (on b55 b42)
         (on b42 b97)
         (on b97 b22)
         (on b22 b79)
         (on b79 b95)
         (on b95 b70)
         (on b70 b12)
         (on b12 b40)
         (clear b39)
         (on-table b21)
         (on b39 b78)
         (on b78 b43)
         (on b43 b21)
         (clear b47)
         (on-table b16)
         (on b47 b16)
         (clear b52)
         (on-table b10)
         (on b52 b33)
         (on b33 b99)
         (on b99 b82)
         (on b82 b89)
         (on b89 b54)
         (on b54 b62)
         (on b62 b34)
         (on b34 b27)
         (on b27 b35)
         (on b35 b86)
         (on b86 b10))))