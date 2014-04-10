(define
 (problem pfile_100)
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
  (hand-empty)
  (clear b96)
  (on-table b93)
  (on b96 b49)
  (on b49 b76)
  (on b76 b93)
  (clear b57)
  (on-table b72)
  (on b57 b60)
  (on b60 b32)
  (on b32 b3)
  (on b3 b26)
  (on b26 b100)
  (on b100 b50)
  (on b50 b90)
  (on b90 b8)
  (on b8 b94)
  (on b94 b4)
  (on b4 b72)
  (clear b80)
  (on-table b53)
  (on b80 b31)
  (on b31 b51)
  (on b51 b41)
  (on b41 b5)
  (on b5 b97)
  (on b97 b89)
  (on b89 b88)
  (on b88 b21)
  (on b21 b40)
  (on b40 b45)
  (on b45 b6)
  (on b6 b87)
  (on b87 b35)
  (on b35 b98)
  (on b98 b82)
  (on b82 b34)
  (on b34 b64)
  (on b64 b27)
  (on b27 b33)
  (on b33 b38)
  (on b38 b83)
  (on b83 b23)
  (on b23 b74)
  (on b74 b68)
  (on b68 b58)
  (on b58 b46)
  (on b46 b75)
  (on b75 b7)
  (on b7 b85)
  (on b85 b79)
  (on b79 b77)
  (on b77 b24)
  (on b24 b66)
  (on b66 b81)
  (on b81 b13)
  (on b13 b86)
  (on b86 b16)
  (on b16 b48)
  (on b48 b29)
  (on b29 b44)
  (on b44 b92)
  (on b92 b55)
  (on b55 b59)
  (on b59 b99)
  (on b99 b2)
  (on b2 b63)
  (on b63 b39)
  (on b39 b70)
  (on b70 b71)
  (on b71 b69)
  (on b69 b65)
  (on b65 b78)
  (on b78 b91)
  (on b91 b47)
  (on b47 b11)
  (on b11 b73)
  (on b73 b15)
  (on b15 b62)
  (on b62 b25)
  (on b25 b14)
  (on b14 b19)
  (on b19 b20)
  (on b20 b17)
  (on b17 b37)
  (on b37 b53)
  (clear b28)
  (on-table b43)
  (on b28 b12)
  (on b12 b43)
  (clear b56)
  (on-table b36)
  (on b56 b36)
  (clear b18)
  (on-table b10)
  (on b18 b52)
  (on b52 b42)
  (on b42 b9)
  (on b9 b10)
  (clear b22)
  (on-table b1)
  (on b22 b84)
  (on b84 b95)
  (on b95 b30)
  (on b30 b61)
  (on b61 b54)
  (on b54 b67)
  (on b67 b1))
 (:goal (and
         (clear b1)
         (on-table b90)
         (on b1 b55)
         (on b55 b2)
         (on b2 b22)
         (on b22 b73)
         (on b73 b14)
         (on b14 b97)
         (on b97 b76)
         (on b76 b60)
         (on b60 b90)
         (clear b29)
         (on-table b84)
         (on b29 b84)
         (clear b27)
         (on-table b78)
         (on b27 b89)
         (on b89 b31)
         (on b31 b32)
         (on b32 b39)
         (on b39 b48)
         (on b48 b92)
         (on b92 b24)
         (on b24 b87)
         (on b87 b99)
         (on b99 b78)
         (clear b74)
         (on-table b74)
         (clear b36)
         (on-table b72)
         (on b36 b51)
         (on b51 b77)
         (on b77 b61)
         (on b61 b47)
         (on b47 b85)
         (on b85 b59)
         (on b59 b37)
         (on b37 b88)
         (on b88 b4)
         (on b4 b11)
         (on b11 b72)
         (clear b33)
         (on-table b69)
         (on b33 b98)
         (on b98 b21)
         (on b21 b10)
         (on b10 b23)
         (on b23 b70)
         (on b70 b40)
         (on b40 b79)
         (on b79 b17)
         (on b17 b82)
         (on b82 b69)
         (clear b57)
         (on-table b66)
         (on b57 b80)
         (on b80 b71)
         (on b71 b42)
         (on b42 b13)
         (on b13 b65)
         (on b65 b41)
         (on b41 b34)
         (on b34 b44)
         (on b44 b19)
         (on b19 b7)
         (on b7 b25)
         (on b25 b96)
         (on b96 b83)
         (on b83 b28)
         (on b28 b91)
         (on b91 b95)
         (on b95 b54)
         (on b54 b93)
         (on b93 b38)
         (on b38 b86)
         (on b86 b26)
         (on b26 b75)
         (on b75 b20)
         (on b20 b18)
         (on b18 b67)
         (on b67 b66)
         (clear b100)
         (on-table b50)
         (on b100 b53)
         (on b53 b56)
         (on b56 b30)
         (on b30 b50)
         (clear b52)
         (on-table b16)
         (on b52 b16)
         (clear b58)
         (on-table b9)
         (on b58 b49)
         (on b49 b46)
         (on b46 b68)
         (on b68 b94)
         (on b94 b63)
         (on b63 b43)
         (on b43 b12)
         (on b12 b45)
         (on b45 b81)
         (on b81 b8)
         (on b8 b35)
         (on b35 b64)
         (on b64 b15)
         (on b15 b62)
         (on b62 b6)
         (on b6 b3)
         (on b3 b5)
         (on b5 b9))))