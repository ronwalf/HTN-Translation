(define
 (problem pfile_3_095)
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
           - BLOCK)
 (:init
  (hand-empty arm1)
  (hand-empty arm2)
  (hand-empty arm3)
  (clear b36)
  (on-table b92)
  (on b36 b92)
  (clear b84)
  (on-table b84)
  (clear b32)
  (on-table b53)
  (on b32 b41)
  (on b41 b22)
  (on b22 b60)
  (on b60 b95)
  (on b95 b93)
  (on b93 b34)
  (on b34 b24)
  (on b24 b90)
  (on b90 b19)
  (on b19 b74)
  (on b74 b29)
  (on b29 b3)
  (on b3 b53)
  (clear b48)
  (on-table b48)
  (clear b51)
  (on-table b26)
  (on b51 b26)
  (clear b71)
  (on-table b23)
  (on b71 b4)
  (on b4 b42)
  (on b42 b43)
  (on b43 b70)
  (on b70 b64)
  (on b64 b69)
  (on b69 b85)
  (on b85 b25)
  (on b25 b28)
  (on b28 b78)
  (on b78 b56)
  (on b56 b55)
  (on b55 b14)
  (on b14 b17)
  (on b17 b72)
  (on b72 b46)
  (on b46 b8)
  (on b8 b73)
  (on b73 b75)
  (on b75 b94)
  (on b94 b27)
  (on b27 b89)
  (on b89 b57)
  (on b57 b91)
  (on b91 b77)
  (on b77 b82)
  (on b82 b54)
  (on b54 b62)
  (on b62 b21)
  (on b21 b31)
  (on b31 b15)
  (on b15 b9)
  (on b9 b76)
  (on b76 b45)
  (on b45 b1)
  (on b1 b16)
  (on b16 b13)
  (on b13 b47)
  (on b47 b12)
  (on b12 b58)
  (on b58 b83)
  (on b83 b11)
  (on b11 b23)
  (clear b59)
  (on-table b20)
  (on b59 b52)
  (on b52 b88)
  (on b88 b5)
  (on b5 b50)
  (on b50 b38)
  (on b38 b49)
  (on b49 b2)
  (on b2 b30)
  (on b30 b67)
  (on b67 b63)
  (on b63 b7)
  (on b7 b20)
  (clear b18)
  (on-table b10)
  (on b18 b6)
  (on b6 b35)
  (on b35 b80)
  (on b80 b33)
  (on b33 b39)
  (on b39 b61)
  (on b61 b81)
  (on b81 b65)
  (on b65 b87)
  (on b87 b66)
  (on b66 b86)
  (on b86 b40)
  (on b40 b44)
  (on b44 b37)
  (on b37 b68)
  (on b68 b79)
  (on b79 b10))
 (:goal (and
         (clear b52)
         (on-table b79)
         (on b52 b84)
         (on b84 b51)
         (on b51 b73)
         (on b73 b43)
         (on b43 b20)
         (on b20 b39)
         (on b39 b33)
         (on b33 b79)
         (clear b28)
         (on-table b76)
         (on b28 b88)
         (on b88 b85)
         (on b85 b74)
         (on b74 b12)
         (on b12 b70)
         (on b70 b10)
         (on b10 b59)
         (on b59 b26)
         (on b26 b95)
         (on b95 b4)
         (on b4 b60)
         (on b60 b48)
         (on b48 b13)
         (on b13 b41)
         (on b41 b24)
         (on b24 b77)
         (on b77 b47)
         (on b47 b36)
         (on b36 b38)
         (on b38 b7)
         (on b7 b18)
         (on b18 b76)
         (clear b63)
         (on-table b63)
         (clear b16)
         (on-table b54)
         (on b16 b55)
         (on b55 b86)
         (on b86 b5)
         (on b5 b80)
         (on b80 b91)
         (on b91 b93)
         (on b93 b29)
         (on b29 b82)
         (on b82 b87)
         (on b87 b21)
         (on b21 b34)
         (on b34 b94)
         (on b94 b56)
         (on b56 b1)
         (on b1 b8)
         (on b8 b75)
         (on b75 b37)
         (on b37 b50)
         (on b50 b11)
         (on b11 b35)
         (on b35 b22)
         (on b22 b3)
         (on b3 b49)
         (on b49 b42)
         (on b42 b40)
         (on b40 b19)
         (on b19 b32)
         (on b32 b78)
         (on b78 b23)
         (on b23 b64)
         (on b64 b6)
         (on b6 b54)
         (clear b17)
         (on-table b31)
         (on b17 b68)
         (on b68 b57)
         (on b57 b83)
         (on b83 b31)
         (clear b53)
         (on-table b27)
         (on b53 b89)
         (on b89 b81)
         (on b81 b65)
         (on b65 b90)
         (on b90 b2)
         (on b2 b30)
         (on b30 b44)
         (on b44 b45)
         (on b45 b46)
         (on b46 b27)
         (clear b92)
         (on-table b14)
         (on b92 b71)
         (on b71 b69)
         (on b69 b61)
         (on b61 b66)
         (on b66 b72)
         (on b72 b15)
         (on b15 b62)
         (on b62 b67)
         (on b67 b58)
         (on b58 b9)
         (on b9 b25)
         (on b25 b14))))