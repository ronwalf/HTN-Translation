(define
 (problem pfile_3_085)
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
           - BLOCK)
 (:init
  (hand-empty arm1)
  (hand-empty arm2)
  (hand-empty arm3)
  (clear b7)
  (on-table b85)
  (on b7 b76)
  (on b76 b42)
  (on b42 b6)
  (on b6 b55)
  (on b55 b60)
  (on b60 b12)
  (on b12 b30)
  (on b30 b29)
  (on b29 b46)
  (on b46 b72)
  (on b72 b85)
  (clear b34)
  (on-table b81)
  (on b34 b15)
  (on b15 b81)
  (clear b47)
  (on-table b70)
  (on b47 b70)
  (clear b32)
  (on-table b68)
  (on b32 b18)
  (on b18 b50)
  (on b50 b28)
  (on b28 b68)
  (clear b13)
  (on-table b66)
  (on b13 b38)
  (on b38 b36)
  (on b36 b74)
  (on b74 b19)
  (on b19 b73)
  (on b73 b2)
  (on b2 b5)
  (on b5 b8)
  (on b8 b20)
  (on b20 b57)
  (on b57 b41)
  (on b41 b79)
  (on b79 b66)
  (clear b53)
  (on-table b63)
  (on b53 b11)
  (on b11 b4)
  (on b4 b78)
  (on b78 b17)
  (on b17 b22)
  (on b22 b61)
  (on b61 b82)
  (on b82 b77)
  (on b77 b63)
  (clear b21)
  (on-table b58)
  (on b21 b58)
  (clear b37)
  (on-table b54)
  (on b37 b69)
  (on b69 b10)
  (on b10 b83)
  (on b83 b54)
  (clear b49)
  (on-table b49)
  (clear b1)
  (on-table b45)
  (on b1 b14)
  (on b14 b23)
  (on b23 b45)
  (clear b67)
  (on-table b40)
  (on b67 b80)
  (on b80 b65)
  (on b65 b40)
  (clear b31)
  (on-table b31)
  (clear b59)
  (on-table b24)
  (on b59 b9)
  (on b9 b27)
  (on b27 b43)
  (on b43 b48)
  (on b48 b56)
  (on b56 b62)
  (on b62 b51)
  (on b51 b3)
  (on b3 b39)
  (on b39 b64)
  (on b64 b33)
  (on b33 b84)
  (on b84 b26)
  (on b26 b44)
  (on b44 b25)
  (on b25 b24)
  (clear b75)
  (on-table b16)
  (on b75 b35)
  (on b35 b71)
  (on b71 b52)
  (on b52 b16))
 (:goal (and
         (clear b43)
         (on-table b85)
         (on b43 b39)
         (on b39 b71)
         (on b71 b48)
         (on b48 b69)
         (on b69 b74)
         (on b74 b70)
         (on b70 b79)
         (on b79 b4)
         (on b4 b51)
         (on b51 b34)
         (on b34 b78)
         (on b78 b53)
         (on b53 b28)
         (on b28 b84)
         (on b84 b1)
         (on b1 b81)
         (on b81 b18)
         (on b18 b29)
         (on b29 b77)
         (on b77 b85)
         (clear b14)
         (on-table b83)
         (on b14 b16)
         (on b16 b6)
         (on b6 b15)
         (on b15 b22)
         (on b22 b65)
         (on b65 b12)
         (on b12 b83)
         (clear b45)
         (on-table b60)
         (on b45 b59)
         (on b59 b10)
         (on b10 b44)
         (on b44 b17)
         (on b17 b52)
         (on b52 b35)
         (on b35 b33)
         (on b33 b50)
         (on b50 b26)
         (on b26 b46)
         (on b46 b8)
         (on b8 b11)
         (on b11 b58)
         (on b58 b60)
         (clear b42)
         (on-table b42)
         (clear b66)
         (on-table b38)
         (on b66 b37)
         (on b37 b21)
         (on b21 b7)
         (on b7 b47)
         (on b47 b23)
         (on b23 b76)
         (on b76 b3)
         (on b3 b64)
         (on b64 b25)
         (on b25 b54)
         (on b54 b56)
         (on b56 b31)
         (on b31 b73)
         (on b73 b82)
         (on b82 b72)
         (on b72 b9)
         (on b9 b68)
         (on b68 b55)
         (on b55 b57)
         (on b57 b62)
         (on b62 b75)
         (on b75 b41)
         (on b41 b36)
         (on b36 b61)
         (on b61 b27)
         (on b27 b49)
         (on b49 b24)
         (on b24 b32)
         (on b32 b5)
         (on b5 b2)
         (on b2 b20)
         (on b20 b40)
         (on b40 b13)
         (on b13 b19)
         (on b19 b63)
         (on b63 b67)
         (on b67 b30)
         (on b30 b80)
         (on b80 b38))))