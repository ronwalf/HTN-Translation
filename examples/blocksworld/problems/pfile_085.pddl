(define
 (problem pfile_085)
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
           - BLOCK)
 (:init
  (hand-empty)
  (clear b40)
  (on-table b67)
  (on b40 b67)
  (clear b2)
  (on-table b64)
  (on b2 b9)
  (on b9 b16)
  (on b16 b55)
  (on b55 b52)
  (on b52 b60)
  (on b60 b75)
  (on b75 b24)
  (on b24 b19)
  (on b19 b66)
  (on b66 b8)
  (on b8 b44)
  (on b44 b46)
  (on b46 b64)
  (clear b82)
  (on-table b58)
  (on b82 b34)
  (on b34 b81)
  (on b81 b31)
  (on b31 b85)
  (on b85 b6)
  (on b6 b54)
  (on b54 b49)
  (on b49 b56)
  (on b56 b18)
  (on b18 b72)
  (on b72 b76)
  (on b76 b23)
  (on b23 b73)
  (on b73 b41)
  (on b41 b84)
  (on b84 b50)
  (on b50 b3)
  (on b3 b17)
  (on b17 b65)
  (on b65 b61)
  (on b61 b37)
  (on b37 b39)
  (on b39 b30)
  (on b30 b26)
  (on b26 b58)
  (clear b47)
  (on-table b53)
  (on b47 b25)
  (on b25 b5)
  (on b5 b77)
  (on b77 b45)
  (on b45 b53)
  (clear b33)
  (on-table b48)
  (on b33 b51)
  (on b51 b80)
  (on b80 b71)
  (on b71 b35)
  (on b35 b10)
  (on b10 b63)
  (on b63 b62)
  (on b62 b21)
  (on b21 b13)
  (on b13 b48)
  (clear b83)
  (on-table b20)
  (on b83 b20)
  (clear b42)
  (on-table b1)
  (on b42 b59)
  (on b59 b29)
  (on b29 b14)
  (on b14 b27)
  (on b27 b70)
  (on b70 b4)
  (on b4 b22)
  (on b22 b79)
  (on b79 b74)
  (on b74 b11)
  (on b11 b43)
  (on b43 b28)
  (on b28 b36)
  (on b36 b38)
  (on b38 b7)
  (on b7 b78)
  (on b78 b12)
  (on b12 b57)
  (on b57 b68)
  (on b68 b15)
  (on b15 b32)
  (on b32 b69)
  (on b69 b1))
 (:goal (and
         (clear b48)
         (on-table b83)
         (on b48 b42)
         (on b42 b10)
         (on b10 b47)
         (on b47 b15)
         (on b15 b73)
         (on b73 b84)
         (on b84 b6)
         (on b6 b69)
         (on b69 b52)
         (on b52 b83)
         (clear b39)
         (on-table b80)
         (on b39 b11)
         (on b11 b2)
         (on b2 b77)
         (on b77 b67)
         (on b67 b80)
         (clear b62)
         (on-table b62)
         (clear b70)
         (on-table b59)
         (on b70 b60)
         (on b60 b78)
         (on b78 b68)
         (on b68 b85)
         (on b85 b20)
         (on b20 b30)
         (on b30 b43)
         (on b43 b13)
         (on b13 b59)
         (clear b7)
         (on-table b58)
         (on b7 b26)
         (on b26 b56)
         (on b56 b61)
         (on b61 b18)
         (on b18 b65)
         (on b65 b28)
         (on b28 b57)
         (on b57 b24)
         (on b24 b34)
         (on b34 b72)
         (on b72 b58)
         (clear b54)
         (on-table b54)
         (clear b44)
         (on-table b50)
         (on b44 b9)
         (on b9 b79)
         (on b79 b14)
         (on b14 b46)
         (on b46 b37)
         (on b37 b25)
         (on b25 b50)
         (clear b5)
         (on-table b38)
         (on b5 b31)
         (on b31 b27)
         (on b27 b36)
         (on b36 b12)
         (on b12 b4)
         (on b4 b53)
         (on b53 b81)
         (on b81 b51)
         (on b51 b38)
         (clear b76)
         (on-table b21)
         (on b76 b23)
         (on b23 b64)
         (on b64 b32)
         (on b32 b75)
         (on b75 b33)
         (on b33 b55)
         (on b55 b74)
         (on b74 b41)
         (on b41 b16)
         (on b16 b35)
         (on b35 b82)
         (on b82 b66)
         (on b66 b71)
         (on b71 b63)
         (on b63 b45)
         (on b45 b1)
         (on b1 b8)
         (on b8 b21)
         (clear b40)
         (on-table b19)
         (on b40 b49)
         (on b49 b29)
         (on b29 b22)
         (on b22 b19)
         (clear b17)
         (on-table b3)
         (on b17 b3))))