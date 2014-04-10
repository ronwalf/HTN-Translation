(define
 (problem pfile_080)
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
           - BLOCK)
 (:init
  (hand-empty)
  (clear b62)
  (on-table b69)
  (on b62 b16)
  (on b16 b5)
  (on b5 b34)
  (on b34 b9)
  (on b9 b78)
  (on b78 b3)
  (on b3 b37)
  (on b37 b69)
  (clear b4)
  (on-table b44)
  (on b4 b66)
  (on b66 b64)
  (on b64 b8)
  (on b8 b20)
  (on b20 b55)
  (on b55 b50)
  (on b50 b65)
  (on b65 b29)
  (on b29 b44)
  (clear b74)
  (on-table b41)
  (on b74 b73)
  (on b73 b67)
  (on b67 b15)
  (on b15 b70)
  (on b70 b68)
  (on b68 b38)
  (on b38 b35)
  (on b35 b77)
  (on b77 b49)
  (on b49 b14)
  (on b14 b24)
  (on b24 b31)
  (on b31 b10)
  (on b10 b11)
  (on b11 b26)
  (on b26 b54)
  (on b54 b41)
  (clear b71)
  (on-table b39)
  (on b71 b52)
  (on b52 b1)
  (on b1 b39)
  (clear b57)
  (on-table b36)
  (on b57 b47)
  (on b47 b42)
  (on b42 b75)
  (on b75 b19)
  (on b19 b2)
  (on b2 b53)
  (on b53 b13)
  (on b13 b27)
  (on b27 b80)
  (on b80 b12)
  (on b12 b79)
  (on b79 b32)
  (on b32 b36)
  (clear b33)
  (on-table b33)
  (clear b30)
  (on-table b25)
  (on b30 b59)
  (on b59 b51)
  (on b51 b23)
  (on b23 b58)
  (on b58 b76)
  (on b76 b46)
  (on b46 b22)
  (on b22 b25)
  (clear b45)
  (on-table b7)
  (on b45 b7)
  (clear b28)
  (on-table b6)
  (on b28 b43)
  (on b43 b60)
  (on b60 b40)
  (on b40 b18)
  (on b18 b17)
  (on b17 b21)
  (on b21 b63)
  (on b63 b48)
  (on b48 b72)
  (on b72 b56)
  (on b56 b61)
  (on b61 b6))
 (:goal (and
         (clear b4)
         (on-table b65)
         (on b4 b71)
         (on b71 b65)
         (clear b21)
         (on-table b62)
         (on b21 b14)
         (on b14 b22)
         (on b22 b23)
         (on b23 b46)
         (on b46 b2)
         (on b2 b68)
         (on b68 b3)
         (on b3 b63)
         (on b63 b31)
         (on b31 b6)
         (on b6 b17)
         (on b17 b47)
         (on b47 b20)
         (on b20 b24)
         (on b24 b62)
         (clear b36)
         (on-table b56)
         (on b36 b38)
         (on b38 b75)
         (on b75 b39)
         (on b39 b40)
         (on b40 b56)
         (clear b52)
         (on-table b51)
         (on b52 b64)
         (on b64 b80)
         (on b80 b51)
         (clear b45)
         (on-table b45)
         (clear b5)
         (on-table b42)
         (on b5 b48)
         (on b48 b9)
         (on b9 b59)
         (on b59 b42)
         (clear b30)
         (on-table b10)
         (on b30 b66)
         (on b66 b44)
         (on b44 b43)
         (on b43 b58)
         (on b58 b49)
         (on b49 b15)
         (on b15 b69)
         (on b69 b27)
         (on b27 b55)
         (on b55 b79)
         (on b79 b10)
         (clear b12)
         (on-table b8)
         (on b12 b67)
         (on b67 b25)
         (on b25 b13)
         (on b13 b34)
         (on b34 b76)
         (on b76 b33)
         (on b33 b78)
         (on b78 b28)
         (on b28 b60)
         (on b60 b35)
         (on b35 b53)
         (on b53 b77)
         (on b77 b70)
         (on b70 b19)
         (on b19 b16)
         (on b16 b50)
         (on b50 b37)
         (on b37 b41)
         (on b41 b11)
         (on b11 b7)
         (on b7 b72)
         (on b72 b26)
         (on b26 b73)
         (on b73 b54)
         (on b54 b18)
         (on b18 b32)
         (on b32 b61)
         (on b61 b57)
         (on b57 b74)
         (on b74 b29)
         (on b29 b8)
         (clear b1)
         (on-table b1))))