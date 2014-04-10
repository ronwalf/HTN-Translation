(define
 (problem pfile_085)
 (:domain blocks)
 (:objects
  b1 - BLOCK
  b2 - BLOCK
  b3 - BLOCK
  b4 - BLOCK
  b5 - BLOCK
  b6 - BLOCK
  b7 - BLOCK
  b8 - BLOCK
  b9 - BLOCK
  b10 - BLOCK
  b11 - BLOCK
  b12 - BLOCK
  b13 - BLOCK
  b14 - BLOCK
  b15 - BLOCK
  b16 - BLOCK
  b17 - BLOCK
  b18 - BLOCK
  b19 - BLOCK
  b20 - BLOCK
  b21 - BLOCK
  b22 - BLOCK
  b23 - BLOCK
  b24 - BLOCK
  b25 - BLOCK
  b26 - BLOCK
  b27 - BLOCK
  b28 - BLOCK
  b29 - BLOCK
  b30 - BLOCK
  b31 - BLOCK
  b32 - BLOCK
  b33 - BLOCK
  b34 - BLOCK
  b35 - BLOCK
  b36 - BLOCK
  b37 - BLOCK
  b38 - BLOCK
  b39 - BLOCK
  b40 - BLOCK
  b41 - BLOCK
  b42 - BLOCK
  b43 - BLOCK
  b44 - BLOCK
  b45 - BLOCK
  b46 - BLOCK
  b47 - BLOCK
  b48 - BLOCK
  b49 - BLOCK
  b50 - BLOCK
  b51 - BLOCK
  b52 - BLOCK
  b53 - BLOCK
  b54 - BLOCK
  b55 - BLOCK
  b56 - BLOCK
  b57 - BLOCK
  b58 - BLOCK
  b59 - BLOCK
  b60 - BLOCK
  b61 - BLOCK
  b62 - BLOCK
  b63 - BLOCK
  b64 - BLOCK
  b65 - BLOCK
  b66 - BLOCK
  b67 - BLOCK
  b68 - BLOCK
  b69 - BLOCK
  b70 - BLOCK
  b71 - BLOCK
  b72 - BLOCK
  b73 - BLOCK
  b74 - BLOCK
  b75 - BLOCK
  b76 - BLOCK
  b77 - BLOCK
  b78 - BLOCK
  b79 - BLOCK
  b80 - BLOCK
  b81 - BLOCK
  b82 - BLOCK
  b83 - BLOCK
  b84 - BLOCK
  b85 - BLOCK)
 (:init
  (armempty)
  (clear b60)
  (on-table b83)
  (on b60 b79)
  (on b79 b39)
  (on b39 b24)
  (on b24 b10)
  (on b10 b83)
  (clear b53)
  (on-table b70)
  (on b53 b31)
  (on b31 b22)
  (on b22 b76)
  (on b76 b17)
  (on b17 b33)
  (on b33 b70)
  (clear b57)
  (on-table b66)
  (on b57 b67)
  (on b67 b45)
  (on b45 b32)
  (on b32 b62)
  (on b62 b42)
  (on b42 b20)
  (on b20 b34)
  (on b34 b66)
  (clear b16)
  (on-table b56)
  (on b16 b68)
  (on b68 b21)
  (on b21 b56)
  (clear b64)
  (on-table b52)
  (on b64 b74)
  (on b74 b15)
  (on b15 b58)
  (on b58 b37)
  (on b37 b52)
  (clear b75)
  (on-table b49)
  (on b75 b23)
  (on b23 b4)
  (on b4 b49)
  (clear b61)
  (on-table b48)
  (on b61 b48)
  (clear b78)
  (on-table b46)
  (on b78 b44)
  (on b44 b29)
  (on b29 b1)
  (on b1 b6)
  (on b6 b2)
  (on b2 b46)
  (clear b43)
  (on-table b43)
  (clear b11)
  (on-table b30)
  (on b11 b81)
  (on b81 b3)
  (on b3 b63)
  (on b63 b38)
  (on b38 b85)
  (on b85 b27)
  (on b27 b40)
  (on b40 b18)
  (on b18 b12)
  (on b12 b84)
  (on b84 b77)
  (on b77 b47)
  (on b47 b9)
  (on b9 b41)
  (on b41 b30)
  (clear b26)
  (on-table b26)
  (clear b54)
  (on-table b14)
  (on b54 b25)
  (on b25 b65)
  (on b65 b13)
  (on b13 b51)
  (on b51 b35)
  (on b35 b59)
  (on b59 b8)
  (on b8 b69)
  (on b69 b50)
  (on b50 b80)
  (on b80 b72)
  (on b72 b19)
  (on b19 b55)
  (on b55 b36)
  (on b36 b73)
  (on b73 b28)
  (on b28 b71)
  (on b71 b5)
  (on b5 b14)
  (clear b82)
  (on-table b7)
  (on b82 b7))
 (:goal
  (and
   (clear b71)
   (on-table b69)
   (on b71 b66)
   (on b66 b25)
   (on b25 b34)
   (on b34 b52)
   (on b52 b69)
   (clear b78)
   (on-table b65)
   (on b78 b64)
   (on b64 b83)
   (on b83 b41)
   (on b41 b42)
   (on b42 b24)
   (on b24 b13)
   (on b13 b51)
   (on b51 b1)
   (on b1 b74)
   (on b74 b2)
   (on b2 b67)
   (on b67 b4)
   (on b4 b43)
   (on b43 b46)
   (on b46 b84)
   (on b84 b65)
   (clear b26)
   (on-table b50)
   (on b26 b72)
   (on b72 b29)
   (on b29 b56)
   (on b56 b57)
   (on b57 b79)
   (on b79 b16)
   (on b16 b15)
   (on b15 b81)
   (on b81 b10)
   (on b10 b53)
   (on b53 b63)
   (on b63 b45)
   (on b45 b76)
   (on b76 b40)
   (on b40 b82)
   (on b82 b38)
   (on b38 b35)
   (on b35 b8)
   (on b8 b7)
   (on b7 b6)
   (on b6 b85)
   (on b85 b11)
   (on b11 b31)
   (on b31 b33)
   (on b33 b39)
   (on b39 b80)
   (on b80 b47)
   (on b47 b17)
   (on b17 b50)
   (clear b18)
   (on-table b49)
   (on b18 b14)
   (on b14 b20)
   (on b20 b59)
   (on b59 b49)
   (clear b55)
   (on-table b36)
   (on b55 b36)
   (clear b70)
   (on-table b27)
   (on b70 b77)
   (on b77 b62)
   (on b62 b19)
   (on b19 b30)
   (on b30 b61)
   (on b61 b48)
   (on b48 b21)
   (on b21 b27)
   (clear b44)
   (on-table b23)
   (on b44 b54)
   (on b54 b37)
   (on b37 b60)
   (on b60 b22)
   (on b22 b32)
   (on b32 b58)
   (on b58 b12)
   (on b12 b28)
   (on b28 b3)
   (on b3 b5)
   (on b5 b68)
   (on b68 b73)
   (on b73 b9)
   (on b9 b75)
   (on b75 b23))))