(define
 (problem pfile_095)
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
  b85 - BLOCK
  b86 - BLOCK
  b87 - BLOCK
  b88 - BLOCK
  b89 - BLOCK
  b90 - BLOCK
  b91 - BLOCK
  b92 - BLOCK
  b93 - BLOCK
  b94 - BLOCK
  b95 - BLOCK)
 (:init
  (armempty)
  (clear b29)
  (on-table b92)
  (on b29 b84)
  (on b84 b6)
  (on b6 b67)
  (on b67 b92)
  (clear b42)
  (on-table b77)
  (on b42 b70)
  (on b70 b63)
  (on b63 b3)
  (on b3 b18)
  (on b18 b31)
  (on b31 b66)
  (on b66 b72)
  (on b72 b23)
  (on b23 b9)
  (on b9 b7)
  (on b7 b4)
  (on b4 b17)
  (on b17 b10)
  (on b10 b51)
  (on b51 b61)
  (on b61 b64)
  (on b64 b35)
  (on b35 b80)
  (on b80 b58)
  (on b58 b90)
  (on b90 b77)
  (clear b1)
  (on-table b59)
  (on b1 b37)
  (on b37 b88)
  (on b88 b22)
  (on b22 b5)
  (on b5 b26)
  (on b26 b28)
  (on b28 b54)
  (on b54 b69)
  (on b69 b16)
  (on b16 b59)
  (clear b81)
  (on-table b55)
  (on b81 b8)
  (on b8 b71)
  (on b71 b56)
  (on b56 b40)
  (on b40 b74)
  (on b74 b89)
  (on b89 b25)
  (on b25 b39)
  (on b39 b24)
  (on b24 b55)
  (clear b86)
  (on-table b44)
  (on b86 b68)
  (on b68 b57)
  (on b57 b79)
  (on b79 b46)
  (on b46 b94)
  (on b94 b53)
  (on b53 b44)
  (clear b19)
  (on-table b41)
  (on b19 b62)
  (on b62 b13)
  (on b13 b95)
  (on b95 b2)
  (on b2 b48)
  (on b48 b30)
  (on b30 b27)
  (on b27 b82)
  (on b82 b41)
  (clear b85)
  (on-table b33)
  (on b85 b45)
  (on b45 b87)
  (on b87 b43)
  (on b43 b33)
  (clear b78)
  (on-table b20)
  (on b78 b32)
  (on b32 b76)
  (on b76 b11)
  (on b11 b73)
  (on b73 b50)
  (on b50 b38)
  (on b38 b21)
  (on b21 b47)
  (on b47 b91)
  (on b91 b34)
  (on b34 b60)
  (on b60 b83)
  (on b83 b15)
  (on b15 b93)
  (on b93 b49)
  (on b49 b75)
  (on b75 b20)
  (clear b14)
  (on-table b12)
  (on b14 b52)
  (on b52 b65)
  (on b65 b36)
  (on b36 b12))
 (:goal
  (and
   (clear b95)
   (on-table b86)
   (on b95 b52)
   (on b52 b53)
   (on b53 b39)
   (on b39 b7)
   (on b7 b17)
   (on b17 b80)
   (on b80 b6)
   (on b6 b93)
   (on b93 b37)
   (on b37 b46)
   (on b46 b32)
   (on b32 b57)
   (on b57 b89)
   (on b89 b23)
   (on b23 b91)
   (on b91 b65)
   (on b65 b54)
   (on b54 b62)
   (on b62 b24)
   (on b24 b78)
   (on b78 b21)
   (on b21 b64)
   (on b64 b67)
   (on b67 b66)
   (on b66 b86)
   (clear b30)
   (on-table b60)
   (on b30 b50)
   (on b50 b58)
   (on b58 b9)
   (on b9 b88)
   (on b88 b28)
   (on b28 b25)
   (on b25 b33)
   (on b33 b69)
   (on b69 b73)
   (on b73 b55)
   (on b55 b71)
   (on b71 b3)
   (on b3 b49)
   (on b49 b81)
   (on b81 b76)
   (on b76 b13)
   (on b13 b41)
   (on b41 b51)
   (on b51 b83)
   (on b83 b44)
   (on b44 b60)
   (clear b94)
   (on-table b48)
   (on b94 b29)
   (on b29 b87)
   (on b87 b38)
   (on b38 b48)
   (clear b16)
   (on-table b45)
   (on b16 b42)
   (on b42 b12)
   (on b12 b27)
   (on b27 b85)
   (on b85 b63)
   (on b63 b43)
   (on b43 b59)
   (on b59 b22)
   (on b22 b31)
   (on b31 b92)
   (on b92 b70)
   (on b70 b5)
   (on b5 b18)
   (on b18 b35)
   (on b35 b72)
   (on b72 b20)
   (on b20 b45)
   (clear b84)
   (on-table b34)
   (on b84 b61)
   (on b61 b79)
   (on b79 b40)
   (on b40 b19)
   (on b19 b1)
   (on b1 b10)
   (on b10 b68)
   (on b68 b34)
   (clear b74)
   (on-table b26)
   (on b74 b75)
   (on b75 b2)
   (on b2 b56)
   (on b56 b47)
   (on b47 b26)
   (clear b14)
   (on-table b14)
   (clear b82)
   (on-table b4)
   (on b82 b11)
   (on b11 b15)
   (on b15 b77)
   (on b77 b90)
   (on b90 b8)
   (on b8 b36)
   (on b36 b4))))