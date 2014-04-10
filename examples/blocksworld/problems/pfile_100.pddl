(define
 (problem pfile_100)
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
  b95 - BLOCK
  b96 - BLOCK
  b97 - BLOCK
  b98 - BLOCK
  b99 - BLOCK
  b100 - BLOCK)
 (:init
  (armempty)
  (clear b69)
  (on-table b96)
  (on b69 b67)
  (on b67 b29)
  (on b29 b83)
  (on b83 b77)
  (on b77 b96)
  (clear b92)
  (on-table b93)
  (on b92 b93)
  (clear b88)
  (on-table b88)
  (clear b78)
  (on-table b73)
  (on b78 b15)
  (on b15 b79)
  (on b79 b73)
  (clear b12)
  (on-table b70)
  (on b12 b23)
  (on b23 b4)
  (on b4 b80)
  (on b80 b30)
  (on b30 b76)
  (on b76 b26)
  (on b26 b52)
  (on b52 b89)
  (on b89 b94)
  (on b94 b84)
  (on b84 b18)
  (on b18 b3)
  (on b3 b49)
  (on b49 b70)
  (clear b50)
  (on-table b57)
  (on b50 b11)
  (on b11 b56)
  (on b56 b27)
  (on b27 b65)
  (on b65 b31)
  (on b31 b71)
  (on b71 b9)
  (on b9 b16)
  (on b16 b64)
  (on b64 b28)
  (on b28 b85)
  (on b85 b10)
  (on b10 b19)
  (on b19 b66)
  (on b66 b62)
  (on b62 b95)
  (on b95 b46)
  (on b46 b51)
  (on b51 b98)
  (on b98 b17)
  (on b17 b97)
  (on b97 b45)
  (on b45 b38)
  (on b38 b57)
  (clear b87)
  (on-table b55)
  (on b87 b5)
  (on b5 b37)
  (on b37 b53)
  (on b53 b58)
  (on b58 b55)
  (clear b25)
  (on-table b44)
  (on b25 b82)
  (on b82 b99)
  (on b99 b36)
  (on b36 b32)
  (on b32 b44)
  (clear b39)
  (on-table b41)
  (on b39 b100)
  (on b100 b40)
  (on b40 b90)
  (on b90 b22)
  (on b22 b48)
  (on b48 b86)
  (on b86 b43)
  (on b43 b60)
  (on b60 b2)
  (on b2 b34)
  (on b34 b41)
  (clear b33)
  (on-table b35)
  (on b33 b75)
  (on b75 b81)
  (on b81 b59)
  (on b59 b54)
  (on b54 b13)
  (on b13 b8)
  (on b8 b68)
  (on b68 b14)
  (on b14 b63)
  (on b63 b7)
  (on b7 b20)
  (on b20 b35)
  (clear b72)
  (on-table b1)
  (on b72 b74)
  (on b74 b21)
  (on b21 b24)
  (on b24 b42)
  (on b42 b47)
  (on b47 b91)
  (on b91 b6)
  (on b6 b61)
  (on b61 b1))
 (:goal
  (and
   (clear b98)
   (on-table b95)
   (on b98 b49)
   (on b49 b85)
   (on b85 b17)
   (on b17 b73)
   (on b73 b91)
   (on b91 b55)
   (on b55 b41)
   (on b41 b60)
   (on b60 b50)
   (on b50 b35)
   (on b35 b13)
   (on b13 b19)
   (on b19 b47)
   (on b47 b39)
   (on b39 b33)
   (on b33 b20)
   (on b20 b59)
   (on b59 b44)
   (on b44 b21)
   (on b21 b53)
   (on b53 b75)
   (on b75 b14)
   (on b14 b76)
   (on b76 b18)
   (on b18 b90)
   (on b90 b72)
   (on b72 b1)
   (on b1 b4)
   (on b4 b10)
   (on b10 b89)
   (on b89 b95)
   (clear b79)
   (on-table b94)
   (on b79 b37)
   (on b37 b16)
   (on b16 b80)
   (on b80 b6)
   (on b6 b74)
   (on b74 b66)
   (on b66 b38)
   (on b38 b40)
   (on b40 b3)
   (on b3 b82)
   (on b82 b84)
   (on b84 b86)
   (on b86 b94)
   (clear b67)
   (on-table b81)
   (on b67 b46)
   (on b46 b51)
   (on b51 b81)
   (clear b45)
   (on-table b63)
   (on b45 b93)
   (on b93 b22)
   (on b22 b96)
   (on b96 b63)
   (clear b30)
   (on-table b56)
   (on b30 b24)
   (on b24 b99)
   (on b99 b43)
   (on b43 b27)
   (on b27 b15)
   (on b15 b56)
   (clear b64)
   (on-table b54)
   (on b64 b8)
   (on b8 b87)
   (on b87 b65)
   (on b65 b54)
   (clear b62)
   (on-table b28)
   (on b62 b2)
   (on b2 b70)
   (on b70 b23)
   (on b23 b68)
   (on b68 b69)
   (on b69 b34)
   (on b34 b29)
   (on b29 b11)
   (on b11 b71)
   (on b71 b7)
   (on b7 b100)
   (on b100 b28)
   (clear b52)
   (on-table b12)
   (on b52 b31)
   (on b31 b12)
   (clear b88)
   (on-table b9)
   (on b88 b36)
   (on b36 b61)
   (on b61 b83)
   (on b83 b42)
   (on b42 b57)
   (on b57 b25)
   (on b25 b97)
   (on b97 b77)
   (on b77 b92)
   (on b92 b48)
   (on b48 b58)
   (on b58 b32)
   (on b32 b9)
   (clear b26)
   (on-table b5)
   (on b26 b78)
   (on b78 b5))))