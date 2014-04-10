(define
 (problem pfile_080)
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
  b80 - BLOCK)
 (:init
  (armempty)
  (clear b71)
  (on-table b44)
  (on b71 b35)
  (on b35 b46)
  (on b46 b51)
  (on b51 b22)
  (on b22 b17)
  (on b17 b80)
  (on b80 b39)
  (on b39 b50)
  (on b50 b44)
  (clear b62)
  (on-table b43)
  (on b62 b36)
  (on b36 b79)
  (on b79 b41)
  (on b41 b75)
  (on b75 b2)
  (on b2 b40)
  (on b40 b14)
  (on b14 b8)
  (on b8 b43)
  (clear b59)
  (on-table b38)
  (on b59 b4)
  (on b4 b49)
  (on b49 b48)
  (on b48 b58)
  (on b58 b9)
  (on b9 b47)
  (on b47 b53)
  (on b53 b30)
  (on b30 b33)
  (on b33 b16)
  (on b16 b38)
  (clear b61)
  (on-table b32)
  (on b61 b68)
  (on b68 b11)
  (on b11 b18)
  (on b18 b78)
  (on b78 b55)
  (on b55 b1)
  (on b1 b76)
  (on b76 b3)
  (on b3 b73)
  (on b73 b10)
  (on b10 b31)
  (on b31 b20)
  (on b20 b77)
  (on b77 b72)
  (on b72 b13)
  (on b13 b74)
  (on b74 b6)
  (on b6 b32)
  (clear b28)
  (on-table b28)
  (clear b19)
  (on-table b27)
  (on b19 b57)
  (on b57 b5)
  (on b5 b7)
  (on b7 b42)
  (on b42 b15)
  (on b15 b64)
  (on b64 b65)
  (on b65 b45)
  (on b45 b21)
  (on b21 b27)
  (clear b56)
  (on-table b23)
  (on b56 b63)
  (on b63 b26)
  (on b26 b25)
  (on b25 b60)
  (on b60 b66)
  (on b66 b70)
  (on b70 b29)
  (on b29 b52)
  (on b52 b34)
  (on b34 b23)
  (clear b24)
  (on-table b12)
  (on b24 b54)
  (on b54 b69)
  (on b69 b37)
  (on b37 b67)
  (on b67 b12))
 (:goal
  (and
   (clear b78)
   (on-table b76)
   (on b78 b36)
   (on b36 b63)
   (on b63 b60)
   (on b60 b46)
   (on b46 b2)
   (on b2 b56)
   (on b56 b61)
   (on b61 b76)
   (clear b8)
   (on-table b66)
   (on b8 b50)
   (on b50 b23)
   (on b23 b42)
   (on b42 b66)
   (clear b79)
   (on-table b41)
   (on b79 b54)
   (on b54 b77)
   (on b77 b35)
   (on b35 b52)
   (on b52 b18)
   (on b18 b34)
   (on b34 b41)
   (clear b80)
   (on-table b14)
   (on b80 b7)
   (on b7 b24)
   (on b24 b55)
   (on b55 b68)
   (on b68 b40)
   (on b40 b6)
   (on b6 b26)
   (on b26 b74)
   (on b74 b19)
   (on b19 b59)
   (on b59 b72)
   (on b72 b16)
   (on b16 b12)
   (on b12 b70)
   (on b70 b58)
   (on b58 b65)
   (on b65 b28)
   (on b28 b17)
   (on b17 b29)
   (on b29 b14)
   (clear b69)
   (on-table b13)
   (on b69 b53)
   (on b53 b25)
   (on b25 b30)
   (on b30 b37)
   (on b37 b20)
   (on b20 b27)
   (on b27 b38)
   (on b38 b57)
   (on b57 b13)
   (clear b21)
   (on-table b10)
   (on b21 b11)
   (on b11 b33)
   (on b33 b62)
   (on b62 b39)
   (on b39 b10)
   (clear b45)
   (on-table b9)
   (on b45 b71)
   (on b71 b32)
   (on b32 b44)
   (on b44 b5)
   (on b5 b31)
   (on b31 b22)
   (on b22 b47)
   (on b47 b48)
   (on b48 b64)
   (on b64 b9)
   (clear b49)
   (on-table b4)
   (on b49 b15)
   (on b15 b43)
   (on b43 b1)
   (on b1 b51)
   (on b51 b67)
   (on b67 b4)
   (clear b75)
   (on-table b3)
   (on b75 b73)
   (on b73 b3))))