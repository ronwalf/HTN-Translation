(define
 (problem pfile_075)
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
  b75 - BLOCK)
 (:init
  (armempty)
  (clear b52)
  (on-table b73)
  (on b52 b73)
  (clear b4)
  (on-table b60)
  (on b4 b65)
  (on b65 b41)
  (on b41 b70)
  (on b70 b46)
  (on b46 b26)
  (on b26 b2)
  (on b2 b10)
  (on b10 b56)
  (on b56 b30)
  (on b30 b34)
  (on b34 b27)
  (on b27 b33)
  (on b33 b22)
  (on b22 b49)
  (on b49 b59)
  (on b59 b32)
  (on b32 b44)
  (on b44 b5)
  (on b5 b71)
  (on b71 b75)
  (on b75 b6)
  (on b6 b16)
  (on b16 b3)
  (on b3 b61)
  (on b61 b48)
  (on b48 b43)
  (on b43 b35)
  (on b35 b29)
  (on b29 b24)
  (on b24 b45)
  (on b45 b11)
  (on b11 b68)
  (on b68 b69)
  (on b69 b53)
  (on b53 b74)
  (on b74 b15)
  (on b15 b64)
  (on b64 b60)
  (clear b20)
  (on-table b58)
  (on b20 b28)
  (on b28 b1)
  (on b1 b36)
  (on b36 b58)
  (clear b8)
  (on-table b57)
  (on b8 b38)
  (on b38 b50)
  (on b50 b57)
  (clear b62)
  (on-table b47)
  (on b62 b23)
  (on b23 b54)
  (on b54 b19)
  (on b19 b37)
  (on b37 b51)
  (on b51 b39)
  (on b39 b9)
  (on b9 b40)
  (on b40 b14)
  (on b14 b17)
  (on b17 b13)
  (on b13 b55)
  (on b55 b47)
  (clear b66)
  (on-table b42)
  (on b66 b63)
  (on b63 b25)
  (on b25 b31)
  (on b31 b67)
  (on b67 b7)
  (on b7 b42)
  (clear b21)
  (on-table b12)
  (on b21 b18)
  (on b18 b72)
  (on b72 b12))
 (:goal
  (and
   (clear b22)
   (on-table b69)
   (on b22 b75)
   (on b75 b16)
   (on b16 b53)
   (on b53 b6)
   (on b6 b72)
   (on b72 b44)
   (on b44 b36)
   (on b36 b47)
   (on b47 b10)
   (on b10 b23)
   (on b23 b73)
   (on b73 b8)
   (on b8 b26)
   (on b26 b70)
   (on b70 b49)
   (on b49 b14)
   (on b14 b37)
   (on b37 b57)
   (on b57 b17)
   (on b17 b50)
   (on b50 b29)
   (on b29 b20)
   (on b20 b31)
   (on b31 b46)
   (on b46 b12)
   (on b12 b18)
   (on b18 b24)
   (on b24 b38)
   (on b38 b35)
   (on b35 b11)
   (on b11 b32)
   (on b32 b68)
   (on b68 b71)
   (on b71 b39)
   (on b39 b69)
   (clear b43)
   (on-table b67)
   (on b43 b52)
   (on b52 b25)
   (on b25 b30)
   (on b30 b62)
   (on b62 b9)
   (on b9 b59)
   (on b59 b74)
   (on b74 b61)
   (on b61 b15)
   (on b15 b60)
   (on b60 b67)
   (clear b48)
   (on-table b66)
   (on b48 b54)
   (on b54 b66)
   (clear b51)
   (on-table b51)
   (clear b45)
   (on-table b45)
   (clear b65)
   (on-table b41)
   (on b65 b55)
   (on b55 b42)
   (on b42 b33)
   (on b33 b64)
   (on b64 b28)
   (on b28 b58)
   (on b58 b41)
   (clear b21)
   (on-table b27)
   (on b21 b5)
   (on b5 b63)
   (on b63 b2)
   (on b2 b19)
   (on b19 b27)
   (clear b13)
   (on-table b7)
   (on b13 b34)
   (on b34 b56)
   (on b56 b4)
   (on b4 b7)
   (clear b40)
   (on-table b3)
   (on b40 b1)
   (on b1 b3))))