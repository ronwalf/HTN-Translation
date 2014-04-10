(define
 (problem pfile_070)
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
  b70 - BLOCK)
 (:init
  (armempty)
  (clear b7)
  (on-table b68)
  (on b7 b64)
  (on b64 b4)
  (on b4 b68)
  (clear b17)
  (on-table b40)
  (on b17 b10)
  (on b10 b3)
  (on b3 b24)
  (on b24 b44)
  (on b44 b18)
  (on b18 b67)
  (on b67 b30)
  (on b30 b40)
  (clear b50)
  (on-table b32)
  (on b50 b32)
  (clear b55)
  (on-table b31)
  (on b55 b69)
  (on b69 b31)
  (clear b6)
  (on-table b19)
  (on b6 b52)
  (on b52 b62)
  (on b62 b49)
  (on b49 b43)
  (on b43 b8)
  (on b8 b65)
  (on b65 b54)
  (on b54 b26)
  (on b26 b34)
  (on b34 b46)
  (on b46 b29)
  (on b29 b51)
  (on b51 b38)
  (on b38 b33)
  (on b33 b27)
  (on b27 b53)
  (on b53 b37)
  (on b37 b1)
  (on b1 b63)
  (on b63 b35)
  (on b35 b15)
  (on b15 b20)
  (on b20 b25)
  (on b25 b9)
  (on b9 b14)
  (on b14 b45)
  (on b45 b57)
  (on b57 b28)
  (on b28 b12)
  (on b12 b47)
  (on b47 b23)
  (on b23 b41)
  (on b41 b21)
  (on b21 b13)
  (on b13 b2)
  (on b2 b59)
  (on b59 b36)
  (on b36 b39)
  (on b39 b19)
  (clear b61)
  (on-table b16)
  (on b61 b42)
  (on b42 b66)
  (on b66 b60)
  (on b60 b16)
  (clear b58)
  (on-table b11)
  (on b58 b48)
  (on b48 b5)
  (on b5 b56)
  (on b56 b70)
  (on b70 b22)
  (on b22 b11))
 (:goal
  (and
   (clear b23)
   (on-table b63)
   (on b23 b38)
   (on b38 b1)
   (on b1 b18)
   (on b18 b61)
   (on b61 b48)
   (on b48 b27)
   (on b27 b15)
   (on b15 b12)
   (on b12 b3)
   (on b3 b22)
   (on b22 b20)
   (on b20 b49)
   (on b49 b64)
   (on b64 b60)
   (on b60 b69)
   (on b69 b14)
   (on b14 b26)
   (on b26 b16)
   (on b16 b63)
   (clear b33)
   (on-table b59)
   (on b33 b2)
   (on b2 b5)
   (on b5 b58)
   (on b58 b32)
   (on b32 b25)
   (on b25 b40)
   (on b40 b70)
   (on b70 b29)
   (on b29 b13)
   (on b13 b52)
   (on b52 b55)
   (on b55 b62)
   (on b62 b59)
   (clear b65)
   (on-table b45)
   (on b65 b42)
   (on b42 b17)
   (on b17 b8)
   (on b8 b36)
   (on b36 b54)
   (on b54 b51)
   (on b51 b35)
   (on b35 b56)
   (on b56 b53)
   (on b53 b11)
   (on b11 b45)
   (clear b50)
   (on-table b31)
   (on b50 b30)
   (on b30 b39)
   (on b39 b31)
   (clear b47)
   (on-table b9)
   (on b47 b66)
   (on b66 b10)
   (on b10 b44)
   (on b44 b43)
   (on b43 b24)
   (on b24 b7)
   (on b7 b28)
   (on b28 b21)
   (on b21 b37)
   (on b37 b19)
   (on b19 b57)
   (on b57 b68)
   (on b68 b41)
   (on b41 b4)
   (on b4 b67)
   (on b67 b46)
   (on b46 b34)
   (on b34 b6)
   (on b6 b9))))