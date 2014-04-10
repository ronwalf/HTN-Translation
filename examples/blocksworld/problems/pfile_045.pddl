(define
 (problem pfile_045)
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
  b45 - BLOCK)
 (:init
  (armempty)
  (clear b8)
  (on-table b44)
  (on b8 b44)
  (clear b36)
  (on-table b42)
  (on b36 b22)
  (on b22 b42)
  (clear b37)
  (on-table b33)
  (on b37 b3)
  (on b3 b28)
  (on b28 b9)
  (on b9 b16)
  (on b16 b5)
  (on b5 b10)
  (on b10 b30)
  (on b30 b33)
  (clear b39)
  (on-table b31)
  (on b39 b6)
  (on b6 b14)
  (on b14 b38)
  (on b38 b34)
  (on b34 b41)
  (on b41 b26)
  (on b26 b45)
  (on b45 b20)
  (on b20 b4)
  (on b4 b43)
  (on b43 b27)
  (on b27 b40)
  (on b40 b11)
  (on b11 b31)
  (clear b7)
  (on-table b25)
  (on b7 b17)
  (on b17 b12)
  (on b12 b23)
  (on b23 b21)
  (on b21 b25)
  (clear b24)
  (on-table b18)
  (on b24 b32)
  (on b32 b29)
  (on b29 b13)
  (on b13 b2)
  (on b2 b19)
  (on b19 b18)
  (clear b35)
  (on-table b1)
  (on b35 b15)
  (on b15 b1))
 (:goal
  (and
   (clear b41)
   (on-table b45)
   (on b41 b2)
   (on b2 b1)
   (on b1 b12)
   (on b12 b29)
   (on b29 b22)
   (on b22 b4)
   (on b4 b45)
   (clear b26)
   (on-table b37)
   (on b26 b8)
   (on b8 b9)
   (on b9 b38)
   (on b38 b14)
   (on b14 b21)
   (on b21 b39)
   (on b39 b6)
   (on b6 b34)
   (on b34 b43)
   (on b43 b42)
   (on b42 b44)
   (on b44 b17)
   (on b17 b37)
   (clear b16)
   (on-table b35)
   (on b16 b35)
   (clear b33)
   (on-table b30)
   (on b33 b32)
   (on b32 b30)
   (clear b40)
   (on-table b28)
   (on b40 b3)
   (on b3 b25)
   (on b25 b11)
   (on b11 b18)
   (on b18 b13)
   (on b13 b15)
   (on b15 b31)
   (on b31 b10)
   (on b10 b28)
   (clear b36)
   (on-table b27)
   (on b36 b27)
   (clear b23)
   (on-table b23)
   (clear b5)
   (on-table b19)
   (on b5 b19)
   (clear b24)
   (on-table b7)
   (on b24 b20)
   (on b20 b7))))