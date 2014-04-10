(define
 (problem pfile_040)
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
  b40 - BLOCK)
 (:init
  (armempty)
  (clear b33)
  (on-table b36)
  (on b33 b23)
  (on b23 b27)
  (on b27 b31)
  (on b31 b2)
  (on b2 b16)
  (on b16 b14)
  (on b14 b36)
  (clear b37)
  (on-table b34)
  (on b37 b15)
  (on b15 b1)
  (on b1 b19)
  (on b19 b30)
  (on b30 b38)
  (on b38 b22)
  (on b22 b9)
  (on b9 b11)
  (on b11 b5)
  (on b5 b28)
  (on b28 b13)
  (on b13 b34)
  (clear b32)
  (on-table b29)
  (on b32 b24)
  (on b24 b20)
  (on b20 b29)
  (clear b17)
  (on-table b8)
  (on b17 b4)
  (on b4 b8)
  (clear b3)
  (on-table b7)
  (on b3 b40)
  (on b40 b25)
  (on b25 b21)
  (on b21 b18)
  (on b18 b7)
  (clear b26)
  (on-table b6)
  (on b26 b35)
  (on b35 b39)
  (on b39 b12)
  (on b12 b10)
  (on b10 b6))
 (:goal
  (and
   (clear b30)
   (on-table b30)
   (clear b31)
   (on-table b19)
   (on b31 b7)
   (on b7 b2)
   (on b2 b19)
   (clear b32)
   (on-table b11)
   (on b32 b37)
   (on b37 b11)
   (clear b21)
   (on-table b9)
   (on b21 b24)
   (on b24 b16)
   (on b16 b8)
   (on b8 b20)
   (on b20 b38)
   (on b38 b40)
   (on b40 b36)
   (on b36 b29)
   (on b29 b4)
   (on b4 b26)
   (on b26 b6)
   (on b6 b33)
   (on b33 b34)
   (on b34 b25)
   (on b25 b9)
   (clear b1)
   (on-table b5)
   (on b1 b17)
   (on b17 b22)
   (on b22 b10)
   (on b10 b28)
   (on b28 b14)
   (on b14 b27)
   (on b27 b39)
   (on b39 b35)
   (on b35 b3)
   (on b3 b13)
   (on b13 b18)
   (on b18 b15)
   (on b15 b12)
   (on b12 b23)
   (on b23 b5))))