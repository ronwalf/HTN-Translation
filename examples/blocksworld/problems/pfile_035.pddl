(define
 (problem pfile_035)
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
  b35 - BLOCK)
 (:init
  (armempty)
  (clear b32)
  (on-table b28)
  (on b32 b28)
  (clear b10)
  (on-table b26)
  (on b10 b26)
  (clear b27)
  (on-table b24)
  (on b27 b34)
  (on b34 b23)
  (on b23 b24)
  (clear b16)
  (on-table b20)
  (on b16 b20)
  (clear b8)
  (on-table b13)
  (on b8 b2)
  (on b2 b1)
  (on b1 b21)
  (on b21 b25)
  (on b25 b7)
  (on b7 b13)
  (clear b31)
  (on-table b12)
  (on b31 b11)
  (on b11 b12)
  (clear b15)
  (on-table b4)
  (on b15 b29)
  (on b29 b19)
  (on b19 b14)
  (on b14 b5)
  (on b5 b6)
  (on b6 b22)
  (on b22 b9)
  (on b9 b17)
  (on b17 b35)
  (on b35 b33)
  (on b33 b18)
  (on b18 b3)
  (on b3 b30)
  (on b30 b4))
 (:goal
  (and
   (clear b20)
   (on-table b26)
   (on b20 b33)
   (on b33 b4)
   (on b4 b5)
   (on b5 b26)
   (clear b8)
   (on-table b18)
   (on b8 b27)
   (on b27 b34)
   (on b34 b15)
   (on b15 b24)
   (on b24 b30)
   (on b30 b14)
   (on b14 b7)
   (on b7 b32)
   (on b32 b18)
   (clear b22)
   (on-table b17)
   (on b22 b19)
   (on b19 b23)
   (on b23 b13)
   (on b13 b9)
   (on b9 b6)
   (on b6 b17)
   (clear b35)
   (on-table b10)
   (on b35 b1)
   (on b1 b21)
   (on b21 b12)
   (on b12 b3)
   (on b3 b29)
   (on b29 b10)
   (clear b11)
   (on-table b2)
   (on b11 b28)
   (on b28 b25)
   (on b25 b16)
   (on b16 b31)
   (on b31 b2))))