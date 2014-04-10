(define
 (problem pfile_030)
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
  b30 - BLOCK)
 (:init
  (armempty)
  (clear b10)
  (on-table b29)
  (on b10 b18)
  (on b18 b29)
  (clear b16)
  (on-table b24)
  (on b16 b21)
  (on b21 b11)
  (on b11 b24)
  (clear b30)
  (on-table b20)
  (on b30 b14)
  (on b14 b4)
  (on b4 b20)
  (clear b22)
  (on-table b12)
  (on b22 b23)
  (on b23 b12)
  (clear b7)
  (on-table b3)
  (on b7 b5)
  (on b5 b17)
  (on b17 b27)
  (on b27 b9)
  (on b9 b25)
  (on b25 b13)
  (on b13 b1)
  (on b1 b8)
  (on b8 b3)
  (clear b28)
  (on-table b2)
  (on b28 b19)
  (on b19 b26)
  (on b26 b15)
  (on b15 b6)
  (on b6 b2))
 (:goal
  (and
   (clear b5)
   (on-table b28)
   (on b5 b10)
   (on b10 b16)
   (on b16 b29)
   (on b29 b13)
   (on b13 b2)
   (on b2 b22)
   (on b22 b28)
   (clear b6)
   (on-table b19)
   (on b6 b21)
   (on b21 b19)
   (clear b3)
   (on-table b18)
   (on b3 b7)
   (on b7 b18)
   (clear b27)
   (on-table b4)
   (on b27 b4)
   (clear b9)
   (on-table b1)
   (on b9 b24)
   (on b24 b15)
   (on b15 b8)
   (on b8 b26)
   (on b26 b11)
   (on b11 b12)
   (on b12 b14)
   (on b14 b25)
   (on b25 b30)
   (on b30 b23)
   (on b23 b17)
   (on b17 b20)
   (on b20 b1))))