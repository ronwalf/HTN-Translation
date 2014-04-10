(define
 (problem pfile_025)
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
  b25 - BLOCK)
 (:init
  (armempty)
  (clear b18)
  (on-table b23)
  (on b18 b12)
  (on b12 b8)
  (on b8 b1)
  (on b1 b16)
  (on b16 b2)
  (on b2 b20)
  (on b20 b9)
  (on b9 b7)
  (on b7 b23)
  (clear b3)
  (on-table b22)
  (on b3 b10)
  (on b10 b24)
  (on b24 b5)
  (on b5 b25)
  (on b25 b22)
  (clear b15)
  (on-table b21)
  (on b15 b14)
  (on b14 b21)
  (clear b13)
  (on-table b17)
  (on b13 b4)
  (on b4 b17)
  (clear b19)
  (on-table b11)
  (on b19 b11)
  (clear b6)
  (on-table b6))
 (:goal
  (and
   (clear b7)
   (on-table b25)
   (on b7 b2)
   (on b2 b6)
   (on b6 b18)
   (on b18 b8)
   (on b8 b11)
   (on b11 b12)
   (on b12 b24)
   (on b24 b9)
   (on b9 b13)
   (on b13 b19)
   (on b19 b25)
   (clear b1)
   (on-table b16)
   (on b1 b23)
   (on b23 b21)
   (on b21 b16)
   (clear b15)
   (on-table b4)
   (on b15 b17)
   (on b17 b22)
   (on b22 b5)
   (on b5 b4)
   (clear b14)
   (on-table b3)
   (on b14 b20)
   (on b20 b10)
   (on b10 b3))))