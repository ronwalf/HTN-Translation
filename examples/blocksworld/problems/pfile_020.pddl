(define
 (problem pfile_020)
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
  b20 - BLOCK)
 (:init
  (armempty)
  (clear b4)
  (on-table b9)
  (on b4 b17)
  (on b17 b10)
  (on b10 b9)
  (clear b7)
  (on-table b7)
  (clear b12)
  (on-table b6)
  (on b12 b13)
  (on b13 b18)
  (on b18 b8)
  (on b8 b1)
  (on b1 b20)
  (on b20 b16)
  (on b16 b15)
  (on b15 b11)
  (on b11 b14)
  (on b14 b3)
  (on b3 b19)
  (on b19 b6)
  (clear b5)
  (on-table b2)
  (on b5 b2))
 (:goal
  (and
   (clear b20)
   (on-table b20)
   (clear b6)
   (on-table b19)
   (on b6 b19)
   (clear b18)
   (on-table b18)
   (clear b17)
   (on-table b17)
   (clear b14)
   (on-table b12)
   (on b14 b12)
   (clear b15)
   (on-table b9)
   (on b15 b9)
   (clear b4)
   (on-table b8)
   (on b4 b11)
   (on b11 b13)
   (on b13 b8)
   (clear b5)
   (on-table b7)
   (on b5 b7)
   (clear b16)
   (on-table b3)
   (on b16 b10)
   (on b10 b3)
   (clear b2)
   (on-table b1)
   (on b2 b1))))