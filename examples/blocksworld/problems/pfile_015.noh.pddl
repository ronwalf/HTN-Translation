(define
 (problem pfile_015)
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
  b15 - BLOCK)
 (:init
  (armempty)
  (clear b9)
  (on-table b10)
  (on b9 b10)
  (clear b14)
  (on-table b7)
  (on b14 b15)
  (on b15 b13)
  (on b13 b8)
  (on b8 b3)
  (on b3 b7)
  (clear b11)
  (on-table b2)
  (on b11 b4)
  (on b4 b6)
  (on b6 b12)
  (on b12 b2)
  (clear b5)
  (on-table b1)
  (on b5 b1))
 (:goal
  (and
   (clear b12)
   (on-table b12)
   (clear b3)
   (on-table b10)
   (on b3 b15)
   (on b15 b1)
   (on b1 b5)
   (on b5 b14)
   (on b14 b2)
   (on b2 b13)
   (on b13 b11)
   (on b11 b4)
   (on b4 b6)
   (on b6 b9)
   (on b9 b10)
   (clear b8)
   (on-table b8)
   (clear b7)
   (on-table b7))))