(define
 (problem pfile_010)
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
  b10 - BLOCK)
 (:init
  (armempty)
  (clear b6)
  (on-table b10)
  (on b6 b10)
  (clear b7)
  (on-table b8)
  (on b7 b4)
  (on b4 b9)
  (on b9 b1)
  (on b1 b3)
  (on b3 b8)
  (clear b2)
  (on-table b5)
  (on b2 b5))
 (:goal
  (and
   (clear b9)
   (on-table b9)
   (clear b10)
   (on-table b3)
   (on b10 b8)
   (on b8 b4)
   (on b4 b7)
   (on b7 b5)
   (on b5 b6)
   (on b6 b1)
   (on b1 b3)
   (clear b2)
   (on-table b2))))