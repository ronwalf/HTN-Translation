(define
 (problem pfile_040)
 (:domain blocks)
 (:objects b1
           b2
           b3
           b4
           b5
           b6
           b7
           b8
           b9
           b10
           b11
           b12
           b13
           b14
           b15
           b16
           b17
           b18
           b19
           b20
           b21
           b22
           b23
           b24
           b25
           b26
           b27
           b28
           b29
           b30
           b31
           b32
           b33
           b34
           b35
           b36
           b37
           b38
           b39
           b40
           - BLOCK)
 (:init
  (hand-empty)
  (clear b30)
  (on-table b37)
  (on b30 b37)
  (clear b27)
  (on-table b24)
  (on b27 b22)
  (on b22 b3)
  (on b3 b35)
  (on b35 b28)
  (on b28 b38)
  (on b38 b31)
  (on b31 b19)
  (on b19 b2)
  (on b2 b26)
  (on b26 b21)
  (on b21 b29)
  (on b29 b34)
  (on b34 b14)
  (on b14 b18)
  (on b18 b39)
  (on b39 b25)
  (on b25 b24)
  (clear b36)
  (on-table b11)
  (on b36 b11)
  (clear b1)
  (on-table b10)
  (on b1 b13)
  (on b13 b40)
  (on b40 b4)
  (on b4 b33)
  (on b33 b15)
  (on b15 b32)
  (on b32 b9)
  (on b9 b23)
  (on b23 b10)
  (clear b17)
  (on-table b6)
  (on b17 b8)
  (on b8 b5)
  (on b5 b20)
  (on b20 b7)
  (on b7 b16)
  (on b16 b12)
  (on b12 b6))
 (:goal (and
         (clear b34)
         (on-table b34)
         (clear b36)
         (on-table b26)
         (on b36 b24)
         (on b24 b26)
         (clear b40)
         (on-table b25)
         (on b40 b28)
         (on b28 b38)
         (on b38 b18)
         (on b18 b1)
         (on b1 b11)
         (on b11 b12)
         (on b12 b20)
         (on b20 b16)
         (on b16 b4)
         (on b4 b15)
         (on b15 b21)
         (on b21 b17)
         (on b17 b27)
         (on b27 b30)
         (on b30 b35)
         (on b35 b7)
         (on b7 b25)
         (clear b32)
         (on-table b14)
         (on b32 b23)
         (on b23 b39)
         (on b39 b9)
         (on b9 b5)
         (on b5 b37)
         (on b37 b13)
         (on b13 b3)
         (on b3 b8)
         (on b8 b31)
         (on b31 b14)
         (clear b19)
         (on-table b10)
         (on b19 b22)
         (on b22 b10)
         (clear b2)
         (on-table b6)
         (on b2 b29)
         (on b29 b33)
         (on b33 b6))))