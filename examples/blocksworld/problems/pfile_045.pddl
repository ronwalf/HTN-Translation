(define
 (problem pfile_045)
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
           b41
           b42
           b43
           b44
           b45
           - BLOCK)
 (:init
  (hand-empty)
  (clear b44)
  (on-table b44)
  (clear b25)
  (on-table b23)
  (on b25 b16)
  (on b16 b19)
  (on b19 b37)
  (on b37 b2)
  (on b2 b33)
  (on b33 b12)
  (on b12 b34)
  (on b34 b8)
  (on b8 b21)
  (on b21 b36)
  (on b36 b15)
  (on b15 b24)
  (on b24 b4)
  (on b4 b22)
  (on b22 b14)
  (on b14 b5)
  (on b5 b23)
  (clear b29)
  (on-table b20)
  (on b29 b30)
  (on b30 b3)
  (on b3 b9)
  (on b9 b6)
  (on b6 b38)
  (on b38 b18)
  (on b18 b20)
  (clear b10)
  (on-table b17)
  (on b10 b41)
  (on b41 b43)
  (on b43 b35)
  (on b35 b31)
  (on b31 b42)
  (on b42 b1)
  (on b1 b13)
  (on b13 b39)
  (on b39 b17)
  (clear b32)
  (on-table b11)
  (on b32 b28)
  (on b28 b40)
  (on b40 b11)
  (clear b27)
  (on-table b7)
  (on b27 b26)
  (on b26 b45)
  (on b45 b7))
 (:goal (and
         (clear b2)
         (on-table b38)
         (on b2 b22)
         (on b22 b42)
         (on b42 b23)
         (on b23 b45)
         (on b45 b1)
         (on b1 b30)
         (on b30 b36)
         (on b36 b43)
         (on b43 b6)
         (on b6 b4)
         (on b4 b15)
         (on b15 b25)
         (on b25 b24)
         (on b24 b19)
         (on b19 b12)
         (on b12 b41)
         (on b41 b44)
         (on b44 b33)
         (on b33 b14)
         (on b14 b39)
         (on b39 b40)
         (on b40 b8)
         (on b8 b38)
         (clear b9)
         (on-table b21)
         (on b9 b11)
         (on b11 b32)
         (on b32 b27)
         (on b27 b26)
         (on b26 b28)
         (on b28 b18)
         (on b18 b35)
         (on b35 b17)
         (on b17 b13)
         (on b13 b34)
         (on b34 b10)
         (on b10 b21)
         (clear b29)
         (on-table b16)
         (on b29 b5)
         (on b5 b31)
         (on b31 b37)
         (on b37 b7)
         (on b7 b20)
         (on b20 b3)
         (on b3 b16))))