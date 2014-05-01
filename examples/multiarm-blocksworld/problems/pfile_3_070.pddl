(define
 (problem pfile_3_070)
 (:domain blocks)
 (:objects arm1 arm2 arm3 - ARM
           b1
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
           b46
           b47
           b48
           b49
           b50
           b51
           b52
           b53
           b54
           b55
           b56
           b57
           b58
           b59
           b60
           b61
           b62
           b63
           b64
           b65
           b66
           b67
           b68
           b69
           b70
           - BLOCK)
 (:init
  (hand-empty arm1)
  (hand-empty arm2)
  (hand-empty arm3)
  (clear b53)
  (on-table b57)
  (on b53 b18)
  (on b18 b57)
  (clear b62)
  (on-table b37)
  (on b62 b15)
  (on b15 b66)
  (on b66 b49)
  (on b49 b35)
  (on b35 b52)
  (on b52 b19)
  (on b19 b7)
  (on b7 b9)
  (on b9 b61)
  (on b61 b37)
  (clear b26)
  (on-table b36)
  (on b26 b22)
  (on b22 b2)
  (on b2 b28)
  (on b28 b44)
  (on b44 b36)
  (clear b67)
  (on-table b25)
  (on b67 b5)
  (on b5 b33)
  (on b33 b32)
  (on b32 b40)
  (on b40 b48)
  (on b48 b51)
  (on b51 b45)
  (on b45 b43)
  (on b43 b13)
  (on b13 b17)
  (on b17 b10)
  (on b10 b8)
  (on b8 b55)
  (on b55 b29)
  (on b29 b39)
  (on b39 b56)
  (on b56 b68)
  (on b68 b1)
  (on b1 b20)
  (on b20 b69)
  (on b69 b34)
  (on b34 b27)
  (on b27 b25)
  (clear b42)
  (on-table b11)
  (on b42 b30)
  (on b30 b54)
  (on b54 b24)
  (on b24 b63)
  (on b63 b12)
  (on b12 b58)
  (on b58 b16)
  (on b16 b64)
  (on b64 b65)
  (on b65 b23)
  (on b23 b70)
  (on b70 b50)
  (on b50 b11)
  (clear b47)
  (on-table b6)
  (on b47 b38)
  (on b38 b14)
  (on b14 b3)
  (on b3 b6)
  (clear b59)
  (on-table b4)
  (on b59 b60)
  (on b60 b46)
  (on b46 b41)
  (on b41 b21)
  (on b21 b31)
  (on b31 b4))
 (:goal (and
         (clear b39)
         (on-table b59)
         (on b39 b10)
         (on b10 b9)
         (on b9 b46)
         (on b46 b60)
         (on b60 b16)
         (on b16 b18)
         (on b18 b2)
         (on b2 b17)
         (on b17 b31)
         (on b31 b69)
         (on b69 b68)
         (on b68 b44)
         (on b44 b64)
         (on b64 b41)
         (on b41 b70)
         (on b70 b45)
         (on b45 b59)
         (clear b37)
         (on-table b50)
         (on b37 b35)
         (on b35 b65)
         (on b65 b52)
         (on b52 b57)
         (on b57 b50)
         (clear b36)
         (on-table b36)
         (clear b66)
         (on-table b29)
         (on b66 b11)
         (on b11 b42)
         (on b42 b29)
         (clear b58)
         (on-table b24)
         (on b58 b19)
         (on b19 b53)
         (on b53 b22)
         (on b22 b4)
         (on b4 b48)
         (on b48 b14)
         (on b14 b6)
         (on b6 b21)
         (on b21 b54)
         (on b54 b13)
         (on b13 b24)
         (clear b23)
         (on-table b12)
         (on b23 b40)
         (on b40 b1)
         (on b1 b43)
         (on b43 b3)
         (on b3 b5)
         (on b5 b32)
         (on b32 b8)
         (on b8 b56)
         (on b56 b55)
         (on b55 b51)
         (on b51 b67)
         (on b67 b27)
         (on b27 b33)
         (on b33 b61)
         (on b61 b38)
         (on b38 b62)
         (on b62 b12)
         (clear b15)
         (on-table b7)
         (on b15 b20)
         (on b20 b34)
         (on b34 b26)
         (on b26 b28)
         (on b28 b30)
         (on b30 b47)
         (on b47 b49)
         (on b49 b63)
         (on b63 b25)
         (on b25 b7))))