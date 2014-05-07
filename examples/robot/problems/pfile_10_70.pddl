(define
 (problem pfile_10_70)
 (:domain robot)
 (:objects o1
           o2
           o3
           o4
           o5
           o6
           o7
           o8
           o9
           o10
           o11
           o12
           o13
           o14
           o15
           o16
           o17
           o18
           o19
           o20
           o21
           o22
           o23
           o24
           o25
           o26
           o27
           o28
           o29
           o30
           o31
           o32
           o33
           o34
           o35
           o36
           o37
           o38
           o39
           o40
           o41
           o42
           o43
           o44
           o45
           o46
           o47
           o48
           o49
           o50
           o51
           o52
           o53
           o54
           o55
           o56
           o57
           o58
           o59
           o60
           o61
           o62
           o63
           o64
           o65
           o66
           o67
           o68
           o69
           o70
           - PACKAGE
           c r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 - ROOM
           d69 d610 d110 d18 d56 d35 d78 d27 d34 d04 - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r4 d04)
  (door r1 r8 d18)
  (door r1 r10 d110)
  (door r2 r7 d27)
  (door r3 r4 d34)
  (door r3 r5 d35)
  (door r4 c d04)
  (door r4 r3 d34)
  (door r5 r3 d35)
  (door r5 r6 d56)
  (door r6 r5 d56)
  (door r6 r9 d69)
  (door r6 r10 d610)
  (door r7 r2 d27)
  (door r7 r8 d78)
  (door r8 r1 d18)
  (door r8 r7 d78)
  (door r9 r6 d69)
  (door r10 r1 d110)
  (door r10 r6 d610)
  (closed d69)
  (closed d110)
  (closed d35)
  (closed d78)
  (closed d27)
  (closed d34)
  (in o1 r4)
  (in o2 r8)
  (in o3 r7)
  (in o4 r7)
  (in o5 r2)
  (in o6 r7)
  (in o7 r3)
  (in o8 r7)
  (in o9 r3)
  (in o10 r10)
  (in o11 r5)
  (in o12 r8)
  (in o13 r7)
  (in o14 r4)
  (in o15 r5)
  (in o16 r4)
  (in o17 r3)
  (in o18 r7)
  (in o19 r8)
  (in o20 r10)
  (in o21 r10)
  (in o22 r1)
  (in o23 r5)
  (in o24 r6)
  (in o25 r7)
  (in o26 r1)
  (in o27 r3)
  (in o28 r6)
  (in o29 r6)
  (in o30 r5)
  (in o31 r4)
  (in o32 r8)
  (in o33 r2)
  (in o34 r6)
  (in o35 r9)
  (in o36 r10)
  (in o37 r8)
  (in o38 r6)
  (in o39 r7)
  (in o40 r3)
  (in o41 r10)
  (in o42 r5)
  (in o43 r7)
  (in o44 r3)
  (in o45 r6)
  (in o46 r5)
  (in o47 r10)
  (in o48 r6)
  (in o49 r2)
  (in o50 r9)
  (in o51 r7)
  (in o52 r1)
  (in o53 r4)
  (in o54 r5)
  (in o55 r9)
  (in o56 r5)
  (in o57 r6)
  (in o58 r8)
  (in o59 r10)
  (in o60 r8)
  (in o61 r6)
  (in o62 r10)
  (in o63 r6)
  (in o64 r2)
  (in o65 r8)
  (in o66 r1)
  (in o67 r2)
  (in o68 r10)
  (in o69 r2)
  (in o70 r10))
 (:goal (and
         (in o1 r3)
         (in o2 r5)
         (in o3 r4)
         (in o4 r8)
         (in o5 r8)
         (in o6 r7)
         (in o7 r4)
         (in o8 r3)
         (in o9 r6)
         (in o10 r2)
         (in o11 r5)
         (in o12 r10)
         (in o13 r5)
         (in o14 r5)
         (in o15 r3)
         (in o16 r4)
         (in o17 r5)
         (in o18 r9)
         (in o19 r10)
         (in o20 r2)
         (in o21 r3)
         (in o22 r2)
         (in o23 r6)
         (in o24 r6)
         (in o25 r8)
         (in o26 r5)
         (in o27 r6)
         (in o28 r10)
         (in o29 r1)
         (in o30 r3)
         (in o31 r3)
         (in o32 r8)
         (in o33 r10)
         (in o34 r5)
         (in o35 r9)
         (in o36 r8)
         (in o37 r7)
         (in o38 r6)
         (in o39 r3)
         (in o40 r3)
         (in o41 r7)
         (in o42 r9)
         (in o43 r4)
         (in o44 r2)
         (in o45 r4)
         (in o46 r9)
         (in o47 r5)
         (in o48 r9)
         (in o49 r6)
         (in o50 r6)
         (in o51 r6)
         (in o52 r5)
         (in o53 r10)
         (in o54 r4)
         (in o55 r7)
         (in o56 r9)
         (in o57 r4)
         (in o58 r6)
         (in o59 r8)
         (in o60 r7)
         (in o61 r9)
         (in o62 r3)
         (in o63 r9)
         (in o64 r9)
         (in o65 r1)
         (in o66 r3)
         (in o67 r4)
         (in o68 r4)
         (in o69 r4)
         (in o70 r1))))