(define
 (problem pfile_10_90)
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
           o71
           o72
           o73
           o74
           o75
           o76
           o77
           o78
           o79
           o80
           o81
           o82
           o83
           o84
           o85
           o86
           o87
           o88
           o89
           o90
           - PACKAGE
           c r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 - ROOM
           d12 d24 d27 d78 d29 d69 d04 d34 d310 d05 - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r4 d04)
  (door c r5 d05)
  (door r1 r2 d12)
  (door r2 r1 d12)
  (door r2 r4 d24)
  (door r2 r7 d27)
  (door r2 r9 d29)
  (door r3 r4 d34)
  (door r3 r10 d310)
  (door r4 c d04)
  (door r4 r2 d24)
  (door r4 r3 d34)
  (door r5 c d05)
  (door r6 r9 d69)
  (door r7 r2 d27)
  (door r7 r8 d78)
  (door r8 r7 d78)
  (door r9 r2 d29)
  (door r9 r6 d69)
  (door r10 r3 d310)
  (closed d27)
  (closed d78)
  (closed d29)
  (closed d69)
  (closed d310)
  (closed d05)
  (in o1 r2)
  (in o2 r6)
  (in o3 r1)
  (in o4 r8)
  (in o5 r9)
  (in o6 r5)
  (in o7 r8)
  (in o8 r8)
  (in o9 r8)
  (in o10 r8)
  (in o11 r5)
  (in o12 r9)
  (in o13 r9)
  (in o14 r5)
  (in o15 r3)
  (in o16 r3)
  (in o17 r3)
  (in o18 r4)
  (in o19 r4)
  (in o20 r10)
  (in o21 r10)
  (in o22 r7)
  (in o23 r10)
  (in o24 r4)
  (in o25 r6)
  (in o26 r2)
  (in o27 r2)
  (in o28 r1)
  (in o29 r5)
  (in o30 r8)
  (in o31 r3)
  (in o32 r6)
  (in o33 r8)
  (in o34 r8)
  (in o35 r7)
  (in o36 r7)
  (in o37 r7)
  (in o38 r8)
  (in o39 r10)
  (in o40 r1)
  (in o41 r10)
  (in o42 r5)
  (in o43 r8)
  (in o44 r1)
  (in o45 r9)
  (in o46 r4)
  (in o47 r1)
  (in o48 r7)
  (in o49 r1)
  (in o50 r1)
  (in o51 r2)
  (in o52 r7)
  (in o53 r5)
  (in o54 r3)
  (in o55 r3)
  (in o56 r8)
  (in o57 r1)
  (in o58 r7)
  (in o59 r2)
  (in o60 r9)
  (in o61 r3)
  (in o62 r6)
  (in o63 r7)
  (in o64 r6)
  (in o65 r8)
  (in o66 r6)
  (in o67 r6)
  (in o68 r5)
  (in o69 r2)
  (in o70 r8)
  (in o71 r6)
  (in o72 r8)
  (in o73 r1)
  (in o74 r4)
  (in o75 r7)
  (in o76 r9)
  (in o77 r1)
  (in o78 r4)
  (in o79 r7)
  (in o80 r7)
  (in o81 r4)
  (in o82 r6)
  (in o83 r3)
  (in o84 r7)
  (in o85 r2)
  (in o86 r2)
  (in o87 r10)
  (in o88 r9)
  (in o89 r10)
  (in o90 r10))
 (:goal (and
         (in o1 r4)
         (in o2 r4)
         (in o3 r6)
         (in o4 r1)
         (in o5 r3)
         (in o6 r4)
         (in o7 r1)
         (in o8 r7)
         (in o9 r4)
         (in o10 r7)
         (in o11 r9)
         (in o12 r2)
         (in o13 r1)
         (in o14 r5)
         (in o15 r10)
         (in o16 r3)
         (in o17 r8)
         (in o18 r9)
         (in o19 r3)
         (in o20 r3)
         (in o21 r7)
         (in o22 r1)
         (in o23 r4)
         (in o24 r5)
         (in o25 r7)
         (in o26 r4)
         (in o27 r2)
         (in o28 r7)
         (in o29 r7)
         (in o30 r5)
         (in o31 r3)
         (in o32 r8)
         (in o33 r10)
         (in o34 r4)
         (in o35 r4)
         (in o36 r9)
         (in o37 r4)
         (in o38 r8)
         (in o39 r7)
         (in o40 r1)
         (in o41 r3)
         (in o42 r6)
         (in o43 r10)
         (in o44 r1)
         (in o45 r6)
         (in o46 r9)
         (in o47 r6)
         (in o48 r4)
         (in o49 r8)
         (in o50 r3)
         (in o51 r3)
         (in o52 r6)
         (in o53 r3)
         (in o54 r9)
         (in o55 r5)
         (in o56 r6)
         (in o57 r3)
         (in o58 r2)
         (in o59 r9)
         (in o60 r9)
         (in o61 r7)
         (in o62 r3)
         (in o63 r1)
         (in o64 r2)
         (in o65 r8)
         (in o66 r8)
         (in o67 r8)
         (in o68 r7)
         (in o69 r6)
         (in o70 r6)
         (in o71 r8)
         (in o72 r8)
         (in o73 r2)
         (in o74 r3)
         (in o75 r3)
         (in o76 r8)
         (in o77 r7)
         (in o78 r9)
         (in o79 r3)
         (in o80 r8)
         (in o81 r1)
         (in o82 r8)
         (in o83 r1)
         (in o84 r9)
         (in o85 r10)
         (in o86 r4)
         (in o87 r9)
         (in o88 r7)
         (in o89 r9)
         (in o90 r1))))