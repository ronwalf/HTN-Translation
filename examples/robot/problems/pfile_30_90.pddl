(define
 (problem pfile_30_90)
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
           c
           r1
           r2
           r3
           r4
           r5
           r6
           r7
           r8
           r9
           r10
           r11
           r12
           r13
           r14
           r15
           r16
           r17
           r18
           r19
           r20
           r21
           r22
           r23
           r24
           r25
           r26
           r27
           r28
           r29
           r30
           - ROOM
           d113
           d1324
           d110
           d1016
           d129
           d2124
           d521
           d2122
           d022
           d030
           d014
           d814
           d48
           d89
           d918
           d2223
           d616
           d26
           d36
           d217
           d1720
           d625
           d1125
           d012
           d127
           d1520
           d419
           d2830
           d026
           d79
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r12 d012)
  (door c r14 d014)
  (door c r22 d022)
  (door c r26 d026)
  (door c r30 d030)
  (door r1 r10 d110)
  (door r1 r13 d113)
  (door r1 r27 d127)
  (door r1 r29 d129)
  (door r2 r6 d26)
  (door r2 r17 d217)
  (door r3 r6 d36)
  (door r4 r8 d48)
  (door r4 r19 d419)
  (door r5 r21 d521)
  (door r6 r2 d26)
  (door r6 r3 d36)
  (door r6 r16 d616)
  (door r6 r25 d625)
  (door r7 r9 d79)
  (door r8 r4 d48)
  (door r8 r9 d89)
  (door r8 r14 d814)
  (door r9 r7 d79)
  (door r9 r8 d89)
  (door r9 r18 d918)
  (door r10 r1 d110)
  (door r10 r16 d1016)
  (door r11 r25 d1125)
  (door r12 c d012)
  (door r13 r1 d113)
  (door r13 r24 d1324)
  (door r14 c d014)
  (door r14 r8 d814)
  (door r15 r20 d1520)
  (door r16 r6 d616)
  (door r16 r10 d1016)
  (door r17 r2 d217)
  (door r17 r20 d1720)
  (door r18 r9 d918)
  (door r19 r4 d419)
  (door r20 r15 d1520)
  (door r20 r17 d1720)
  (door r21 r5 d521)
  (door r21 r22 d2122)
  (door r21 r24 d2124)
  (door r22 c d022)
  (door r22 r21 d2122)
  (door r22 r23 d2223)
  (door r23 r22 d2223)
  (door r24 r13 d1324)
  (door r24 r21 d2124)
  (door r25 r6 d625)
  (door r25 r11 d1125)
  (door r26 c d026)
  (door r27 r1 d127)
  (door r28 r30 d2830)
  (door r29 r1 d129)
  (door r30 c d030)
  (door r30 r28 d2830)
  (closed d113)
  (closed d1324)
  (closed d014)
  (closed d814)
  (closed d89)
  (closed d2223)
  (closed d36)
  (closed d625)
  (closed d419)
  (closed d026)
  (in o1 r25)
  (in o2 r10)
  (in o3 r4)
  (in o4 r12)
  (in o5 r15)
  (in o6 r4)
  (in o7 r10)
  (in o8 r17)
  (in o9 r7)
  (in o10 r26)
  (in o11 r26)
  (in o12 r18)
  (in o13 r26)
  (in o14 r26)
  (in o15 r24)
  (in o16 r22)
  (in o17 r24)
  (in o18 r14)
  (in o19 r1)
  (in o20 r23)
  (in o21 r16)
  (in o22 r28)
  (in o23 r2)
  (in o24 r5)
  (in o25 r22)
  (in o26 r5)
  (in o27 r8)
  (in o28 r14)
  (in o29 r7)
  (in o30 r13)
  (in o31 r24)
  (in o32 r15)
  (in o33 r3)
  (in o34 r1)
  (in o35 r1)
  (in o36 r14)
  (in o37 r19)
  (in o38 r10)
  (in o39 r15)
  (in o40 r26)
  (in o41 r15)
  (in o42 r13)
  (in o43 r15)
  (in o44 r25)
  (in o45 r25)
  (in o46 r8)
  (in o47 r25)
  (in o48 r23)
  (in o49 r24)
  (in o50 r12)
  (in o51 r19)
  (in o52 r3)
  (in o53 r18)
  (in o54 r6)
  (in o55 r14)
  (in o56 r22)
  (in o57 r27)
  (in o58 r29)
  (in o59 r16)
  (in o60 r20)
  (in o61 r24)
  (in o62 r21)
  (in o63 r13)
  (in o64 r21)
  (in o65 r7)
  (in o66 r5)
  (in o67 r4)
  (in o68 r10)
  (in o69 r8)
  (in o70 r18)
  (in o71 r9)
  (in o72 r11)
  (in o73 r6)
  (in o74 r25)
  (in o75 r28)
  (in o76 r4)
  (in o77 r12)
  (in o78 r2)
  (in o79 r23)
  (in o80 r8)
  (in o81 r3)
  (in o82 r28)
  (in o83 r1)
  (in o84 r8)
  (in o85 r8)
  (in o86 r13)
  (in o87 r19)
  (in o88 r28)
  (in o89 r14)
  (in o90 r26))
 (:goal (and
         (in o1 r16)
         (in o2 r22)
         (in o3 r19)
         (in o4 r23)
         (in o5 r13)
         (in o6 r3)
         (in o7 r8)
         (in o8 r21)
         (in o9 r8)
         (in o10 r29)
         (in o11 r28)
         (in o12 r3)
         (in o13 r24)
         (in o14 r8)
         (in o15 r17)
         (in o16 r29)
         (in o17 r26)
         (in o18 r3)
         (in o19 r2)
         (in o20 r24)
         (in o21 r24)
         (in o22 r18)
         (in o23 r11)
         (in o24 r7)
         (in o25 r1)
         (in o26 r18)
         (in o27 r9)
         (in o28 r29)
         (in o29 r15)
         (in o30 r6)
         (in o31 r10)
         (in o32 r30)
         (in o33 r17)
         (in o34 r10)
         (in o35 r30)
         (in o36 r15)
         (in o37 r14)
         (in o38 r25)
         (in o39 r1)
         (in o40 r16)
         (in o41 r22)
         (in o42 r26)
         (in o43 r4)
         (in o44 r15)
         (in o45 r11)
         (in o46 r25)
         (in o47 r5)
         (in o48 r14)
         (in o49 r20)
         (in o50 r17)
         (in o51 r25)
         (in o52 r22)
         (in o53 r16)
         (in o54 r27)
         (in o55 r14)
         (in o56 r6)
         (in o57 r2)
         (in o58 r14)
         (in o59 r12)
         (in o60 r30)
         (in o61 r10)
         (in o62 r30)
         (in o63 r19)
         (in o64 r29)
         (in o65 r19)
         (in o66 r17)
         (in o67 r27)
         (in o68 r5)
         (in o69 r8)
         (in o70 r10)
         (in o71 r2)
         (in o72 r23)
         (in o73 r4)
         (in o74 r27)
         (in o75 r15)
         (in o76 r22)
         (in o77 r6)
         (in o78 r5)
         (in o79 r25)
         (in o80 r11)
         (in o81 r23)
         (in o82 r12)
         (in o83 r14)
         (in o84 r11)
         (in o85 r22)
         (in o86 r13)
         (in o87 r23)
         (in o88 r19)
         (in o89 r27)
         (in o90 r8))))