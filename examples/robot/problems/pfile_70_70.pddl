(define
 (problem pfile_70_70)
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
           r31
           r32
           r33
           r34
           r35
           r36
           r37
           r38
           r39
           r40
           r41
           r42
           r43
           r44
           r45
           r46
           r47
           r48
           r49
           r50
           r51
           r52
           r53
           r54
           r55
           r56
           r57
           r58
           r59
           r60
           r61
           r62
           r63
           r64
           r65
           r66
           r67
           r68
           r69
           r70
           - ROOM
           d1442
           d1459
           d1427
           d2769
           d1456
           d5660
           d5160
           d2151
           d5152
           d4952
           d1552
           d1535
           d735
           d3553
           d2553
           d1545
           d746
           d4853
           d2645
           d2945
           d3145
           d3165
           d6365
           d6566
           d2066
           d1866
           d2024
           d524
           d4970
           d870
           d854
           d5054
           d5268
           d3368
           d368
           d316
           d3362
           d2333
           d423
           d2344
           d1744
           d017
           d047
           d166
           d119
           d1822
           d233
           d1024
           d3652
           d3664
           d4145
           d2439
           d411
           d1132
           d1232
           d3243
           d4357
           d921
           d2228
           d3056
           d1555
           d4055
           d6169
           d3747
           d3738
           d3438
           d06
           d1355
           d5067
           d3758
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r6 d06)
  (door c r17 d017)
  (door c r47 d047)
  (door r1 r19 d119)
  (door r1 r66 d166)
  (door r2 r33 d233)
  (door r3 r16 d316)
  (door r3 r68 d368)
  (door r4 r11 d411)
  (door r4 r23 d423)
  (door r5 r24 d524)
  (door r6 c d06)
  (door r7 r35 d735)
  (door r7 r46 d746)
  (door r8 r54 d854)
  (door r8 r70 d870)
  (door r9 r21 d921)
  (door r10 r24 d1024)
  (door r11 r4 d411)
  (door r11 r32 d1132)
  (door r12 r32 d1232)
  (door r13 r55 d1355)
  (door r14 r27 d1427)
  (door r14 r42 d1442)
  (door r14 r56 d1456)
  (door r14 r59 d1459)
  (door r15 r35 d1535)
  (door r15 r45 d1545)
  (door r15 r52 d1552)
  (door r15 r55 d1555)
  (door r16 r3 d316)
  (door r17 c d017)
  (door r17 r44 d1744)
  (door r18 r22 d1822)
  (door r18 r66 d1866)
  (door r19 r1 d119)
  (door r20 r24 d2024)
  (door r20 r66 d2066)
  (door r21 r9 d921)
  (door r21 r51 d2151)
  (door r22 r18 d1822)
  (door r22 r28 d2228)
  (door r23 r4 d423)
  (door r23 r33 d2333)
  (door r23 r44 d2344)
  (door r24 r5 d524)
  (door r24 r10 d1024)
  (door r24 r20 d2024)
  (door r24 r39 d2439)
  (door r25 r53 d2553)
  (door r26 r45 d2645)
  (door r27 r14 d1427)
  (door r27 r69 d2769)
  (door r28 r22 d2228)
  (door r29 r45 d2945)
  (door r30 r56 d3056)
  (door r31 r45 d3145)
  (door r31 r65 d3165)
  (door r32 r11 d1132)
  (door r32 r12 d1232)
  (door r32 r43 d3243)
  (door r33 r2 d233)
  (door r33 r23 d2333)
  (door r33 r62 d3362)
  (door r33 r68 d3368)
  (door r34 r38 d3438)
  (door r35 r7 d735)
  (door r35 r15 d1535)
  (door r35 r53 d3553)
  (door r36 r52 d3652)
  (door r36 r64 d3664)
  (door r37 r38 d3738)
  (door r37 r47 d3747)
  (door r37 r58 d3758)
  (door r38 r34 d3438)
  (door r38 r37 d3738)
  (door r39 r24 d2439)
  (door r40 r55 d4055)
  (door r41 r45 d4145)
  (door r42 r14 d1442)
  (door r43 r32 d3243)
  (door r43 r57 d4357)
  (door r44 r17 d1744)
  (door r44 r23 d2344)
  (door r45 r15 d1545)
  (door r45 r26 d2645)
  (door r45 r29 d2945)
  (door r45 r31 d3145)
  (door r45 r41 d4145)
  (door r46 r7 d746)
  (door r47 c d047)
  (door r47 r37 d3747)
  (door r48 r53 d4853)
  (door r49 r52 d4952)
  (door r49 r70 d4970)
  (door r50 r54 d5054)
  (door r50 r67 d5067)
  (door r51 r21 d2151)
  (door r51 r52 d5152)
  (door r51 r60 d5160)
  (door r52 r15 d1552)
  (door r52 r36 d3652)
  (door r52 r49 d4952)
  (door r52 r51 d5152)
  (door r52 r68 d5268)
  (door r53 r25 d2553)
  (door r53 r35 d3553)
  (door r53 r48 d4853)
  (door r54 r8 d854)
  (door r54 r50 d5054)
  (door r55 r13 d1355)
  (door r55 r15 d1555)
  (door r55 r40 d4055)
  (door r56 r14 d1456)
  (door r56 r30 d3056)
  (door r56 r60 d5660)
  (door r57 r43 d4357)
  (door r58 r37 d3758)
  (door r59 r14 d1459)
  (door r60 r51 d5160)
  (door r60 r56 d5660)
  (door r61 r69 d6169)
  (door r62 r33 d3362)
  (door r63 r65 d6365)
  (door r64 r36 d3664)
  (door r65 r31 d3165)
  (door r65 r63 d6365)
  (door r65 r66 d6566)
  (door r66 r1 d166)
  (door r66 r18 d1866)
  (door r66 r20 d2066)
  (door r66 r65 d6566)
  (door r67 r50 d5067)
  (door r68 r3 d368)
  (door r68 r33 d3368)
  (door r68 r52 d5268)
  (door r69 r27 d2769)
  (door r69 r61 d6169)
  (door r70 r8 d870)
  (door r70 r49 d4970)
  (closed d3553)
  (closed d2553)
  (closed d1545)
  (closed d2645)
  (closed d3145)
  (closed d3165)
  (closed d6365)
  (closed d2024)
  (closed d524)
  (closed d5054)
  (closed d3368)
  (closed d316)
  (closed d2333)
  (closed d1744)
  (closed d017)
  (closed d119)
  (closed d1024)
  (closed d3652)
  (closed d2439)
  (closed d1132)
  (closed d4357)
  (closed d921)
  (closed d3056)
  (closed d1555)
  (closed d6169)
  (closed d3747)
  (closed d3438)
  (closed d06)
  (closed d1355)
  (closed d3758)
  (in o1 r53)
  (in o2 r45)
  (in o3 r42)
  (in o4 r22)
  (in o5 r24)
  (in o6 r6)
  (in o7 r65)
  (in o8 r70)
  (in o9 r42)
  (in o10 r37)
  (in o11 r32)
  (in o12 r56)
  (in o13 r69)
  (in o14 r32)
  (in o15 r48)
  (in o16 r57)
  (in o17 r3)
  (in o18 r67)
  (in o19 r26)
  (in o20 r54)
  (in o21 r24)
  (in o22 r45)
  (in o23 r31)
  (in o24 r48)
  (in o25 r67)
  (in o26 r19)
  (in o27 r46)
  (in o28 r27)
  (in o29 r50)
  (in o30 r22)
  (in o31 r44)
  (in o32 r53)
  (in o33 r49)
  (in o34 r37)
  (in o35 r24)
  (in o36 r61)
  (in o37 r11)
  (in o38 r15)
  (in o39 r70)
  (in o40 r29)
  (in o41 r22)
  (in o42 r28)
  (in o43 r4)
  (in o44 r40)
  (in o45 r36)
  (in o46 r46)
  (in o47 r16)
  (in o48 r54)
  (in o49 r5)
  (in o50 r4)
  (in o51 r21)
  (in o52 r42)
  (in o53 r39)
  (in o54 r52)
  (in o55 r40)
  (in o56 r36)
  (in o57 r34)
  (in o58 r69)
  (in o59 r41)
  (in o60 r6)
  (in o61 r53)
  (in o62 r67)
  (in o63 r42)
  (in o64 r38)
  (in o65 r6)
  (in o66 r57)
  (in o67 r24)
  (in o68 r24)
  (in o69 r43)
  (in o70 r67))
 (:goal (and
         (in o1 r33)
         (in o2 r20)
         (in o3 r7)
         (in o4 r17)
         (in o5 r33)
         (in o6 r53)
         (in o7 r46)
         (in o8 r54)
         (in o9 r69)
         (in o10 r57)
         (in o11 r2)
         (in o12 r44)
         (in o13 r37)
         (in o14 r33)
         (in o15 r7)
         (in o16 r12)
         (in o17 r31)
         (in o18 r13)
         (in o19 r69)
         (in o20 r66)
         (in o21 r45)
         (in o22 r61)
         (in o23 r61)
         (in o24 r54)
         (in o25 r42)
         (in o26 r1)
         (in o27 r12)
         (in o28 r43)
         (in o29 r30)
         (in o30 r33)
         (in o31 r58)
         (in o32 r32)
         (in o33 r9)
         (in o34 r44)
         (in o35 r60)
         (in o36 r11)
         (in o37 r28)
         (in o38 r6)
         (in o39 r22)
         (in o40 r70)
         (in o41 r60)
         (in o42 r66)
         (in o43 r26)
         (in o44 r33)
         (in o45 r41)
         (in o46 r70)
         (in o47 r36)
         (in o48 r53)
         (in o49 r36)
         (in o50 r35)
         (in o51 r45)
         (in o52 r1)
         (in o53 r7)
         (in o54 r38)
         (in o55 r28)
         (in o56 r15)
         (in o57 r58)
         (in o58 r40)
         (in o59 r31)
         (in o60 r46)
         (in o61 r67)
         (in o62 r44)
         (in o63 r26)
         (in o64 r8)
         (in o65 r68)
         (in o66 r66)
         (in o67 r50)
         (in o68 r37)
         (in o69 r24)
         (in o70 r40))))