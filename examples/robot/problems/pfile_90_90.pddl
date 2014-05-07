(define
 (problem pfile_90_90)
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
           r71
           r72
           r73
           r74
           r75
           r76
           r77
           r78
           r79
           r80
           r81
           r82
           r83
           r84
           r85
           r86
           r87
           r88
           r89
           r90
           - ROOM
           d6172
           d4461
           d4467
           d2644
           d2671
           d2684
           d8490
           d490
           d48
           d2670
           d3161
           d3166
           d1831
           d6183
           d6570
           d065
           d2470
           d2486
           d2450
           d5060
           d3660
           d3643
           d4355
           d2236
           d2249
           d1572
           d115
           d180
           d2481
           d3481
           d2674
           d4660
           d5661
           d3255
           d3641
           d1781
           d1758
           d3358
           d3337
           d1258
           d1279
           d979
           d212
           d3979
           d214
           d1463
           d4279
           d3879
           d2738
           d3539
           d823
           d6364
           d7380
           d7378
           d6273
           d2862
           d778
           d2149
           d5184
           d342
           d6990
           d6988
           d1669
           d2588
           d1069
           d1085
           d4769
           d1659
           d2931
           d4569
           d8082
           d8990
           d689
           d687
           d2046
           d583
           d576
           d548
           d5273
           d6168
           d5768
           d2775
           d4076
           d1352
           d4054
           d6177
           d2653
           d3053
           d011
           d1953
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r11 d011)
  (door c r65 d065)
  (door r1 r15 d115)
  (door r1 r80 d180)
  (door r2 r12 d212)
  (door r2 r14 d214)
  (door r3 r42 d342)
  (door r4 r8 d48)
  (door r4 r90 d490)
  (door r5 r48 d548)
  (door r5 r76 d576)
  (door r5 r83 d583)
  (door r6 r87 d687)
  (door r6 r89 d689)
  (door r7 r78 d778)
  (door r8 r4 d48)
  (door r8 r23 d823)
  (door r9 r79 d979)
  (door r10 r69 d1069)
  (door r10 r85 d1085)
  (door r11 c d011)
  (door r12 r2 d212)
  (door r12 r58 d1258)
  (door r12 r79 d1279)
  (door r13 r52 d1352)
  (door r14 r2 d214)
  (door r14 r63 d1463)
  (door r15 r1 d115)
  (door r15 r72 d1572)
  (door r16 r59 d1659)
  (door r16 r69 d1669)
  (door r17 r58 d1758)
  (door r17 r81 d1781)
  (door r18 r31 d1831)
  (door r19 r53 d1953)
  (door r20 r46 d2046)
  (door r21 r49 d2149)
  (door r22 r36 d2236)
  (door r22 r49 d2249)
  (door r23 r8 d823)
  (door r24 r50 d2450)
  (door r24 r70 d2470)
  (door r24 r81 d2481)
  (door r24 r86 d2486)
  (door r25 r88 d2588)
  (door r26 r44 d2644)
  (door r26 r53 d2653)
  (door r26 r70 d2670)
  (door r26 r71 d2671)
  (door r26 r74 d2674)
  (door r26 r84 d2684)
  (door r27 r38 d2738)
  (door r27 r75 d2775)
  (door r28 r62 d2862)
  (door r29 r31 d2931)
  (door r30 r53 d3053)
  (door r31 r18 d1831)
  (door r31 r29 d2931)
  (door r31 r61 d3161)
  (door r31 r66 d3166)
  (door r32 r55 d3255)
  (door r33 r37 d3337)
  (door r33 r58 d3358)
  (door r34 r81 d3481)
  (door r35 r39 d3539)
  (door r36 r22 d2236)
  (door r36 r41 d3641)
  (door r36 r43 d3643)
  (door r36 r60 d3660)
  (door r37 r33 d3337)
  (door r38 r27 d2738)
  (door r38 r79 d3879)
  (door r39 r35 d3539)
  (door r39 r79 d3979)
  (door r40 r54 d4054)
  (door r40 r76 d4076)
  (door r41 r36 d3641)
  (door r42 r3 d342)
  (door r42 r79 d4279)
  (door r43 r36 d3643)
  (door r43 r55 d4355)
  (door r44 r26 d2644)
  (door r44 r61 d4461)
  (door r44 r67 d4467)
  (door r45 r69 d4569)
  (door r46 r20 d2046)
  (door r46 r60 d4660)
  (door r47 r69 d4769)
  (door r48 r5 d548)
  (door r49 r21 d2149)
  (door r49 r22 d2249)
  (door r50 r24 d2450)
  (door r50 r60 d5060)
  (door r51 r84 d5184)
  (door r52 r13 d1352)
  (door r52 r73 d5273)
  (door r53 r19 d1953)
  (door r53 r26 d2653)
  (door r53 r30 d3053)
  (door r54 r40 d4054)
  (door r55 r32 d3255)
  (door r55 r43 d4355)
  (door r56 r61 d5661)
  (door r57 r68 d5768)
  (door r58 r12 d1258)
  (door r58 r17 d1758)
  (door r58 r33 d3358)
  (door r59 r16 d1659)
  (door r60 r36 d3660)
  (door r60 r46 d4660)
  (door r60 r50 d5060)
  (door r61 r31 d3161)
  (door r61 r44 d4461)
  (door r61 r56 d5661)
  (door r61 r68 d6168)
  (door r61 r72 d6172)
  (door r61 r77 d6177)
  (door r61 r83 d6183)
  (door r62 r28 d2862)
  (door r62 r73 d6273)
  (door r63 r14 d1463)
  (door r63 r64 d6364)
  (door r64 r63 d6364)
  (door r65 c d065)
  (door r65 r70 d6570)
  (door r66 r31 d3166)
  (door r67 r44 d4467)
  (door r68 r57 d5768)
  (door r68 r61 d6168)
  (door r69 r10 d1069)
  (door r69 r16 d1669)
  (door r69 r45 d4569)
  (door r69 r47 d4769)
  (door r69 r88 d6988)
  (door r69 r90 d6990)
  (door r70 r24 d2470)
  (door r70 r26 d2670)
  (door r70 r65 d6570)
  (door r71 r26 d2671)
  (door r72 r15 d1572)
  (door r72 r61 d6172)
  (door r73 r52 d5273)
  (door r73 r62 d6273)
  (door r73 r78 d7378)
  (door r73 r80 d7380)
  (door r74 r26 d2674)
  (door r75 r27 d2775)
  (door r76 r5 d576)
  (door r76 r40 d4076)
  (door r77 r61 d6177)
  (door r78 r7 d778)
  (door r78 r73 d7378)
  (door r79 r9 d979)
  (door r79 r12 d1279)
  (door r79 r38 d3879)
  (door r79 r39 d3979)
  (door r79 r42 d4279)
  (door r80 r1 d180)
  (door r80 r73 d7380)
  (door r80 r82 d8082)
  (door r81 r17 d1781)
  (door r81 r24 d2481)
  (door r81 r34 d3481)
  (door r82 r80 d8082)
  (door r83 r5 d583)
  (door r83 r61 d6183)
  (door r84 r26 d2684)
  (door r84 r51 d5184)
  (door r84 r90 d8490)
  (door r85 r10 d1085)
  (door r86 r24 d2486)
  (door r87 r6 d687)
  (door r88 r25 d2588)
  (door r88 r69 d6988)
  (door r89 r6 d689)
  (door r89 r90 d8990)
  (door r90 r4 d490)
  (door r90 r69 d6990)
  (door r90 r84 d8490)
  (door r90 r89 d8990)
  (closed d6172)
  (closed d4467)
  (closed d2644)
  (closed d8490)
  (closed d490)
  (closed d2670)
  (closed d1831)
  (closed d6183)
  (closed d6570)
  (closed d065)
  (closed d2450)
  (closed d3660)
  (closed d2236)
  (closed d2249)
  (closed d180)
  (closed d2481)
  (closed d3255)
  (closed d1781)
  (closed d1758)
  (closed d3979)
  (closed d1463)
  (closed d4279)
  (closed d3879)
  (closed d6364)
  (closed d7380)
  (closed d7378)
  (closed d6273)
  (closed d2862)
  (closed d2149)
  (closed d6988)
  (closed d2588)
  (closed d1085)
  (closed d4769)
  (closed d2931)
  (closed d4569)
  (closed d687)
  (closed d548)
  (closed d1352)
  (closed d6177)
  (closed d2653)
  (closed d011)
  (in o1 r7)
  (in o2 r47)
  (in o3 r63)
  (in o4 r45)
  (in o5 r71)
  (in o6 r40)
  (in o7 r36)
  (in o8 r90)
  (in o9 r70)
  (in o10 r9)
  (in o11 r43)
  (in o12 r7)
  (in o13 r58)
  (in o14 r40)
  (in o15 r74)
  (in o16 r37)
  (in o17 r50)
  (in o18 r73)
  (in o19 r48)
  (in o20 r55)
  (in o21 r16)
  (in o22 r22)
  (in o23 r51)
  (in o24 r58)
  (in o25 r1)
  (in o26 r13)
  (in o27 r89)
  (in o28 r33)
  (in o29 r79)
  (in o30 r20)
  (in o31 r66)
  (in o32 r75)
  (in o33 r19)
  (in o34 r29)
  (in o35 r65)
  (in o36 r5)
  (in o37 r70)
  (in o38 r70)
  (in o39 r5)
  (in o40 r51)
  (in o41 r64)
  (in o42 r60)
  (in o43 r54)
  (in o44 r52)
  (in o45 r69)
  (in o46 r46)
  (in o47 r83)
  (in o48 r64)
  (in o49 r5)
  (in o50 r84)
  (in o51 r7)
  (in o52 r35)
  (in o53 r88)
  (in o54 r32)
  (in o55 r13)
  (in o56 r40)
  (in o57 r9)
  (in o58 r32)
  (in o59 r45)
  (in o60 r63)
  (in o61 r30)
  (in o62 r4)
  (in o63 r76)
  (in o64 r90)
  (in o65 r74)
  (in o66 r6)
  (in o67 r27)
  (in o68 r68)
  (in o69 r86)
  (in o70 r36)
  (in o71 r69)
  (in o72 r32)
  (in o73 r82)
  (in o74 r48)
  (in o75 r19)
  (in o76 r87)
  (in o77 r47)
  (in o78 r19)
  (in o79 r11)
  (in o80 r21)
  (in o81 r51)
  (in o82 r29)
  (in o83 r82)
  (in o84 r50)
  (in o85 r86)
  (in o86 r40)
  (in o87 r79)
  (in o88 r59)
  (in o89 r52)
  (in o90 r30))
 (:goal (and
         (in o1 r19)
         (in o2 r35)
         (in o3 r83)
         (in o4 r46)
         (in o5 r47)
         (in o6 r68)
         (in o7 r75)
         (in o8 r66)
         (in o9 r37)
         (in o10 r43)
         (in o11 r38)
         (in o12 r43)
         (in o13 r71)
         (in o14 r45)
         (in o15 r41)
         (in o16 r73)
         (in o17 r38)
         (in o18 r23)
         (in o19 r22)
         (in o20 r6)
         (in o21 r26)
         (in o22 r51)
         (in o23 r55)
         (in o24 r25)
         (in o25 r71)
         (in o26 r2)
         (in o27 r19)
         (in o28 r11)
         (in o29 r80)
         (in o30 r79)
         (in o31 r9)
         (in o32 r15)
         (in o33 r71)
         (in o34 r11)
         (in o35 r24)
         (in o36 r14)
         (in o37 r76)
         (in o38 r66)
         (in o39 r59)
         (in o40 r18)
         (in o41 r8)
         (in o42 r33)
         (in o43 r81)
         (in o44 r88)
         (in o45 r7)
         (in o46 r74)
         (in o47 r66)
         (in o48 r17)
         (in o49 r48)
         (in o50 r89)
         (in o51 r80)
         (in o52 r63)
         (in o53 r26)
         (in o54 r38)
         (in o55 r18)
         (in o56 r70)
         (in o57 r68)
         (in o58 r63)
         (in o59 r25)
         (in o60 r83)
         (in o61 r28)
         (in o62 r27)
         (in o63 r44)
         (in o64 r88)
         (in o65 r66)
         (in o66 r65)
         (in o67 r54)
         (in o68 r18)
         (in o69 r12)
         (in o70 r22)
         (in o71 r7)
         (in o72 r26)
         (in o73 r37)
         (in o74 r61)
         (in o75 r71)
         (in o76 r49)
         (in o77 r8)
         (in o78 r57)
         (in o79 r22)
         (in o80 r74)
         (in o81 r45)
         (in o82 r5)
         (in o83 r71)
         (in o84 r14)
         (in o85 r87)
         (in o86 r3)
         (in o87 r61)
         (in o88 r18)
         (in o89 r49)
         (in o90 r57))))