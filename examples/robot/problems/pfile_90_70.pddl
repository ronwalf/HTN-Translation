(define
 (problem pfile_90_70)
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
           d6370
           d5263
           d3052
           d3089
           d4789
           d789
           d744
           d4458
           d258
           d5868
           d3758
           d3787
           d4387
           d187
           d2287
           d2259
           d1444
           d1425
           d2550
           d5065
           d2025
           d2551
           d1568
           d915
           d983
           d975
           d4883
           d4857
           d2657
           d2673
           d1562
           d6272
           d6286
           d386
           d1574
           d6876
           d7186
           d671
           d4864
           d864
           d5564
           d3955
           d2540
           d426
           d481
           d6781
           d2740
           d2761
           d3861
           d1531
           d3141
           d2628
           d1828
           d2860
           d4660
           d3343
           d2229
           d45
           d4654
           d510
           d1082
           d4572
           d4588
           d018
           d1283
           d3647
           d4078
           d6884
           d3554
           d3542
           d269
           d7477
           d1165
           d1957
           d5386
           d1449
           d156
           d5680
           d3280
           d2866
           d1666
           d1421
           d1358
           d1787
           d5579
           d8085
           d3488
           d6290
           d2480
           d2223
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r18 d018)
  (door r1 r56 d156)
  (door r1 r87 d187)
  (door r2 r58 d258)
  (door r2 r69 d269)
  (door r3 r86 d386)
  (door r4 r5 d45)
  (door r4 r26 d426)
  (door r4 r81 d481)
  (door r5 r4 d45)
  (door r5 r10 d510)
  (door r6 r71 d671)
  (door r7 r44 d744)
  (door r7 r89 d789)
  (door r8 r64 d864)
  (door r9 r15 d915)
  (door r9 r75 d975)
  (door r9 r83 d983)
  (door r10 r5 d510)
  (door r10 r82 d1082)
  (door r11 r65 d1165)
  (door r12 r83 d1283)
  (door r13 r58 d1358)
  (door r14 r21 d1421)
  (door r14 r25 d1425)
  (door r14 r44 d1444)
  (door r14 r49 d1449)
  (door r15 r9 d915)
  (door r15 r31 d1531)
  (door r15 r62 d1562)
  (door r15 r68 d1568)
  (door r15 r74 d1574)
  (door r16 r66 d1666)
  (door r17 r87 d1787)
  (door r18 c d018)
  (door r18 r28 d1828)
  (door r19 r57 d1957)
  (door r20 r25 d2025)
  (door r21 r14 d1421)
  (door r22 r23 d2223)
  (door r22 r29 d2229)
  (door r22 r59 d2259)
  (door r22 r87 d2287)
  (door r23 r22 d2223)
  (door r24 r80 d2480)
  (door r25 r14 d1425)
  (door r25 r20 d2025)
  (door r25 r40 d2540)
  (door r25 r50 d2550)
  (door r25 r51 d2551)
  (door r26 r4 d426)
  (door r26 r28 d2628)
  (door r26 r57 d2657)
  (door r26 r73 d2673)
  (door r27 r40 d2740)
  (door r27 r61 d2761)
  (door r28 r18 d1828)
  (door r28 r26 d2628)
  (door r28 r60 d2860)
  (door r28 r66 d2866)
  (door r29 r22 d2229)
  (door r30 r52 d3052)
  (door r30 r89 d3089)
  (door r31 r15 d1531)
  (door r31 r41 d3141)
  (door r32 r80 d3280)
  (door r33 r43 d3343)
  (door r34 r88 d3488)
  (door r35 r42 d3542)
  (door r35 r54 d3554)
  (door r36 r47 d3647)
  (door r37 r58 d3758)
  (door r37 r87 d3787)
  (door r38 r61 d3861)
  (door r39 r55 d3955)
  (door r40 r25 d2540)
  (door r40 r27 d2740)
  (door r40 r78 d4078)
  (door r41 r31 d3141)
  (door r42 r35 d3542)
  (door r43 r33 d3343)
  (door r43 r87 d4387)
  (door r44 r7 d744)
  (door r44 r14 d1444)
  (door r44 r58 d4458)
  (door r45 r72 d4572)
  (door r45 r88 d4588)
  (door r46 r54 d4654)
  (door r46 r60 d4660)
  (door r47 r36 d3647)
  (door r47 r89 d4789)
  (door r48 r57 d4857)
  (door r48 r64 d4864)
  (door r48 r83 d4883)
  (door r49 r14 d1449)
  (door r50 r25 d2550)
  (door r50 r65 d5065)
  (door r51 r25 d2551)
  (door r52 r30 d3052)
  (door r52 r63 d5263)
  (door r53 r86 d5386)
  (door r54 r35 d3554)
  (door r54 r46 d4654)
  (door r55 r39 d3955)
  (door r55 r64 d5564)
  (door r55 r79 d5579)
  (door r56 r1 d156)
  (door r56 r80 d5680)
  (door r57 r19 d1957)
  (door r57 r26 d2657)
  (door r57 r48 d4857)
  (door r58 r2 d258)
  (door r58 r13 d1358)
  (door r58 r37 d3758)
  (door r58 r44 d4458)
  (door r58 r68 d5868)
  (door r59 r22 d2259)
  (door r60 r28 d2860)
  (door r60 r46 d4660)
  (door r61 r27 d2761)
  (door r61 r38 d3861)
  (door r62 r15 d1562)
  (door r62 r72 d6272)
  (door r62 r86 d6286)
  (door r62 r90 d6290)
  (door r63 r52 d5263)
  (door r63 r70 d6370)
  (door r64 r8 d864)
  (door r64 r48 d4864)
  (door r64 r55 d5564)
  (door r65 r11 d1165)
  (door r65 r50 d5065)
  (door r66 r16 d1666)
  (door r66 r28 d2866)
  (door r67 r81 d6781)
  (door r68 r15 d1568)
  (door r68 r58 d5868)
  (door r68 r76 d6876)
  (door r68 r84 d6884)
  (door r69 r2 d269)
  (door r70 r63 d6370)
  (door r71 r6 d671)
  (door r71 r86 d7186)
  (door r72 r45 d4572)
  (door r72 r62 d6272)
  (door r73 r26 d2673)
  (door r74 r15 d1574)
  (door r74 r77 d7477)
  (door r75 r9 d975)
  (door r76 r68 d6876)
  (door r77 r74 d7477)
  (door r78 r40 d4078)
  (door r79 r55 d5579)
  (door r80 r24 d2480)
  (door r80 r32 d3280)
  (door r80 r56 d5680)
  (door r80 r85 d8085)
  (door r81 r4 d481)
  (door r81 r67 d6781)
  (door r82 r10 d1082)
  (door r83 r9 d983)
  (door r83 r12 d1283)
  (door r83 r48 d4883)
  (door r84 r68 d6884)
  (door r85 r80 d8085)
  (door r86 r3 d386)
  (door r86 r53 d5386)
  (door r86 r62 d6286)
  (door r86 r71 d7186)
  (door r87 r1 d187)
  (door r87 r17 d1787)
  (door r87 r22 d2287)
  (door r87 r37 d3787)
  (door r87 r43 d4387)
  (door r88 r34 d3488)
  (door r88 r45 d4588)
  (door r89 r7 d789)
  (door r89 r30 d3089)
  (door r89 r47 d4789)
  (door r90 r62 d6290)
  (closed d5263)
  (closed d3052)
  (closed d4789)
  (closed d789)
  (closed d4458)
  (closed d258)
  (closed d4387)
  (closed d187)
  (closed d1444)
  (closed d1425)
  (closed d2551)
  (closed d1568)
  (closed d915)
  (closed d975)
  (closed d2657)
  (closed d2673)
  (closed d6272)
  (closed d1574)
  (closed d671)
  (closed d864)
  (closed d5564)
  (closed d6781)
  (closed d2740)
  (closed d2761)
  (closed d1531)
  (closed d3141)
  (closed d1828)
  (closed d4660)
  (closed d510)
  (closed d1082)
  (closed d4588)
  (closed d3647)
  (closed d6884)
  (closed d3542)
  (closed d7477)
  (closed d1165)
  (closed d5386)
  (closed d1449)
  (closed d156)
  (closed d5680)
  (closed d3280)
  (closed d2866)
  (closed d1787)
  (closed d3488)
  (closed d6290)
  (closed d2223)
  (in o1 r63)
  (in o2 r50)
  (in o3 r61)
  (in o4 r1)
  (in o5 r34)
  (in o6 r71)
  (in o7 r37)
  (in o8 r72)
  (in o9 r26)
  (in o10 r56)
  (in o11 r58)
  (in o12 r33)
  (in o13 r30)
  (in o14 r57)
  (in o15 r73)
  (in o16 r81)
  (in o17 r15)
  (in o18 r44)
  (in o19 r32)
  (in o20 r45)
  (in o21 r26)
  (in o22 r15)
  (in o23 r2)
  (in o24 r41)
  (in o25 r42)
  (in o26 r80)
  (in o27 r8)
  (in o28 r2)
  (in o29 r8)
  (in o30 r70)
  (in o31 r84)
  (in o32 r45)
  (in o33 r2)
  (in o34 r35)
  (in o35 r27)
  (in o36 r66)
  (in o37 r45)
  (in o38 r62)
  (in o39 r54)
  (in o40 r6)
  (in o41 r33)
  (in o42 r13)
  (in o43 r24)
  (in o44 r24)
  (in o45 r76)
  (in o46 r72)
  (in o47 r50)
  (in o48 r22)
  (in o49 r73)
  (in o50 r67)
  (in o51 r78)
  (in o52 r88)
  (in o53 r80)
  (in o54 r26)
  (in o55 r35)
  (in o56 r48)
  (in o57 r17)
  (in o58 r29)
  (in o59 r10)
  (in o60 r17)
  (in o61 r14)
  (in o62 r76)
  (in o63 r51)
  (in o64 r67)
  (in o65 r50)
  (in o66 r60)
  (in o67 r85)
  (in o68 r4)
  (in o69 r60)
  (in o70 r7))
 (:goal (and
         (in o1 r64)
         (in o2 r15)
         (in o3 r89)
         (in o4 r51)
         (in o5 r83)
         (in o6 r23)
         (in o7 r44)
         (in o8 r24)
         (in o9 r50)
         (in o10 r5)
         (in o11 r73)
         (in o12 r43)
         (in o13 r18)
         (in o14 r16)
         (in o15 r17)
         (in o16 r1)
         (in o17 r8)
         (in o18 r67)
         (in o19 r29)
         (in o20 r28)
         (in o21 r54)
         (in o22 r37)
         (in o23 r50)
         (in o24 r87)
         (in o25 r44)
         (in o26 r14)
         (in o27 r9)
         (in o28 r68)
         (in o29 r10)
         (in o30 r67)
         (in o31 r81)
         (in o32 r75)
         (in o33 r45)
         (in o34 r23)
         (in o35 r7)
         (in o36 r24)
         (in o37 r20)
         (in o38 r33)
         (in o39 r51)
         (in o40 r28)
         (in o41 r80)
         (in o42 r38)
         (in o43 r57)
         (in o44 r44)
         (in o45 r49)
         (in o46 r48)
         (in o47 r36)
         (in o48 r68)
         (in o49 r44)
         (in o50 r65)
         (in o51 r56)
         (in o52 r71)
         (in o53 r33)
         (in o54 r87)
         (in o55 r26)
         (in o56 r84)
         (in o57 r11)
         (in o58 r41)
         (in o59 r7)
         (in o60 r45)
         (in o61 r3)
         (in o62 r80)
         (in o63 r66)
         (in o64 r42)
         (in o65 r64)
         (in o66 r53)
         (in o67 r2)
         (in o68 r40)
         (in o69 r49)
         (in o70 r8))))