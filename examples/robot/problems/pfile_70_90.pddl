(define
 (problem pfile_70_90)
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
           - ROOM
           d3753
           d3752
           d4252
           d2542
           d2539
           d1452
           d1417
           d1721
           d2154
           d1751
           d1463
           d063
           d4255
           d4955
           d3649
           d3855
           d3850
           d1055
           d3860
           d955
           d962
           d2462
           d2470
           d1236
           d5558
           d5859
           d5964
           d2359
           d2348
           d423
           d431
           d1554
           d615
           d618
           d1847
           d4856
           d3256
           d3265
           d347
           d1428
           d4547
           d845
           d834
           d4156
           d1840
           d763
           d3360
           d3369
           d916
           d1261
           d3061
           d1349
           d1843
           d1943
           d2770
           d2735
           d344
           d536
           d566
           d3267
           d029
           d1161
           d1157
           d2264
           d5668
           d4669
           d162
           d2033
           d24
           d2670
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r29 d029)
  (door c r63 d063)
  (door r1 r62 d162)
  (door r2 r4 d24)
  (door r3 r44 d344)
  (door r3 r47 d347)
  (door r4 r2 d24)
  (door r4 r23 d423)
  (door r4 r31 d431)
  (door r5 r36 d536)
  (door r5 r66 d566)
  (door r6 r15 d615)
  (door r6 r18 d618)
  (door r7 r63 d763)
  (door r8 r34 d834)
  (door r8 r45 d845)
  (door r9 r16 d916)
  (door r9 r55 d955)
  (door r9 r62 d962)
  (door r10 r55 d1055)
  (door r11 r57 d1157)
  (door r11 r61 d1161)
  (door r12 r36 d1236)
  (door r12 r61 d1261)
  (door r13 r49 d1349)
  (door r14 r17 d1417)
  (door r14 r28 d1428)
  (door r14 r52 d1452)
  (door r14 r63 d1463)
  (door r15 r6 d615)
  (door r15 r54 d1554)
  (door r16 r9 d916)
  (door r17 r14 d1417)
  (door r17 r21 d1721)
  (door r17 r51 d1751)
  (door r18 r6 d618)
  (door r18 r40 d1840)
  (door r18 r43 d1843)
  (door r18 r47 d1847)
  (door r19 r43 d1943)
  (door r20 r33 d2033)
  (door r21 r17 d1721)
  (door r21 r54 d2154)
  (door r22 r64 d2264)
  (door r23 r4 d423)
  (door r23 r48 d2348)
  (door r23 r59 d2359)
  (door r24 r62 d2462)
  (door r24 r70 d2470)
  (door r25 r39 d2539)
  (door r25 r42 d2542)
  (door r26 r70 d2670)
  (door r27 r35 d2735)
  (door r27 r70 d2770)
  (door r28 r14 d1428)
  (door r29 c d029)
  (door r30 r61 d3061)
  (door r31 r4 d431)
  (door r32 r56 d3256)
  (door r32 r65 d3265)
  (door r32 r67 d3267)
  (door r33 r20 d2033)
  (door r33 r60 d3360)
  (door r33 r69 d3369)
  (door r34 r8 d834)
  (door r35 r27 d2735)
  (door r36 r5 d536)
  (door r36 r12 d1236)
  (door r36 r49 d3649)
  (door r37 r52 d3752)
  (door r37 r53 d3753)
  (door r38 r50 d3850)
  (door r38 r55 d3855)
  (door r38 r60 d3860)
  (door r39 r25 d2539)
  (door r40 r18 d1840)
  (door r41 r56 d4156)
  (door r42 r25 d2542)
  (door r42 r52 d4252)
  (door r42 r55 d4255)
  (door r43 r18 d1843)
  (door r43 r19 d1943)
  (door r44 r3 d344)
  (door r45 r8 d845)
  (door r45 r47 d4547)
  (door r46 r69 d4669)
  (door r47 r3 d347)
  (door r47 r18 d1847)
  (door r47 r45 d4547)
  (door r48 r23 d2348)
  (door r48 r56 d4856)
  (door r49 r13 d1349)
  (door r49 r36 d3649)
  (door r49 r55 d4955)
  (door r50 r38 d3850)
  (door r51 r17 d1751)
  (door r52 r14 d1452)
  (door r52 r37 d3752)
  (door r52 r42 d4252)
  (door r53 r37 d3753)
  (door r54 r15 d1554)
  (door r54 r21 d2154)
  (door r55 r9 d955)
  (door r55 r10 d1055)
  (door r55 r38 d3855)
  (door r55 r42 d4255)
  (door r55 r49 d4955)
  (door r55 r58 d5558)
  (door r56 r32 d3256)
  (door r56 r41 d4156)
  (door r56 r48 d4856)
  (door r56 r68 d5668)
  (door r57 r11 d1157)
  (door r58 r55 d5558)
  (door r58 r59 d5859)
  (door r59 r23 d2359)
  (door r59 r58 d5859)
  (door r59 r64 d5964)
  (door r60 r33 d3360)
  (door r60 r38 d3860)
  (door r61 r11 d1161)
  (door r61 r12 d1261)
  (door r61 r30 d3061)
  (door r62 r1 d162)
  (door r62 r9 d962)
  (door r62 r24 d2462)
  (door r63 c d063)
  (door r63 r7 d763)
  (door r63 r14 d1463)
  (door r64 r22 d2264)
  (door r64 r59 d5964)
  (door r65 r32 d3265)
  (door r66 r5 d566)
  (door r67 r32 d3267)
  (door r68 r56 d5668)
  (door r69 r33 d3369)
  (door r69 r46 d4669)
  (door r70 r24 d2470)
  (door r70 r26 d2670)
  (door r70 r27 d2770)
  (closed d2539)
  (closed d1452)
  (closed d1417)
  (closed d1721)
  (closed d2154)
  (closed d1751)
  (closed d1463)
  (closed d063)
  (closed d4255)
  (closed d4955)
  (closed d3855)
  (closed d3860)
  (closed d955)
  (closed d962)
  (closed d2470)
  (closed d5859)
  (closed d2359)
  (closed d2348)
  (closed d615)
  (closed d4856)
  (closed d3256)
  (closed d3265)
  (closed d4547)
  (closed d845)
  (closed d834)
  (closed d4156)
  (closed d1840)
  (closed d3360)
  (closed d3369)
  (closed d3061)
  (closed d1349)
  (closed d1843)
  (closed d1943)
  (closed d2735)
  (closed d029)
  (closed d1161)
  (closed d2264)
  (closed d5668)
  (closed d162)
  (closed d2033)
  (closed d2670)
  (in o1 r61)
  (in o2 r18)
  (in o3 r66)
  (in o4 r9)
  (in o5 r30)
  (in o6 r68)
  (in o7 r59)
  (in o8 r22)
  (in o9 r7)
  (in o10 r63)
  (in o11 r4)
  (in o12 r28)
  (in o13 r57)
  (in o14 r36)
  (in o15 r60)
  (in o16 r63)
  (in o17 r53)
  (in o18 r30)
  (in o19 r48)
  (in o20 r15)
  (in o21 r10)
  (in o22 r50)
  (in o23 r61)
  (in o24 r13)
  (in o25 r58)
  (in o26 r23)
  (in o27 r68)
  (in o28 r65)
  (in o29 r36)
  (in o30 r10)
  (in o31 r40)
  (in o32 r68)
  (in o33 r43)
  (in o34 r69)
  (in o35 r59)
  (in o36 r68)
  (in o37 r30)
  (in o38 r56)
  (in o39 r22)
  (in o40 r1)
  (in o41 r11)
  (in o42 r25)
  (in o43 r59)
  (in o44 r54)
  (in o45 r61)
  (in o46 r56)
  (in o47 r18)
  (in o48 r6)
  (in o49 r46)
  (in o50 r65)
  (in o51 r39)
  (in o52 r17)
  (in o53 r2)
  (in o54 r48)
  (in o55 r4)
  (in o56 r56)
  (in o57 r52)
  (in o58 r13)
  (in o59 r26)
  (in o60 r67)
  (in o61 r61)
  (in o62 r8)
  (in o63 r49)
  (in o64 r20)
  (in o65 r32)
  (in o66 r39)
  (in o67 r55)
  (in o68 r7)
  (in o69 r23)
  (in o70 r35)
  (in o71 r41)
  (in o72 r18)
  (in o73 r67)
  (in o74 r20)
  (in o75 r63)
  (in o76 r47)
  (in o77 r6)
  (in o78 r18)
  (in o79 r21)
  (in o80 r55)
  (in o81 r40)
  (in o82 r66)
  (in o83 r51)
  (in o84 r7)
  (in o85 r25)
  (in o86 r1)
  (in o87 r27)
  (in o88 r61)
  (in o89 r59)
  (in o90 r9))
 (:goal (and
         (in o1 r66)
         (in o2 r28)
         (in o3 r60)
         (in o4 r55)
         (in o5 r9)
         (in o6 r15)
         (in o7 r49)
         (in o8 r60)
         (in o9 r58)
         (in o10 r42)
         (in o11 r70)
         (in o12 r27)
         (in o13 r39)
         (in o14 r6)
         (in o15 r66)
         (in o16 r44)
         (in o17 r30)
         (in o18 r54)
         (in o19 r43)
         (in o20 r2)
         (in o21 r18)
         (in o22 r45)
         (in o23 r67)
         (in o24 r40)
         (in o25 r48)
         (in o26 r39)
         (in o27 r7)
         (in o28 r68)
         (in o29 r8)
         (in o30 r50)
         (in o31 r2)
         (in o32 r41)
         (in o33 r45)
         (in o34 r19)
         (in o35 r27)
         (in o36 r68)
         (in o37 r60)
         (in o38 r23)
         (in o39 r59)
         (in o40 r30)
         (in o41 r32)
         (in o42 r60)
         (in o43 r22)
         (in o44 r35)
         (in o45 r64)
         (in o46 r16)
         (in o47 r69)
         (in o48 r24)
         (in o49 r39)
         (in o50 r7)
         (in o51 r15)
         (in o52 r33)
         (in o53 r31)
         (in o54 r8)
         (in o55 r14)
         (in o56 r62)
         (in o57 r27)
         (in o58 r43)
         (in o59 r27)
         (in o60 r64)
         (in o61 r33)
         (in o62 r46)
         (in o63 r31)
         (in o64 r49)
         (in o65 r45)
         (in o66 r21)
         (in o67 r7)
         (in o68 r31)
         (in o69 r5)
         (in o70 r12)
         (in o71 r5)
         (in o72 r51)
         (in o73 r68)
         (in o74 r54)
         (in o75 r21)
         (in o76 r42)
         (in o77 r12)
         (in o78 r23)
         (in o79 r59)
         (in o80 r30)
         (in o81 r58)
         (in o82 r26)
         (in o83 r24)
         (in o84 r46)
         (in o85 r22)
         (in o86 r30)
         (in o87 r57)
         (in o88 r45)
         (in o89 r24)
         (in o90 r69))))