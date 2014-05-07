(define
 (problem pfile_70_30)
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
           d1929
           d2949
           d649
           d646
           d2953
           d3153
           d3166
           d4353
           d2866
           d1128
           d1150
           d3850
           d5052
           d2552
           d338
           d327
           d369
           d527
           d2539
           d3955
           d1855
           d1862
           d439
           d447
           d1447
           d1458
           d911
           d5060
           d2660
           d2260
           d5065
           d04
           d1027
           d1044
           d1035
           d3567
           d1045
           d1267
           d1233
           d1333
           d1330
           d3037
           d2128
           d25
           d858
           d334
           d2559
           d3064
           d651
           d1560
           d160
           d5868
           d1963
           d763
           d2063
           d4243
           d1441
           d1336
           d1257
           d2061
           d623
           d1017
           d1348
           d3856
           d3246
           d6670
           d2470
           d3540
           d216
           d4954
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r4 d04)
  (door r1 r60 d160)
  (door r2 r5 d25)
  (door r2 r16 d216)
  (door r3 r27 d327)
  (door r3 r34 d334)
  (door r3 r38 d338)
  (door r3 r69 d369)
  (door r4 c d04)
  (door r4 r39 d439)
  (door r4 r47 d447)
  (door r5 r2 d25)
  (door r5 r27 d527)
  (door r6 r23 d623)
  (door r6 r46 d646)
  (door r6 r49 d649)
  (door r6 r51 d651)
  (door r7 r63 d763)
  (door r8 r58 d858)
  (door r9 r11 d911)
  (door r10 r17 d1017)
  (door r10 r27 d1027)
  (door r10 r35 d1035)
  (door r10 r44 d1044)
  (door r10 r45 d1045)
  (door r11 r9 d911)
  (door r11 r28 d1128)
  (door r11 r50 d1150)
  (door r12 r33 d1233)
  (door r12 r57 d1257)
  (door r12 r67 d1267)
  (door r13 r30 d1330)
  (door r13 r33 d1333)
  (door r13 r36 d1336)
  (door r13 r48 d1348)
  (door r14 r41 d1441)
  (door r14 r47 d1447)
  (door r14 r58 d1458)
  (door r15 r60 d1560)
  (door r16 r2 d216)
  (door r17 r10 d1017)
  (door r18 r55 d1855)
  (door r18 r62 d1862)
  (door r19 r29 d1929)
  (door r19 r63 d1963)
  (door r20 r61 d2061)
  (door r20 r63 d2063)
  (door r21 r28 d2128)
  (door r22 r60 d2260)
  (door r23 r6 d623)
  (door r24 r70 d2470)
  (door r25 r39 d2539)
  (door r25 r52 d2552)
  (door r25 r59 d2559)
  (door r26 r60 d2660)
  (door r27 r3 d327)
  (door r27 r5 d527)
  (door r27 r10 d1027)
  (door r28 r11 d1128)
  (door r28 r21 d2128)
  (door r28 r66 d2866)
  (door r29 r19 d1929)
  (door r29 r49 d2949)
  (door r29 r53 d2953)
  (door r30 r13 d1330)
  (door r30 r37 d3037)
  (door r30 r64 d3064)
  (door r31 r53 d3153)
  (door r31 r66 d3166)
  (door r32 r46 d3246)
  (door r33 r12 d1233)
  (door r33 r13 d1333)
  (door r34 r3 d334)
  (door r35 r10 d1035)
  (door r35 r40 d3540)
  (door r35 r67 d3567)
  (door r36 r13 d1336)
  (door r37 r30 d3037)
  (door r38 r3 d338)
  (door r38 r50 d3850)
  (door r38 r56 d3856)
  (door r39 r4 d439)
  (door r39 r25 d2539)
  (door r39 r55 d3955)
  (door r40 r35 d3540)
  (door r41 r14 d1441)
  (door r42 r43 d4243)
  (door r43 r42 d4243)
  (door r43 r53 d4353)
  (door r44 r10 d1044)
  (door r45 r10 d1045)
  (door r46 r6 d646)
  (door r46 r32 d3246)
  (door r47 r4 d447)
  (door r47 r14 d1447)
  (door r48 r13 d1348)
  (door r49 r6 d649)
  (door r49 r29 d2949)
  (door r49 r54 d4954)
  (door r50 r11 d1150)
  (door r50 r38 d3850)
  (door r50 r52 d5052)
  (door r50 r60 d5060)
  (door r50 r65 d5065)
  (door r51 r6 d651)
  (door r52 r25 d2552)
  (door r52 r50 d5052)
  (door r53 r29 d2953)
  (door r53 r31 d3153)
  (door r53 r43 d4353)
  (door r54 r49 d4954)
  (door r55 r18 d1855)
  (door r55 r39 d3955)
  (door r56 r38 d3856)
  (door r57 r12 d1257)
  (door r58 r8 d858)
  (door r58 r14 d1458)
  (door r58 r68 d5868)
  (door r59 r25 d2559)
  (door r60 r1 d160)
  (door r60 r15 d1560)
  (door r60 r22 d2260)
  (door r60 r26 d2660)
  (door r60 r50 d5060)
  (door r61 r20 d2061)
  (door r62 r18 d1862)
  (door r63 r7 d763)
  (door r63 r19 d1963)
  (door r63 r20 d2063)
  (door r64 r30 d3064)
  (door r65 r50 d5065)
  (door r66 r28 d2866)
  (door r66 r31 d3166)
  (door r66 r70 d6670)
  (door r67 r12 d1267)
  (door r67 r35 d3567)
  (door r68 r58 d5868)
  (door r69 r3 d369)
  (door r70 r24 d2470)
  (door r70 r66 d6670)
  (closed d2949)
  (closed d649)
  (closed d646)
  (closed d3153)
  (closed d3166)
  (closed d4353)
  (closed d1128)
  (closed d1150)
  (closed d3850)
  (closed d5052)
  (closed d338)
  (closed d369)
  (closed d527)
  (closed d3955)
  (closed d1855)
  (closed d439)
  (closed d447)
  (closed d1458)
  (closed d911)
  (closed d5065)
  (closed d04)
  (closed d1045)
  (closed d1333)
  (closed d1330)
  (closed d3037)
  (closed d1560)
  (closed d160)
  (closed d5868)
  (closed d763)
  (closed d2063)
  (closed d1348)
  (closed d2470)
  (closed d4954)
  (in o1 r65)
  (in o2 r55)
  (in o3 r18)
  (in o4 r12)
  (in o5 r20)
  (in o6 r23)
  (in o7 r58)
  (in o8 r37)
  (in o9 r42)
  (in o10 r26)
  (in o11 r5)
  (in o12 r27)
  (in o13 r59)
  (in o14 r52)
  (in o15 r65)
  (in o16 r7)
  (in o17 r59)
  (in o18 r19)
  (in o19 r21)
  (in o20 r69)
  (in o21 r14)
  (in o22 r38)
  (in o23 r1)
  (in o24 r68)
  (in o25 r44)
  (in o26 r38)
  (in o27 r38)
  (in o28 r68)
  (in o29 r12)
  (in o30 r32))
 (:goal (and
         (in o1 r57)
         (in o2 r53)
         (in o3 r65)
         (in o4 r52)
         (in o5 r64)
         (in o6 r67)
         (in o7 r58)
         (in o8 r55)
         (in o9 r36)
         (in o10 r3)
         (in o11 r17)
         (in o12 r62)
         (in o13 r56)
         (in o14 r70)
         (in o15 r64)
         (in o16 r54)
         (in o17 r18)
         (in o18 r33)
         (in o19 r20)
         (in o20 r2)
         (in o21 r12)
         (in o22 r44)
         (in o23 r19)
         (in o24 r32)
         (in o25 r67)
         (in o26 r37)
         (in o27 r48)
         (in o28 r26)
         (in o29 r38)
         (in o30 r1))))