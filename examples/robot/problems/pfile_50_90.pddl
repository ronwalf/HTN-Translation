(define
 (problem pfile_50_90)
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
           - ROOM
           d3235
           d1432
           d1422
           d1421
           d2247
           d2335
           d1823
           d1839
           d639
           d2039
           d624
           d2437
           d2025
           d2546
           d1024
           d1049
           d949
           d929
           d09
           d1929
           d1931
           d1731
           d1318
           d314
           d344
           d1144
           d2739
           d127
           d2334
           d434
           d534
           d543
           d333
           d345
           d2845
           d2842
           d2848
           d4550
           d4050
           d240
           d230
           d1649
           d136
           d338
           d3841
           d1541
           d746
           d1217
           d2642
           d828
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r9 d09)
  (door r1 r27 d127)
  (door r1 r36 d136)
  (door r2 r30 d230)
  (door r2 r40 d240)
  (door r3 r14 d314)
  (door r3 r33 d333)
  (door r3 r38 d338)
  (door r3 r44 d344)
  (door r3 r45 d345)
  (door r4 r34 d434)
  (door r5 r34 d534)
  (door r5 r43 d543)
  (door r6 r24 d624)
  (door r6 r39 d639)
  (door r7 r46 d746)
  (door r8 r28 d828)
  (door r9 c d09)
  (door r9 r29 d929)
  (door r9 r49 d949)
  (door r10 r24 d1024)
  (door r10 r49 d1049)
  (door r11 r44 d1144)
  (door r12 r17 d1217)
  (door r13 r18 d1318)
  (door r14 r3 d314)
  (door r14 r21 d1421)
  (door r14 r22 d1422)
  (door r14 r32 d1432)
  (door r15 r41 d1541)
  (door r16 r49 d1649)
  (door r17 r12 d1217)
  (door r17 r31 d1731)
  (door r18 r13 d1318)
  (door r18 r23 d1823)
  (door r18 r39 d1839)
  (door r19 r29 d1929)
  (door r19 r31 d1931)
  (door r20 r25 d2025)
  (door r20 r39 d2039)
  (door r21 r14 d1421)
  (door r22 r14 d1422)
  (door r22 r47 d2247)
  (door r23 r18 d1823)
  (door r23 r34 d2334)
  (door r23 r35 d2335)
  (door r24 r6 d624)
  (door r24 r10 d1024)
  (door r24 r37 d2437)
  (door r25 r20 d2025)
  (door r25 r46 d2546)
  (door r26 r42 d2642)
  (door r27 r1 d127)
  (door r27 r39 d2739)
  (door r28 r8 d828)
  (door r28 r42 d2842)
  (door r28 r45 d2845)
  (door r28 r48 d2848)
  (door r29 r9 d929)
  (door r29 r19 d1929)
  (door r30 r2 d230)
  (door r31 r17 d1731)
  (door r31 r19 d1931)
  (door r32 r14 d1432)
  (door r32 r35 d3235)
  (door r33 r3 d333)
  (door r34 r4 d434)
  (door r34 r5 d534)
  (door r34 r23 d2334)
  (door r35 r23 d2335)
  (door r35 r32 d3235)
  (door r36 r1 d136)
  (door r37 r24 d2437)
  (door r38 r3 d338)
  (door r38 r41 d3841)
  (door r39 r6 d639)
  (door r39 r18 d1839)
  (door r39 r20 d2039)
  (door r39 r27 d2739)
  (door r40 r2 d240)
  (door r40 r50 d4050)
  (door r41 r15 d1541)
  (door r41 r38 d3841)
  (door r42 r26 d2642)
  (door r42 r28 d2842)
  (door r43 r5 d543)
  (door r44 r3 d344)
  (door r44 r11 d1144)
  (door r45 r3 d345)
  (door r45 r28 d2845)
  (door r45 r50 d4550)
  (door r46 r7 d746)
  (door r46 r25 d2546)
  (door r47 r22 d2247)
  (door r48 r28 d2848)
  (door r49 r9 d949)
  (door r49 r10 d1049)
  (door r49 r16 d1649)
  (door r50 r40 d4050)
  (door r50 r45 d4550)
  (closed d1432)
  (closed d1422)
  (closed d1421)
  (closed d2335)
  (closed d2039)
  (closed d2437)
  (closed d2025)
  (closed d929)
  (closed d09)
  (closed d1931)
  (closed d1731)
  (closed d1318)
  (closed d314)
  (closed d1144)
  (closed d434)
  (closed d534)
  (closed d543)
  (closed d2845)
  (closed d2842)
  (closed d4050)
  (closed d240)
  (closed d338)
  (closed d1541)
  (closed d746)
  (closed d1217)
  (closed d828)
  (in o1 r37)
  (in o2 r40)
  (in o3 r24)
  (in o4 r9)
  (in o5 r7)
  (in o6 r32)
  (in o7 r14)
  (in o8 r35)
  (in o9 r30)
  (in o10 r18)
  (in o11 r34)
  (in o12 r29)
  (in o13 r8)
  (in o14 r40)
  (in o15 r1)
  (in o16 r33)
  (in o17 r22)
  (in o18 r17)
  (in o19 r49)
  (in o20 r6)
  (in o21 r30)
  (in o22 r47)
  (in o23 r30)
  (in o24 r44)
  (in o25 r25)
  (in o26 r31)
  (in o27 r34)
  (in o28 r10)
  (in o29 r44)
  (in o30 r41)
  (in o31 r8)
  (in o32 r25)
  (in o33 r41)
  (in o34 r8)
  (in o35 r34)
  (in o36 r9)
  (in o37 r16)
  (in o38 r4)
  (in o39 r24)
  (in o40 r15)
  (in o41 r44)
  (in o42 r35)
  (in o43 r31)
  (in o44 r16)
  (in o45 r2)
  (in o46 r50)
  (in o47 r22)
  (in o48 r46)
  (in o49 r50)
  (in o50 r11)
  (in o51 r43)
  (in o52 r32)
  (in o53 r41)
  (in o54 r49)
  (in o55 r41)
  (in o56 r1)
  (in o57 r11)
  (in o58 r46)
  (in o59 r25)
  (in o60 r37)
  (in o61 r33)
  (in o62 r32)
  (in o63 r36)
  (in o64 r14)
  (in o65 r47)
  (in o66 r13)
  (in o67 r47)
  (in o68 r47)
  (in o69 r49)
  (in o70 r41)
  (in o71 r8)
  (in o72 r8)
  (in o73 r48)
  (in o74 r35)
  (in o75 r35)
  (in o76 r13)
  (in o77 r1)
  (in o78 r15)
  (in o79 r41)
  (in o80 r17)
  (in o81 r21)
  (in o82 r4)
  (in o83 r16)
  (in o84 r23)
  (in o85 r33)
  (in o86 r27)
  (in o87 r3)
  (in o88 r44)
  (in o89 r3)
  (in o90 r27))
 (:goal (and
         (in o1 r49)
         (in o2 r10)
         (in o3 r47)
         (in o4 r19)
         (in o5 r46)
         (in o6 r46)
         (in o7 r19)
         (in o8 r26)
         (in o9 r22)
         (in o10 r25)
         (in o11 r28)
         (in o12 r47)
         (in o13 r37)
         (in o14 r35)
         (in o15 r13)
         (in o16 r3)
         (in o17 r29)
         (in o18 r27)
         (in o19 r10)
         (in o20 r4)
         (in o21 r49)
         (in o22 r17)
         (in o23 r8)
         (in o24 r22)
         (in o25 r46)
         (in o26 r48)
         (in o27 r24)
         (in o28 r35)
         (in o29 r40)
         (in o30 r39)
         (in o31 r38)
         (in o32 r24)
         (in o33 r15)
         (in o34 r42)
         (in o35 r37)
         (in o36 r44)
         (in o37 r1)
         (in o38 r38)
         (in o39 r22)
         (in o40 r6)
         (in o41 r28)
         (in o42 r19)
         (in o43 r14)
         (in o44 r40)
         (in o45 r1)
         (in o46 r18)
         (in o47 r26)
         (in o48 r40)
         (in o49 r21)
         (in o50 r49)
         (in o51 r17)
         (in o52 r22)
         (in o53 r7)
         (in o54 r36)
         (in o55 r25)
         (in o56 r23)
         (in o57 r9)
         (in o58 r26)
         (in o59 r27)
         (in o60 r23)
         (in o61 r16)
         (in o62 r45)
         (in o63 r11)
         (in o64 r38)
         (in o65 r12)
         (in o66 r39)
         (in o67 r32)
         (in o68 r40)
         (in o69 r17)
         (in o70 r48)
         (in o71 r40)
         (in o72 r36)
         (in o73 r34)
         (in o74 r18)
         (in o75 r6)
         (in o76 r12)
         (in o77 r15)
         (in o78 r40)
         (in o79 r42)
         (in o80 r10)
         (in o81 r44)
         (in o82 r16)
         (in o83 r49)
         (in o84 r37)
         (in o85 r37)
         (in o86 r25)
         (in o87 r40)
         (in o88 r35)
         (in o89 r13)
         (in o90 r35))))