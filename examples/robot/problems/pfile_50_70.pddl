(define
 (problem pfile_50_70)
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
           - ROOM
           d148
           d113
           d1748
           d1738
           d1735
           d3550
           d3450
           d3442
           d2634
           d1826
           d1841
           d1426
           d2629
           d2226
           d2026
           d2045
           d2937
           d3744
           d1144
           d1011
           d1030
           d3043
           d1016
           d748
           d2848
           d2430
           d2840
           d440
           d517
           d58
           d512
           d4246
           d1946
           d2546
           d625
           d1525
           d1321
           d2139
           d034
           d213
           d942
           d923
           d2327
           d4849
           d2047
           d13
           d1336
           d1933
           d3239
           d631
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r34 d034)
  (door r1 r3 d13)
  (door r1 r13 d113)
  (door r1 r48 d148)
  (door r2 r13 d213)
  (door r3 r1 d13)
  (door r4 r40 d440)
  (door r5 r8 d58)
  (door r5 r12 d512)
  (door r5 r17 d517)
  (door r6 r25 d625)
  (door r6 r31 d631)
  (door r7 r48 d748)
  (door r8 r5 d58)
  (door r9 r23 d923)
  (door r9 r42 d942)
  (door r10 r11 d1011)
  (door r10 r16 d1016)
  (door r10 r30 d1030)
  (door r11 r10 d1011)
  (door r11 r44 d1144)
  (door r12 r5 d512)
  (door r13 r1 d113)
  (door r13 r2 d213)
  (door r13 r21 d1321)
  (door r13 r36 d1336)
  (door r14 r26 d1426)
  (door r15 r25 d1525)
  (door r16 r10 d1016)
  (door r17 r5 d517)
  (door r17 r35 d1735)
  (door r17 r38 d1738)
  (door r17 r48 d1748)
  (door r18 r26 d1826)
  (door r18 r41 d1841)
  (door r19 r33 d1933)
  (door r19 r46 d1946)
  (door r20 r26 d2026)
  (door r20 r45 d2045)
  (door r20 r47 d2047)
  (door r21 r13 d1321)
  (door r21 r39 d2139)
  (door r22 r26 d2226)
  (door r23 r9 d923)
  (door r23 r27 d2327)
  (door r24 r30 d2430)
  (door r25 r6 d625)
  (door r25 r15 d1525)
  (door r25 r46 d2546)
  (door r26 r14 d1426)
  (door r26 r18 d1826)
  (door r26 r20 d2026)
  (door r26 r22 d2226)
  (door r26 r29 d2629)
  (door r26 r34 d2634)
  (door r27 r23 d2327)
  (door r28 r40 d2840)
  (door r28 r48 d2848)
  (door r29 r26 d2629)
  (door r29 r37 d2937)
  (door r30 r10 d1030)
  (door r30 r24 d2430)
  (door r30 r43 d3043)
  (door r31 r6 d631)
  (door r32 r39 d3239)
  (door r33 r19 d1933)
  (door r34 c d034)
  (door r34 r26 d2634)
  (door r34 r42 d3442)
  (door r34 r50 d3450)
  (door r35 r17 d1735)
  (door r35 r50 d3550)
  (door r36 r13 d1336)
  (door r37 r29 d2937)
  (door r37 r44 d3744)
  (door r38 r17 d1738)
  (door r39 r21 d2139)
  (door r39 r32 d3239)
  (door r40 r4 d440)
  (door r40 r28 d2840)
  (door r41 r18 d1841)
  (door r42 r9 d942)
  (door r42 r34 d3442)
  (door r42 r46 d4246)
  (door r43 r30 d3043)
  (door r44 r11 d1144)
  (door r44 r37 d3744)
  (door r45 r20 d2045)
  (door r46 r19 d1946)
  (door r46 r25 d2546)
  (door r46 r42 d4246)
  (door r47 r20 d2047)
  (door r48 r1 d148)
  (door r48 r7 d748)
  (door r48 r17 d1748)
  (door r48 r28 d2848)
  (door r48 r49 d4849)
  (door r49 r48 d4849)
  (door r50 r34 d3450)
  (door r50 r35 d3550)
  (closed d148)
  (closed d113)
  (closed d1738)
  (closed d2634)
  (closed d1826)
  (closed d1841)
  (closed d1426)
  (closed d2629)
  (closed d2045)
  (closed d2937)
  (closed d1144)
  (closed d1016)
  (closed d748)
  (closed d2848)
  (closed d2840)
  (closed d512)
  (closed d2546)
  (closed d625)
  (closed d2139)
  (closed d034)
  (closed d213)
  (closed d923)
  (closed d2047)
  (closed d13)
  (closed d1933)
  (closed d3239)
  (closed d631)
  (in o1 r12)
  (in o2 r36)
  (in o3 r16)
  (in o4 r3)
  (in o5 r9)
  (in o6 r33)
  (in o7 r32)
  (in o8 r12)
  (in o9 r35)
  (in o10 r15)
  (in o11 r9)
  (in o12 r40)
  (in o13 r3)
  (in o14 r39)
  (in o15 r9)
  (in o16 r39)
  (in o17 r16)
  (in o18 r50)
  (in o19 r1)
  (in o20 r1)
  (in o21 r50)
  (in o22 r14)
  (in o23 r27)
  (in o24 r17)
  (in o25 r41)
  (in o26 r39)
  (in o27 r24)
  (in o28 r9)
  (in o29 r18)
  (in o30 r50)
  (in o31 r5)
  (in o32 r47)
  (in o33 r48)
  (in o34 r1)
  (in o35 r3)
  (in o36 r40)
  (in o37 r43)
  (in o38 r10)
  (in o39 r5)
  (in o40 r44)
  (in o41 r6)
  (in o42 r32)
  (in o43 r11)
  (in o44 r7)
  (in o45 r9)
  (in o46 r45)
  (in o47 r40)
  (in o48 r1)
  (in o49 r6)
  (in o50 r5)
  (in o51 r35)
  (in o52 r16)
  (in o53 r41)
  (in o54 r47)
  (in o55 r1)
  (in o56 r23)
  (in o57 r10)
  (in o58 r2)
  (in o59 r33)
  (in o60 r39)
  (in o61 r4)
  (in o62 r5)
  (in o63 r19)
  (in o64 r15)
  (in o65 r6)
  (in o66 r21)
  (in o67 r6)
  (in o68 r20)
  (in o69 r4)
  (in o70 r33))
 (:goal (and
         (in o1 r30)
         (in o2 r42)
         (in o3 r27)
         (in o4 r50)
         (in o5 r46)
         (in o6 r32)
         (in o7 r34)
         (in o8 r18)
         (in o9 r18)
         (in o10 r5)
         (in o11 r5)
         (in o12 r24)
         (in o13 r26)
         (in o14 r20)
         (in o15 r26)
         (in o16 r10)
         (in o17 r32)
         (in o18 r3)
         (in o19 r34)
         (in o20 r29)
         (in o21 r34)
         (in o22 r10)
         (in o23 r13)
         (in o24 r34)
         (in o25 r27)
         (in o26 r41)
         (in o27 r31)
         (in o28 r8)
         (in o29 r3)
         (in o30 r15)
         (in o31 r32)
         (in o32 r7)
         (in o33 r48)
         (in o34 r23)
         (in o35 r4)
         (in o36 r34)
         (in o37 r10)
         (in o38 r5)
         (in o39 r48)
         (in o40 r8)
         (in o41 r17)
         (in o42 r8)
         (in o43 r20)
         (in o44 r49)
         (in o45 r3)
         (in o46 r35)
         (in o47 r48)
         (in o48 r36)
         (in o49 r40)
         (in o50 r37)
         (in o51 r16)
         (in o52 r29)
         (in o53 r4)
         (in o54 r27)
         (in o55 r24)
         (in o56 r29)
         (in o57 r35)
         (in o58 r48)
         (in o59 r34)
         (in o60 r12)
         (in o61 r29)
         (in o62 r13)
         (in o63 r31)
         (in o64 r24)
         (in o65 r16)
         (in o66 r31)
         (in o67 r3)
         (in o68 r7)
         (in o69 r43)
         (in o70 r29))))