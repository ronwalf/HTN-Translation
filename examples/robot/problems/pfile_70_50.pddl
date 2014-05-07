(define
 (problem pfile_70_50)
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
           d919
           d1970
           d270
           d256
           d4070
           d3040
           d4049
           d4957
           d4951
           d4351
           d1551
           d4960
           d2360
           d015
           d4651
           d2746
           d2023
           d1567
           d2151
           d2251
           d2262
           d1122
           d3162
           d3152
           d5258
           d3658
           d1736
           d1726
           d617
           d758
           d1422
           d664
           d4252
           d1135
           d2935
           d1235
           d1244
           d4453
           d6164
           d2054
           d4263
           d2452
           d2438
           d1838
           d1833
           d3233
           d713
           d458
           d4468
           d168
           d529
           d541
           d569
           d5969
           d5965
           d365
           d3334
           d3447
           d637
           d1643
           d3450
           d842
           d866
           d1054
           d4255
           d4865
           d2548
           d2863
           d3953
           d345
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r15 d015)
  (door r1 r68 d168)
  (door r2 r56 d256)
  (door r2 r70 d270)
  (door r3 r45 d345)
  (door r3 r65 d365)
  (door r4 r58 d458)
  (door r5 r29 d529)
  (door r5 r41 d541)
  (door r5 r69 d569)
  (door r6 r17 d617)
  (door r6 r37 d637)
  (door r6 r64 d664)
  (door r7 r13 d713)
  (door r7 r58 d758)
  (door r8 r42 d842)
  (door r8 r66 d866)
  (door r9 r19 d919)
  (door r10 r54 d1054)
  (door r11 r22 d1122)
  (door r11 r35 d1135)
  (door r12 r35 d1235)
  (door r12 r44 d1244)
  (door r13 r7 d713)
  (door r14 r22 d1422)
  (door r15 c d015)
  (door r15 r51 d1551)
  (door r15 r67 d1567)
  (door r16 r43 d1643)
  (door r17 r6 d617)
  (door r17 r26 d1726)
  (door r17 r36 d1736)
  (door r18 r33 d1833)
  (door r18 r38 d1838)
  (door r19 r9 d919)
  (door r19 r70 d1970)
  (door r20 r23 d2023)
  (door r20 r54 d2054)
  (door r21 r51 d2151)
  (door r22 r11 d1122)
  (door r22 r14 d1422)
  (door r22 r51 d2251)
  (door r22 r62 d2262)
  (door r23 r20 d2023)
  (door r23 r60 d2360)
  (door r24 r38 d2438)
  (door r24 r52 d2452)
  (door r25 r48 d2548)
  (door r26 r17 d1726)
  (door r27 r46 d2746)
  (door r28 r63 d2863)
  (door r29 r5 d529)
  (door r29 r35 d2935)
  (door r30 r40 d3040)
  (door r31 r52 d3152)
  (door r31 r62 d3162)
  (door r32 r33 d3233)
  (door r33 r18 d1833)
  (door r33 r32 d3233)
  (door r33 r34 d3334)
  (door r34 r33 d3334)
  (door r34 r47 d3447)
  (door r34 r50 d3450)
  (door r35 r11 d1135)
  (door r35 r12 d1235)
  (door r35 r29 d2935)
  (door r36 r17 d1736)
  (door r36 r58 d3658)
  (door r37 r6 d637)
  (door r38 r18 d1838)
  (door r38 r24 d2438)
  (door r39 r53 d3953)
  (door r40 r30 d3040)
  (door r40 r49 d4049)
  (door r40 r70 d4070)
  (door r41 r5 d541)
  (door r42 r8 d842)
  (door r42 r52 d4252)
  (door r42 r55 d4255)
  (door r42 r63 d4263)
  (door r43 r16 d1643)
  (door r43 r51 d4351)
  (door r44 r12 d1244)
  (door r44 r53 d4453)
  (door r44 r68 d4468)
  (door r45 r3 d345)
  (door r46 r27 d2746)
  (door r46 r51 d4651)
  (door r47 r34 d3447)
  (door r48 r25 d2548)
  (door r48 r65 d4865)
  (door r49 r40 d4049)
  (door r49 r51 d4951)
  (door r49 r57 d4957)
  (door r49 r60 d4960)
  (door r50 r34 d3450)
  (door r51 r15 d1551)
  (door r51 r21 d2151)
  (door r51 r22 d2251)
  (door r51 r43 d4351)
  (door r51 r46 d4651)
  (door r51 r49 d4951)
  (door r52 r24 d2452)
  (door r52 r31 d3152)
  (door r52 r42 d4252)
  (door r52 r58 d5258)
  (door r53 r39 d3953)
  (door r53 r44 d4453)
  (door r54 r10 d1054)
  (door r54 r20 d2054)
  (door r55 r42 d4255)
  (door r56 r2 d256)
  (door r57 r49 d4957)
  (door r58 r4 d458)
  (door r58 r7 d758)
  (door r58 r36 d3658)
  (door r58 r52 d5258)
  (door r59 r65 d5965)
  (door r59 r69 d5969)
  (door r60 r23 d2360)
  (door r60 r49 d4960)
  (door r61 r64 d6164)
  (door r62 r22 d2262)
  (door r62 r31 d3162)
  (door r63 r28 d2863)
  (door r63 r42 d4263)
  (door r64 r6 d664)
  (door r64 r61 d6164)
  (door r65 r3 d365)
  (door r65 r48 d4865)
  (door r65 r59 d5965)
  (door r66 r8 d866)
  (door r67 r15 d1567)
  (door r68 r1 d168)
  (door r68 r44 d4468)
  (door r69 r5 d569)
  (door r69 r59 d5969)
  (door r70 r2 d270)
  (door r70 r19 d1970)
  (door r70 r40 d4070)
  (closed d1970)
  (closed d4049)
  (closed d4960)
  (closed d2360)
  (closed d4651)
  (closed d2023)
  (closed d2151)
  (closed d2251)
  (closed d2262)
  (closed d1122)
  (closed d3152)
  (closed d5258)
  (closed d3658)
  (closed d617)
  (closed d758)
  (closed d664)
  (closed d4252)
  (closed d1135)
  (closed d1244)
  (closed d4453)
  (closed d6164)
  (closed d3233)
  (closed d529)
  (closed d541)
  (closed d569)
  (closed d5969)
  (closed d5965)
  (closed d3447)
  (closed d1643)
  (closed d3450)
  (closed d842)
  (closed d4255)
  (closed d2548)
  (in o1 r47)
  (in o2 r23)
  (in o3 r39)
  (in o4 r24)
  (in o5 r45)
  (in o6 r9)
  (in o7 r49)
  (in o8 r36)
  (in o9 r51)
  (in o10 r37)
  (in o11 r25)
  (in o12 r62)
  (in o13 r23)
  (in o14 r24)
  (in o15 r37)
  (in o16 r45)
  (in o17 r46)
  (in o18 r31)
  (in o19 r35)
  (in o20 r62)
  (in o21 r48)
  (in o22 r63)
  (in o23 r40)
  (in o24 r68)
  (in o25 r13)
  (in o26 r61)
  (in o27 r21)
  (in o28 r11)
  (in o29 r48)
  (in o30 r11)
  (in o31 r7)
  (in o32 r25)
  (in o33 r46)
  (in o34 r57)
  (in o35 r28)
  (in o36 r46)
  (in o37 r55)
  (in o38 r29)
  (in o39 r19)
  (in o40 r2)
  (in o41 r12)
  (in o42 r9)
  (in o43 r57)
  (in o44 r55)
  (in o45 r66)
  (in o46 r70)
  (in o47 r29)
  (in o48 r62)
  (in o49 r29)
  (in o50 r40))
 (:goal (and
         (in o1 r54)
         (in o2 r27)
         (in o3 r67)
         (in o4 r60)
         (in o5 r4)
         (in o6 r49)
         (in o7 r21)
         (in o8 r56)
         (in o9 r28)
         (in o10 r64)
         (in o11 r20)
         (in o12 r19)
         (in o13 r49)
         (in o14 r66)
         (in o15 r34)
         (in o16 r49)
         (in o17 r13)
         (in o18 r29)
         (in o19 r35)
         (in o20 r55)
         (in o21 r44)
         (in o22 r32)
         (in o23 r35)
         (in o24 r1)
         (in o25 r60)
         (in o26 r15)
         (in o27 r2)
         (in o28 r32)
         (in o29 r22)
         (in o30 r48)
         (in o31 r29)
         (in o32 r30)
         (in o33 r42)
         (in o34 r43)
         (in o35 r12)
         (in o36 r67)
         (in o37 r2)
         (in o38 r5)
         (in o39 r1)
         (in o40 r70)
         (in o41 r47)
         (in o42 r5)
         (in o43 r55)
         (in o44 r34)
         (in o45 r18)
         (in o46 r70)
         (in o47 r53)
         (in o48 r49)
         (in o49 r50)
         (in o50 r45))))