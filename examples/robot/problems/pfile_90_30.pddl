(define
 (problem pfile_90_30)
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
           d3788
           d4288
           d1642
           d1672
           d5972
           d2559
           d2568
           d5059
           d3250
           d1368
           d3236
           d172
           d153
           d5375
           d7581
           d1175
           d253
           d1190
           d147
           d4053
           d4058
           d2958
           d1058
           d284
           d4184
           d7583
           d983
           d156
           d5676
           d441
           d3958
           d1161
           d460
           d1186
           d5286
           d1252
           d1267
           d5271
           d3352
           d444
           d1015
           d1535
           d315
           d354
           d5489
           d3551
           d2151
           d2169
           d6173
           d7385
           d4385
           d1843
           d1834
           d379
           d3464
           d667
           d4667
           d4579
           d4648
           d1748
           d1722
           d949
           d811
           d870
           d1131
           d2389
           d880
           d2680
           d1418
           d2754
           d1765
           d2538
           d4187
           d2876
           d2863
           d063
           d1937
           d1982
           d5582
           d2082
           d720
           d3082
           d2473
           d877
           d538
           d5758
           d5862
           d3978
           d6678
           d3174
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r63 d063)
  (door r1 r47 d147)
  (door r1 r53 d153)
  (door r1 r56 d156)
  (door r1 r72 d172)
  (door r2 r53 d253)
  (door r2 r84 d284)
  (door r3 r15 d315)
  (door r3 r54 d354)
  (door r3 r79 d379)
  (door r4 r41 d441)
  (door r4 r44 d444)
  (door r4 r60 d460)
  (door r5 r38 d538)
  (door r6 r67 d667)
  (door r7 r20 d720)
  (door r8 r11 d811)
  (door r8 r70 d870)
  (door r8 r77 d877)
  (door r8 r80 d880)
  (door r9 r49 d949)
  (door r9 r83 d983)
  (door r10 r15 d1015)
  (door r10 r58 d1058)
  (door r11 r8 d811)
  (door r11 r31 d1131)
  (door r11 r61 d1161)
  (door r11 r75 d1175)
  (door r11 r86 d1186)
  (door r11 r90 d1190)
  (door r12 r52 d1252)
  (door r12 r67 d1267)
  (door r13 r68 d1368)
  (door r14 r18 d1418)
  (door r15 r3 d315)
  (door r15 r10 d1015)
  (door r15 r35 d1535)
  (door r16 r42 d1642)
  (door r16 r72 d1672)
  (door r17 r22 d1722)
  (door r17 r48 d1748)
  (door r17 r65 d1765)
  (door r18 r14 d1418)
  (door r18 r34 d1834)
  (door r18 r43 d1843)
  (door r19 r37 d1937)
  (door r19 r82 d1982)
  (door r20 r7 d720)
  (door r20 r82 d2082)
  (door r21 r51 d2151)
  (door r21 r69 d2169)
  (door r22 r17 d1722)
  (door r23 r89 d2389)
  (door r24 r73 d2473)
  (door r25 r38 d2538)
  (door r25 r59 d2559)
  (door r25 r68 d2568)
  (door r26 r80 d2680)
  (door r27 r54 d2754)
  (door r28 r63 d2863)
  (door r28 r76 d2876)
  (door r29 r58 d2958)
  (door r30 r82 d3082)
  (door r31 r11 d1131)
  (door r31 r74 d3174)
  (door r32 r36 d3236)
  (door r32 r50 d3250)
  (door r33 r52 d3352)
  (door r34 r18 d1834)
  (door r34 r64 d3464)
  (door r35 r15 d1535)
  (door r35 r51 d3551)
  (door r36 r32 d3236)
  (door r37 r19 d1937)
  (door r37 r88 d3788)
  (door r38 r5 d538)
  (door r38 r25 d2538)
  (door r39 r58 d3958)
  (door r39 r78 d3978)
  (door r40 r53 d4053)
  (door r40 r58 d4058)
  (door r41 r4 d441)
  (door r41 r84 d4184)
  (door r41 r87 d4187)
  (door r42 r16 d1642)
  (door r42 r88 d4288)
  (door r43 r18 d1843)
  (door r43 r85 d4385)
  (door r44 r4 d444)
  (door r45 r79 d4579)
  (door r46 r48 d4648)
  (door r46 r67 d4667)
  (door r47 r1 d147)
  (door r48 r17 d1748)
  (door r48 r46 d4648)
  (door r49 r9 d949)
  (door r50 r32 d3250)
  (door r50 r59 d5059)
  (door r51 r21 d2151)
  (door r51 r35 d3551)
  (door r52 r12 d1252)
  (door r52 r33 d3352)
  (door r52 r71 d5271)
  (door r52 r86 d5286)
  (door r53 r1 d153)
  (door r53 r2 d253)
  (door r53 r40 d4053)
  (door r53 r75 d5375)
  (door r54 r3 d354)
  (door r54 r27 d2754)
  (door r54 r89 d5489)
  (door r55 r82 d5582)
  (door r56 r1 d156)
  (door r56 r76 d5676)
  (door r57 r58 d5758)
  (door r58 r10 d1058)
  (door r58 r29 d2958)
  (door r58 r39 d3958)
  (door r58 r40 d4058)
  (door r58 r57 d5758)
  (door r58 r62 d5862)
  (door r59 r25 d2559)
  (door r59 r50 d5059)
  (door r59 r72 d5972)
  (door r60 r4 d460)
  (door r61 r11 d1161)
  (door r61 r73 d6173)
  (door r62 r58 d5862)
  (door r63 c d063)
  (door r63 r28 d2863)
  (door r64 r34 d3464)
  (door r65 r17 d1765)
  (door r66 r78 d6678)
  (door r67 r6 d667)
  (door r67 r12 d1267)
  (door r67 r46 d4667)
  (door r68 r13 d1368)
  (door r68 r25 d2568)
  (door r69 r21 d2169)
  (door r70 r8 d870)
  (door r71 r52 d5271)
  (door r72 r1 d172)
  (door r72 r16 d1672)
  (door r72 r59 d5972)
  (door r73 r24 d2473)
  (door r73 r61 d6173)
  (door r73 r85 d7385)
  (door r74 r31 d3174)
  (door r75 r11 d1175)
  (door r75 r53 d5375)
  (door r75 r81 d7581)
  (door r75 r83 d7583)
  (door r76 r28 d2876)
  (door r76 r56 d5676)
  (door r77 r8 d877)
  (door r78 r39 d3978)
  (door r78 r66 d6678)
  (door r79 r3 d379)
  (door r79 r45 d4579)
  (door r80 r8 d880)
  (door r80 r26 d2680)
  (door r81 r75 d7581)
  (door r82 r19 d1982)
  (door r82 r20 d2082)
  (door r82 r30 d3082)
  (door r82 r55 d5582)
  (door r83 r9 d983)
  (door r83 r75 d7583)
  (door r84 r2 d284)
  (door r84 r41 d4184)
  (door r85 r43 d4385)
  (door r85 r73 d7385)
  (door r86 r11 d1186)
  (door r86 r52 d5286)
  (door r87 r41 d4187)
  (door r88 r37 d3788)
  (door r88 r42 d4288)
  (door r89 r23 d2389)
  (door r89 r54 d5489)
  (door r90 r11 d1190)
  (closed d3788)
  (closed d4288)
  (closed d1642)
  (closed d1672)
  (closed d2568)
  (closed d3250)
  (closed d172)
  (closed d1175)
  (closed d253)
  (closed d147)
  (closed d4053)
  (closed d4058)
  (closed d2958)
  (closed d4184)
  (closed d7583)
  (closed d5676)
  (closed d441)
  (closed d460)
  (closed d1186)
  (closed d3352)
  (closed d444)
  (closed d1015)
  (closed d354)
  (closed d3551)
  (closed d2169)
  (closed d4385)
  (closed d1843)
  (closed d1834)
  (closed d379)
  (closed d667)
  (closed d811)
  (closed d2389)
  (closed d880)
  (closed d2754)
  (closed d063)
  (closed d1937)
  (closed d5582)
  (closed d720)
  (closed d5758)
  (closed d5862)
  (closed d6678)
  (closed d3174)
  (in o1 r28)
  (in o2 r38)
  (in o3 r49)
  (in o4 r39)
  (in o5 r19)
  (in o6 r83)
  (in o7 r37)
  (in o8 r27)
  (in o9 r7)
  (in o10 r66)
  (in o11 r71)
  (in o12 r50)
  (in o13 r78)
  (in o14 r41)
  (in o15 r42)
  (in o16 r55)
  (in o17 r22)
  (in o18 r5)
  (in o19 r56)
  (in o20 r15)
  (in o21 r48)
  (in o22 r26)
  (in o23 r46)
  (in o24 r20)
  (in o25 r19)
  (in o26 r80)
  (in o27 r42)
  (in o28 r28)
  (in o29 r86)
  (in o30 r40))
 (:goal (and
         (in o1 r35)
         (in o2 r77)
         (in o3 r59)
         (in o4 r66)
         (in o5 r74)
         (in o6 r6)
         (in o7 r17)
         (in o8 r79)
         (in o9 r2)
         (in o10 r29)
         (in o11 r63)
         (in o12 r64)
         (in o13 r8)
         (in o14 r75)
         (in o15 r24)
         (in o16 r26)
         (in o17 r42)
         (in o18 r69)
         (in o19 r2)
         (in o20 r53)
         (in o21 r68)
         (in o22 r61)
         (in o23 r10)
         (in o24 r39)
         (in o25 r76)
         (in o26 r5)
         (in o27 r89)
         (in o28 r32)
         (in o29 r87)
         (in o30 r78))))