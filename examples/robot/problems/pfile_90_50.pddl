(define
 (problem pfile_90_50)
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
           d1126
           d2635
           d3571
           d1671
           d816
           d1673
           d849
           d5073
           d5063
           d5082
           d5173
           d4851
           d48
           d5582
           d5255
           d2152
           d2184
           d184
           d384
           d320
           d2088
           d3388
           d2355
           d6379
           d7579
           d7987
           d3275
           d3244
           d1035
           d1029
           d2279
           d837
           d3766
           d3666
           d3640
           d940
           d986
           d958
           d3746
           d1746
           d1337
           d1345
           d3045
           d2551
           d625
           d710
           d2531
           d6275
           d6274
           d5774
           d1257
           d1272
           d5972
           d2472
           d2786
           d2734
           d221
           d247
           d319
           d3142
           d1239
           d4070
           d6870
           d068
           d6470
           d6467
           d6477
           d6485
           d2654
           d4368
           d5776
           d3741
           d4161
           d1486
           d053
           d4289
           d3889
           d589
           d6566
           d1783
           d2960
           d2428
           d5667
           d5678
           d181
           d1888
           d1523
           d6975
           d2090
           d4880
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r53 d053)
  (door c r68 d068)
  (door r1 r81 d181)
  (door r1 r84 d184)
  (door r2 r21 d221)
  (door r2 r47 d247)
  (door r3 r19 d319)
  (door r3 r20 d320)
  (door r3 r84 d384)
  (door r4 r8 d48)
  (door r5 r89 d589)
  (door r6 r25 d625)
  (door r7 r10 d710)
  (door r8 r4 d48)
  (door r8 r16 d816)
  (door r8 r37 d837)
  (door r8 r49 d849)
  (door r9 r40 d940)
  (door r9 r58 d958)
  (door r9 r86 d986)
  (door r10 r7 d710)
  (door r10 r29 d1029)
  (door r10 r35 d1035)
  (door r11 r26 d1126)
  (door r12 r39 d1239)
  (door r12 r57 d1257)
  (door r12 r72 d1272)
  (door r13 r37 d1337)
  (door r13 r45 d1345)
  (door r14 r86 d1486)
  (door r15 r23 d1523)
  (door r16 r8 d816)
  (door r16 r71 d1671)
  (door r16 r73 d1673)
  (door r17 r46 d1746)
  (door r17 r83 d1783)
  (door r18 r88 d1888)
  (door r19 r3 d319)
  (door r20 r3 d320)
  (door r20 r88 d2088)
  (door r20 r90 d2090)
  (door r21 r2 d221)
  (door r21 r52 d2152)
  (door r21 r84 d2184)
  (door r22 r79 d2279)
  (door r23 r15 d1523)
  (door r23 r55 d2355)
  (door r24 r28 d2428)
  (door r24 r72 d2472)
  (door r25 r6 d625)
  (door r25 r31 d2531)
  (door r25 r51 d2551)
  (door r26 r11 d1126)
  (door r26 r35 d2635)
  (door r26 r54 d2654)
  (door r27 r34 d2734)
  (door r27 r86 d2786)
  (door r28 r24 d2428)
  (door r29 r10 d1029)
  (door r29 r60 d2960)
  (door r30 r45 d3045)
  (door r31 r25 d2531)
  (door r31 r42 d3142)
  (door r32 r44 d3244)
  (door r32 r75 d3275)
  (door r33 r88 d3388)
  (door r34 r27 d2734)
  (door r35 r10 d1035)
  (door r35 r26 d2635)
  (door r35 r71 d3571)
  (door r36 r40 d3640)
  (door r36 r66 d3666)
  (door r37 r8 d837)
  (door r37 r13 d1337)
  (door r37 r41 d3741)
  (door r37 r46 d3746)
  (door r37 r66 d3766)
  (door r38 r89 d3889)
  (door r39 r12 d1239)
  (door r40 r9 d940)
  (door r40 r36 d3640)
  (door r40 r70 d4070)
  (door r41 r37 d3741)
  (door r41 r61 d4161)
  (door r42 r31 d3142)
  (door r42 r89 d4289)
  (door r43 r68 d4368)
  (door r44 r32 d3244)
  (door r45 r13 d1345)
  (door r45 r30 d3045)
  (door r46 r17 d1746)
  (door r46 r37 d3746)
  (door r47 r2 d247)
  (door r48 r51 d4851)
  (door r48 r80 d4880)
  (door r49 r8 d849)
  (door r50 r63 d5063)
  (door r50 r73 d5073)
  (door r50 r82 d5082)
  (door r51 r25 d2551)
  (door r51 r48 d4851)
  (door r51 r73 d5173)
  (door r52 r21 d2152)
  (door r52 r55 d5255)
  (door r53 c d053)
  (door r54 r26 d2654)
  (door r55 r23 d2355)
  (door r55 r52 d5255)
  (door r55 r82 d5582)
  (door r56 r67 d5667)
  (door r56 r78 d5678)
  (door r57 r12 d1257)
  (door r57 r74 d5774)
  (door r57 r76 d5776)
  (door r58 r9 d958)
  (door r59 r72 d5972)
  (door r60 r29 d2960)
  (door r61 r41 d4161)
  (door r62 r74 d6274)
  (door r62 r75 d6275)
  (door r63 r50 d5063)
  (door r63 r79 d6379)
  (door r64 r67 d6467)
  (door r64 r70 d6470)
  (door r64 r77 d6477)
  (door r64 r85 d6485)
  (door r65 r66 d6566)
  (door r66 r36 d3666)
  (door r66 r37 d3766)
  (door r66 r65 d6566)
  (door r67 r56 d5667)
  (door r67 r64 d6467)
  (door r68 c d068)
  (door r68 r43 d4368)
  (door r68 r70 d6870)
  (door r69 r75 d6975)
  (door r70 r40 d4070)
  (door r70 r64 d6470)
  (door r70 r68 d6870)
  (door r71 r16 d1671)
  (door r71 r35 d3571)
  (door r72 r12 d1272)
  (door r72 r24 d2472)
  (door r72 r59 d5972)
  (door r73 r16 d1673)
  (door r73 r50 d5073)
  (door r73 r51 d5173)
  (door r74 r57 d5774)
  (door r74 r62 d6274)
  (door r75 r32 d3275)
  (door r75 r62 d6275)
  (door r75 r69 d6975)
  (door r75 r79 d7579)
  (door r76 r57 d5776)
  (door r77 r64 d6477)
  (door r78 r56 d5678)
  (door r79 r22 d2279)
  (door r79 r63 d6379)
  (door r79 r75 d7579)
  (door r79 r87 d7987)
  (door r80 r48 d4880)
  (door r81 r1 d181)
  (door r82 r50 d5082)
  (door r82 r55 d5582)
  (door r83 r17 d1783)
  (door r84 r1 d184)
  (door r84 r3 d384)
  (door r84 r21 d2184)
  (door r85 r64 d6485)
  (door r86 r9 d986)
  (door r86 r14 d1486)
  (door r86 r27 d2786)
  (door r87 r79 d7987)
  (door r88 r18 d1888)
  (door r88 r20 d2088)
  (door r88 r33 d3388)
  (door r89 r5 d589)
  (door r89 r38 d3889)
  (door r89 r42 d4289)
  (door r90 r20 d2090)
  (closed d1126)
  (closed d2635)
  (closed d3571)
  (closed d1673)
  (closed d849)
  (closed d5073)
  (closed d5082)
  (closed d5173)
  (closed d5582)
  (closed d5255)
  (closed d2088)
  (closed d2355)
  (closed d3275)
  (closed d1035)
  (closed d1029)
  (closed d837)
  (closed d3666)
  (closed d986)
  (closed d958)
  (closed d3746)
  (closed d3045)
  (closed d710)
  (closed d6275)
  (closed d6274)
  (closed d1257)
  (closed d1272)
  (closed d5972)
  (closed d2472)
  (closed d2786)
  (closed d2734)
  (closed d319)
  (closed d3142)
  (closed d1239)
  (closed d4070)
  (closed d6870)
  (closed d6470)
  (closed d2654)
  (closed d4368)
  (closed d5776)
  (closed d3741)
  (closed d1486)
  (closed d4289)
  (closed d589)
  (closed d6566)
  (closed d1783)
  (closed d2960)
  (closed d5667)
  (closed d181)
  (closed d1523)
  (closed d6975)
  (closed d2090)
  (in o1 r41)
  (in o2 r67)
  (in o3 r32)
  (in o4 r52)
  (in o5 r55)
  (in o6 r79)
  (in o7 r28)
  (in o8 r3)
  (in o9 r90)
  (in o10 r76)
  (in o11 r29)
  (in o12 r29)
  (in o13 r28)
  (in o14 r51)
  (in o15 r33)
  (in o16 r25)
  (in o17 r7)
  (in o18 r42)
  (in o19 r36)
  (in o20 r27)
  (in o21 r89)
  (in o22 r34)
  (in o23 r38)
  (in o24 r41)
  (in o25 r4)
  (in o26 r70)
  (in o27 r38)
  (in o28 r70)
  (in o29 r27)
  (in o30 r23)
  (in o31 r89)
  (in o32 r50)
  (in o33 r24)
  (in o34 r68)
  (in o35 r3)
  (in o36 r69)
  (in o37 r47)
  (in o38 r28)
  (in o39 r90)
  (in o40 r40)
  (in o41 r45)
  (in o42 r66)
  (in o43 r90)
  (in o44 r88)
  (in o45 r54)
  (in o46 r89)
  (in o47 r17)
  (in o48 r74)
  (in o49 r29)
  (in o50 r7))
 (:goal (and
         (in o1 r65)
         (in o2 r75)
         (in o3 r79)
         (in o4 r73)
         (in o5 r20)
         (in o6 r19)
         (in o7 r37)
         (in o8 r28)
         (in o9 r25)
         (in o10 r58)
         (in o11 r33)
         (in o12 r67)
         (in o13 r82)
         (in o14 r37)
         (in o15 r55)
         (in o16 r55)
         (in o17 r16)
         (in o18 r87)
         (in o19 r87)
         (in o20 r35)
         (in o21 r42)
         (in o22 r36)
         (in o23 r86)
         (in o24 r79)
         (in o25 r12)
         (in o26 r71)
         (in o27 r80)
         (in o28 r1)
         (in o29 r19)
         (in o30 r89)
         (in o31 r14)
         (in o32 r19)
         (in o33 r17)
         (in o34 r56)
         (in o35 r53)
         (in o36 r32)
         (in o37 r39)
         (in o38 r80)
         (in o39 r51)
         (in o40 r83)
         (in o41 r59)
         (in o42 r59)
         (in o43 r66)
         (in o44 r9)
         (in o45 r57)
         (in o46 r80)
         (in o47 r54)
         (in o48 r68)
         (in o49 r57)
         (in o50 r40))))