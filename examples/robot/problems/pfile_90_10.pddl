(define
 (problem pfile_90_10)
 (:domain robot)
 (:objects o1 o2 o3 o4 o5 o6 o7 o8 o9 o10 - PACKAGE
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
           d7590
           d7690
           d3676
           d676
           d680
           d4380
           d4379
           d655
           d6076
           d455
           d4366
           d4166
           d4185
           d7385
           d2285
           d1185
           d5166
           d3551
           d3951
           d339
           d324
           d318
           d2431
           d2031
           d2029
           d2829
           d2840
           d3170
           d4270
           d1342
           d5970
           d3189
           d1089
           d1045
           d1061
           d3182
           d1056
           d3978
           d1978
           d1949
           d2549
           d7888
           d088
           d4957
           d731
           d5972
           d6872
           d1468
           d5981
           d2731
           d3757
           d3584
           d5569
           d1678
           d1653
           d816
           d815
           d3289
           d4352
           d4652
           d3052
           d4658
           d2169
           d4586
           d120
           d138
           d3874
           d4648
           d4883
           d1259
           d573
           d5472
           d4454
           d2264
           d7176
           d6271
           d257
           d3765
           d1755
           d4785
           d3443
           d959
           d967
           d987
           d5977
           d4150
           d2371
           d1526
           d3360
           d1663
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r88 d088)
  (door r1 r20 d120)
  (door r1 r38 d138)
  (door r2 r57 d257)
  (door r3 r18 d318)
  (door r3 r24 d324)
  (door r3 r39 d339)
  (door r4 r55 d455)
  (door r5 r73 d573)
  (door r6 r55 d655)
  (door r6 r76 d676)
  (door r6 r80 d680)
  (door r7 r31 d731)
  (door r8 r15 d815)
  (door r8 r16 d816)
  (door r9 r59 d959)
  (door r9 r67 d967)
  (door r9 r87 d987)
  (door r10 r45 d1045)
  (door r10 r56 d1056)
  (door r10 r61 d1061)
  (door r10 r89 d1089)
  (door r11 r85 d1185)
  (door r12 r59 d1259)
  (door r13 r42 d1342)
  (door r14 r68 d1468)
  (door r15 r8 d815)
  (door r15 r26 d1526)
  (door r16 r8 d816)
  (door r16 r53 d1653)
  (door r16 r63 d1663)
  (door r16 r78 d1678)
  (door r17 r55 d1755)
  (door r18 r3 d318)
  (door r19 r49 d1949)
  (door r19 r78 d1978)
  (door r20 r1 d120)
  (door r20 r29 d2029)
  (door r20 r31 d2031)
  (door r21 r69 d2169)
  (door r22 r64 d2264)
  (door r22 r85 d2285)
  (door r23 r71 d2371)
  (door r24 r3 d324)
  (door r24 r31 d2431)
  (door r25 r49 d2549)
  (door r26 r15 d1526)
  (door r27 r31 d2731)
  (door r28 r29 d2829)
  (door r28 r40 d2840)
  (door r29 r20 d2029)
  (door r29 r28 d2829)
  (door r30 r52 d3052)
  (door r31 r7 d731)
  (door r31 r20 d2031)
  (door r31 r24 d2431)
  (door r31 r27 d2731)
  (door r31 r70 d3170)
  (door r31 r82 d3182)
  (door r31 r89 d3189)
  (door r32 r89 d3289)
  (door r33 r60 d3360)
  (door r34 r43 d3443)
  (door r35 r51 d3551)
  (door r35 r84 d3584)
  (door r36 r76 d3676)
  (door r37 r57 d3757)
  (door r37 r65 d3765)
  (door r38 r1 d138)
  (door r38 r74 d3874)
  (door r39 r3 d339)
  (door r39 r51 d3951)
  (door r39 r78 d3978)
  (door r40 r28 d2840)
  (door r41 r50 d4150)
  (door r41 r66 d4166)
  (door r41 r85 d4185)
  (door r42 r13 d1342)
  (door r42 r70 d4270)
  (door r43 r34 d3443)
  (door r43 r52 d4352)
  (door r43 r66 d4366)
  (door r43 r79 d4379)
  (door r43 r80 d4380)
  (door r44 r54 d4454)
  (door r45 r10 d1045)
  (door r45 r86 d4586)
  (door r46 r48 d4648)
  (door r46 r52 d4652)
  (door r46 r58 d4658)
  (door r47 r85 d4785)
  (door r48 r46 d4648)
  (door r48 r83 d4883)
  (door r49 r19 d1949)
  (door r49 r25 d2549)
  (door r49 r57 d4957)
  (door r50 r41 d4150)
  (door r51 r35 d3551)
  (door r51 r39 d3951)
  (door r51 r66 d5166)
  (door r52 r30 d3052)
  (door r52 r43 d4352)
  (door r52 r46 d4652)
  (door r53 r16 d1653)
  (door r54 r44 d4454)
  (door r54 r72 d5472)
  (door r55 r4 d455)
  (door r55 r6 d655)
  (door r55 r17 d1755)
  (door r55 r69 d5569)
  (door r56 r10 d1056)
  (door r57 r2 d257)
  (door r57 r37 d3757)
  (door r57 r49 d4957)
  (door r58 r46 d4658)
  (door r59 r9 d959)
  (door r59 r12 d1259)
  (door r59 r70 d5970)
  (door r59 r72 d5972)
  (door r59 r77 d5977)
  (door r59 r81 d5981)
  (door r60 r33 d3360)
  (door r60 r76 d6076)
  (door r61 r10 d1061)
  (door r62 r71 d6271)
  (door r63 r16 d1663)
  (door r64 r22 d2264)
  (door r65 r37 d3765)
  (door r66 r41 d4166)
  (door r66 r43 d4366)
  (door r66 r51 d5166)
  (door r67 r9 d967)
  (door r68 r14 d1468)
  (door r68 r72 d6872)
  (door r69 r21 d2169)
  (door r69 r55 d5569)
  (door r70 r31 d3170)
  (door r70 r42 d4270)
  (door r70 r59 d5970)
  (door r71 r23 d2371)
  (door r71 r62 d6271)
  (door r71 r76 d7176)
  (door r72 r54 d5472)
  (door r72 r59 d5972)
  (door r72 r68 d6872)
  (door r73 r5 d573)
  (door r73 r85 d7385)
  (door r74 r38 d3874)
  (door r75 r90 d7590)
  (door r76 r6 d676)
  (door r76 r36 d3676)
  (door r76 r60 d6076)
  (door r76 r71 d7176)
  (door r76 r90 d7690)
  (door r77 r59 d5977)
  (door r78 r16 d1678)
  (door r78 r19 d1978)
  (door r78 r39 d3978)
  (door r78 r88 d7888)
  (door r79 r43 d4379)
  (door r80 r6 d680)
  (door r80 r43 d4380)
  (door r81 r59 d5981)
  (door r82 r31 d3182)
  (door r83 r48 d4883)
  (door r84 r35 d3584)
  (door r85 r11 d1185)
  (door r85 r22 d2285)
  (door r85 r41 d4185)
  (door r85 r47 d4785)
  (door r85 r73 d7385)
  (door r86 r45 d4586)
  (door r87 r9 d987)
  (door r88 c d088)
  (door r88 r78 d7888)
  (door r89 r10 d1089)
  (door r89 r31 d3189)
  (door r89 r32 d3289)
  (door r90 r75 d7590)
  (door r90 r76 d7690)
  (closed d7690)
  (closed d3676)
  (closed d680)
  (closed d455)
  (closed d4366)
  (closed d4185)
  (closed d318)
  (closed d2029)
  (closed d2840)
  (closed d5970)
  (closed d1089)
  (closed d1061)
  (closed d1978)
  (closed d1949)
  (closed d2549)
  (closed d4957)
  (closed d6872)
  (closed d1468)
  (closed d2731)
  (closed d3757)
  (closed d3584)
  (closed d1653)
  (closed d816)
  (closed d4352)
  (closed d4652)
  (closed d3052)
  (closed d2169)
  (closed d138)
  (closed d3874)
  (closed d4648)
  (closed d4883)
  (closed d573)
  (closed d4454)
  (closed d2264)
  (closed d6271)
  (closed d1755)
  (closed d4785)
  (closed d3443)
  (closed d5977)
  (closed d1526)
  (closed d3360)
  (closed d1663)
  (in o1 r27)
  (in o2 r32)
  (in o3 r47)
  (in o4 r49)
  (in o5 r36)
  (in o6 r17)
  (in o7 r36)
  (in o8 r19)
  (in o9 r40)
  (in o10 r75))
 (:goal (and
         (in o1 r7)
         (in o2 r72)
         (in o3 r85)
         (in o4 r18)
         (in o5 r37)
         (in o6 r54)
         (in o7 r77)
         (in o8 r27)
         (in o9 r73)
         (in o10 r21))))