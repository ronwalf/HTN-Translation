(define
 (problem pfile_50_10)
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
           - ROOM
           d1038
           d138
           d134
           d3440
           d340
           d311
           d1122
           d2239
           d539
           d532
           d2032
           d2021
           d621
           d821
           d3249
           d646
           d2446
           d2436
           d1446
           d438
           d423
           d2328
           d023
           d045
           d4145
           d4147
           d1847
           d1647
           d1328
           d1342
           d24
           d4550
           d2742
           d37
           d230
           d4349
           d1426
           d1344
           d1048
           d1948
           d1248
           d535
           d1743
           d1633
           d2933
           d1538
           d29
           d2533
           d3748
           d3149
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r23 d023)
  (door c r45 d045)
  (door r1 r34 d134)
  (door r1 r38 d138)
  (door r2 r4 d24)
  (door r2 r9 d29)
  (door r2 r30 d230)
  (door r3 r7 d37)
  (door r3 r11 d311)
  (door r3 r40 d340)
  (door r4 r2 d24)
  (door r4 r23 d423)
  (door r4 r38 d438)
  (door r5 r32 d532)
  (door r5 r35 d535)
  (door r5 r39 d539)
  (door r6 r21 d621)
  (door r6 r46 d646)
  (door r7 r3 d37)
  (door r8 r21 d821)
  (door r9 r2 d29)
  (door r10 r38 d1038)
  (door r10 r48 d1048)
  (door r11 r3 d311)
  (door r11 r22 d1122)
  (door r12 r48 d1248)
  (door r13 r28 d1328)
  (door r13 r42 d1342)
  (door r13 r44 d1344)
  (door r14 r26 d1426)
  (door r14 r46 d1446)
  (door r15 r38 d1538)
  (door r16 r33 d1633)
  (door r16 r47 d1647)
  (door r17 r43 d1743)
  (door r18 r47 d1847)
  (door r19 r48 d1948)
  (door r20 r21 d2021)
  (door r20 r32 d2032)
  (door r21 r6 d621)
  (door r21 r8 d821)
  (door r21 r20 d2021)
  (door r22 r11 d1122)
  (door r22 r39 d2239)
  (door r23 c d023)
  (door r23 r4 d423)
  (door r23 r28 d2328)
  (door r24 r36 d2436)
  (door r24 r46 d2446)
  (door r25 r33 d2533)
  (door r26 r14 d1426)
  (door r27 r42 d2742)
  (door r28 r13 d1328)
  (door r28 r23 d2328)
  (door r29 r33 d2933)
  (door r30 r2 d230)
  (door r31 r49 d3149)
  (door r32 r5 d532)
  (door r32 r20 d2032)
  (door r32 r49 d3249)
  (door r33 r16 d1633)
  (door r33 r25 d2533)
  (door r33 r29 d2933)
  (door r34 r1 d134)
  (door r34 r40 d3440)
  (door r35 r5 d535)
  (door r36 r24 d2436)
  (door r37 r48 d3748)
  (door r38 r1 d138)
  (door r38 r4 d438)
  (door r38 r10 d1038)
  (door r38 r15 d1538)
  (door r39 r5 d539)
  (door r39 r22 d2239)
  (door r40 r3 d340)
  (door r40 r34 d3440)
  (door r41 r45 d4145)
  (door r41 r47 d4147)
  (door r42 r13 d1342)
  (door r42 r27 d2742)
  (door r43 r17 d1743)
  (door r43 r49 d4349)
  (door r44 r13 d1344)
  (door r45 c d045)
  (door r45 r41 d4145)
  (door r45 r50 d4550)
  (door r46 r6 d646)
  (door r46 r14 d1446)
  (door r46 r24 d2446)
  (door r47 r16 d1647)
  (door r47 r18 d1847)
  (door r47 r41 d4147)
  (door r48 r10 d1048)
  (door r48 r12 d1248)
  (door r48 r19 d1948)
  (door r48 r37 d3748)
  (door r49 r31 d3149)
  (door r49 r32 d3249)
  (door r49 r43 d4349)
  (door r50 r45 d4550)
  (closed d138)
  (closed d134)
  (closed d3440)
  (closed d340)
  (closed d2239)
  (closed d539)
  (closed d532)
  (closed d621)
  (closed d1446)
  (closed d438)
  (closed d2328)
  (closed d023)
  (closed d4145)
  (closed d4147)
  (closed d1847)
  (closed d1328)
  (closed d1342)
  (closed d37)
  (closed d1048)
  (closed d1948)
  (closed d535)
  (closed d2933)
  (closed d1538)
  (closed d29)
  (closed d2533)
  (closed d3149)
  (in o1 r36)
  (in o2 r47)
  (in o3 r9)
  (in o4 r19)
  (in o5 r18)
  (in o6 r16)
  (in o7 r38)
  (in o8 r44)
  (in o9 r11)
  (in o10 r22))
 (:goal (and
         (in o1 r6)
         (in o2 r18)
         (in o3 r13)
         (in o4 r19)
         (in o5 r33)
         (in o6 r1)
         (in o7 r50)
         (in o8 r37)
         (in o9 r16)
         (in o10 r32))))