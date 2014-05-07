(define
 (problem pfile_70_10)
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
           - ROOM
           d23
           d269
           d6669
           d1469
           d1419
           d1961
           d014
           d059
           d459
           d448
           d4248
           d3442
           d4862
           d2142
           d4162
           d4152
           d1922
           d2223
           d723
           d67
           d625
           d2536
           d628
           d3640
           d1540
           d1215
           d2860
           d3639
           d3951
           d551
           d5051
           d5065
           d5365
           d2765
           d2770
           d2950
           d1840
           d1752
           d2256
           d827
           d4351
           d2656
           d926
           d920
           d964
           d2645
           d5566
           d5568
           d1168
           d1130
           d1630
           d3031
           d1132
           d4057
           d1057
           d2346
           d146
           d1847
           d4754
           d3158
           d3855
           d3560
           d4649
           d4344
           d2451
           d1324
           d2433
           d6063
           d3763
           d467
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r14 d014)
  (door c r59 d059)
  (door r1 r46 d146)
  (door r2 r3 d23)
  (door r2 r69 d269)
  (door r3 r2 d23)
  (door r4 r48 d448)
  (door r4 r59 d459)
  (door r4 r67 d467)
  (door r5 r51 d551)
  (door r6 r7 d67)
  (door r6 r25 d625)
  (door r6 r28 d628)
  (door r7 r6 d67)
  (door r7 r23 d723)
  (door r8 r27 d827)
  (door r9 r20 d920)
  (door r9 r26 d926)
  (door r9 r64 d964)
  (door r10 r57 d1057)
  (door r11 r30 d1130)
  (door r11 r32 d1132)
  (door r11 r68 d1168)
  (door r12 r15 d1215)
  (door r13 r24 d1324)
  (door r14 c d014)
  (door r14 r19 d1419)
  (door r14 r69 d1469)
  (door r15 r12 d1215)
  (door r15 r40 d1540)
  (door r16 r30 d1630)
  (door r17 r52 d1752)
  (door r18 r40 d1840)
  (door r18 r47 d1847)
  (door r19 r14 d1419)
  (door r19 r22 d1922)
  (door r19 r61 d1961)
  (door r20 r9 d920)
  (door r21 r42 d2142)
  (door r22 r19 d1922)
  (door r22 r23 d2223)
  (door r22 r56 d2256)
  (door r23 r7 d723)
  (door r23 r22 d2223)
  (door r23 r46 d2346)
  (door r24 r13 d1324)
  (door r24 r33 d2433)
  (door r24 r51 d2451)
  (door r25 r6 d625)
  (door r25 r36 d2536)
  (door r26 r9 d926)
  (door r26 r45 d2645)
  (door r26 r56 d2656)
  (door r27 r8 d827)
  (door r27 r65 d2765)
  (door r27 r70 d2770)
  (door r28 r6 d628)
  (door r28 r60 d2860)
  (door r29 r50 d2950)
  (door r30 r11 d1130)
  (door r30 r16 d1630)
  (door r30 r31 d3031)
  (door r31 r30 d3031)
  (door r31 r58 d3158)
  (door r32 r11 d1132)
  (door r33 r24 d2433)
  (door r34 r42 d3442)
  (door r35 r60 d3560)
  (door r36 r25 d2536)
  (door r36 r39 d3639)
  (door r36 r40 d3640)
  (door r37 r63 d3763)
  (door r38 r55 d3855)
  (door r39 r36 d3639)
  (door r39 r51 d3951)
  (door r40 r15 d1540)
  (door r40 r18 d1840)
  (door r40 r36 d3640)
  (door r40 r57 d4057)
  (door r41 r52 d4152)
  (door r41 r62 d4162)
  (door r42 r21 d2142)
  (door r42 r34 d3442)
  (door r42 r48 d4248)
  (door r43 r44 d4344)
  (door r43 r51 d4351)
  (door r44 r43 d4344)
  (door r45 r26 d2645)
  (door r46 r1 d146)
  (door r46 r23 d2346)
  (door r46 r49 d4649)
  (door r47 r18 d1847)
  (door r47 r54 d4754)
  (door r48 r4 d448)
  (door r48 r42 d4248)
  (door r48 r62 d4862)
  (door r49 r46 d4649)
  (door r50 r29 d2950)
  (door r50 r51 d5051)
  (door r50 r65 d5065)
  (door r51 r5 d551)
  (door r51 r24 d2451)
  (door r51 r39 d3951)
  (door r51 r43 d4351)
  (door r51 r50 d5051)
  (door r52 r17 d1752)
  (door r52 r41 d4152)
  (door r53 r65 d5365)
  (door r54 r47 d4754)
  (door r55 r38 d3855)
  (door r55 r66 d5566)
  (door r55 r68 d5568)
  (door r56 r22 d2256)
  (door r56 r26 d2656)
  (door r57 r10 d1057)
  (door r57 r40 d4057)
  (door r58 r31 d3158)
  (door r59 c d059)
  (door r59 r4 d459)
  (door r60 r28 d2860)
  (door r60 r35 d3560)
  (door r60 r63 d6063)
  (door r61 r19 d1961)
  (door r62 r41 d4162)
  (door r62 r48 d4862)
  (door r63 r37 d3763)
  (door r63 r60 d6063)
  (door r64 r9 d964)
  (door r65 r27 d2765)
  (door r65 r50 d5065)
  (door r65 r53 d5365)
  (door r66 r55 d5566)
  (door r66 r69 d6669)
  (door r67 r4 d467)
  (door r68 r11 d1168)
  (door r68 r55 d5568)
  (door r69 r2 d269)
  (door r69 r14 d1469)
  (door r69 r66 d6669)
  (door r70 r27 d2770)
  (closed d23)
  (closed d1469)
  (closed d1419)
  (closed d1961)
  (closed d014)
  (closed d448)
  (closed d4862)
  (closed d2142)
  (closed d4162)
  (closed d4152)
  (closed d2223)
  (closed d723)
  (closed d625)
  (closed d2536)
  (closed d628)
  (closed d3640)
  (closed d1540)
  (closed d2860)
  (closed d551)
  (closed d5365)
  (closed d2765)
  (closed d2770)
  (closed d1752)
  (closed d2256)
  (closed d5566)
  (closed d5568)
  (closed d1130)
  (closed d1630)
  (closed d3031)
  (closed d4057)
  (closed d1057)
  (closed d146)
  (closed d1847)
  (closed d4754)
  (closed d4649)
  (closed d2451)
  (closed d1324)
  (closed d6063)
  (closed d3763)
  (closed d467)
  (in o1 r38)
  (in o2 r50)
  (in o3 r27)
  (in o4 r54)
  (in o5 r57)
  (in o6 r30)
  (in o7 r68)
  (in o8 r46)
  (in o9 r59)
  (in o10 r13))
 (:goal (and
         (in o1 r2)
         (in o2 r70)
         (in o3 r38)
         (in o4 r51)
         (in o5 r17)
         (in o6 r12)
         (in o7 r34)
         (in o8 r37)
         (in o9 r33)
         (in o10 r5))))