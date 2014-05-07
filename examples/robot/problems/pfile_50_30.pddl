(define
 (problem pfile_50_30)
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
           - ROOM
           d940
           d935
           d913
           d1325
           d725
           d741
           d1625
           d816
           d818
           d1116
           d1114
           d845
           d2550
           d4250
           d4650
           d1750
           d2946
           d318
           d3046
           d1820
           d2943
           d810
           d020
           d021
           d038
           d3031
           d446
           d1649
           d019
           d619
           d2450
           d1047
           d4849
           d1744
           d1022
           d2132
           d1217
           d138
           d3440
           d1326
           d1739
           d27
           d233
           d837
           d2832
           d1528
           d1536
           d623
           d2733
           d533
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r19 d019)
  (door c r20 d020)
  (door c r21 d021)
  (door c r38 d038)
  (door r1 r38 d138)
  (door r2 r7 d27)
  (door r2 r33 d233)
  (door r3 r18 d318)
  (door r4 r46 d446)
  (door r5 r33 d533)
  (door r6 r19 d619)
  (door r6 r23 d623)
  (door r7 r2 d27)
  (door r7 r25 d725)
  (door r7 r41 d741)
  (door r8 r10 d810)
  (door r8 r16 d816)
  (door r8 r18 d818)
  (door r8 r37 d837)
  (door r8 r45 d845)
  (door r9 r13 d913)
  (door r9 r35 d935)
  (door r9 r40 d940)
  (door r10 r8 d810)
  (door r10 r22 d1022)
  (door r10 r47 d1047)
  (door r11 r14 d1114)
  (door r11 r16 d1116)
  (door r12 r17 d1217)
  (door r13 r9 d913)
  (door r13 r25 d1325)
  (door r13 r26 d1326)
  (door r14 r11 d1114)
  (door r15 r28 d1528)
  (door r15 r36 d1536)
  (door r16 r8 d816)
  (door r16 r11 d1116)
  (door r16 r25 d1625)
  (door r16 r49 d1649)
  (door r17 r12 d1217)
  (door r17 r39 d1739)
  (door r17 r44 d1744)
  (door r17 r50 d1750)
  (door r18 r3 d318)
  (door r18 r8 d818)
  (door r18 r20 d1820)
  (door r19 c d019)
  (door r19 r6 d619)
  (door r20 c d020)
  (door r20 r18 d1820)
  (door r21 c d021)
  (door r21 r32 d2132)
  (door r22 r10 d1022)
  (door r23 r6 d623)
  (door r24 r50 d2450)
  (door r25 r7 d725)
  (door r25 r13 d1325)
  (door r25 r16 d1625)
  (door r25 r50 d2550)
  (door r26 r13 d1326)
  (door r27 r33 d2733)
  (door r28 r15 d1528)
  (door r28 r32 d2832)
  (door r29 r43 d2943)
  (door r29 r46 d2946)
  (door r30 r31 d3031)
  (door r30 r46 d3046)
  (door r31 r30 d3031)
  (door r32 r21 d2132)
  (door r32 r28 d2832)
  (door r33 r2 d233)
  (door r33 r5 d533)
  (door r33 r27 d2733)
  (door r34 r40 d3440)
  (door r35 r9 d935)
  (door r36 r15 d1536)
  (door r37 r8 d837)
  (door r38 c d038)
  (door r38 r1 d138)
  (door r39 r17 d1739)
  (door r40 r9 d940)
  (door r40 r34 d3440)
  (door r41 r7 d741)
  (door r42 r50 d4250)
  (door r43 r29 d2943)
  (door r44 r17 d1744)
  (door r45 r8 d845)
  (door r46 r4 d446)
  (door r46 r29 d2946)
  (door r46 r30 d3046)
  (door r46 r50 d4650)
  (door r47 r10 d1047)
  (door r48 r49 d4849)
  (door r49 r16 d1649)
  (door r49 r48 d4849)
  (door r50 r17 d1750)
  (door r50 r24 d2450)
  (door r50 r25 d2550)
  (door r50 r42 d4250)
  (door r50 r46 d4650)
  (closed d940)
  (closed d1325)
  (closed d725)
  (closed d741)
  (closed d1625)
  (closed d816)
  (closed d818)
  (closed d1116)
  (closed d845)
  (closed d2550)
  (closed d4250)
  (closed d4650)
  (closed d318)
  (closed d3046)
  (closed d1820)
  (closed d810)
  (closed d020)
  (closed d021)
  (closed d038)
  (closed d3031)
  (closed d1649)
  (closed d019)
  (closed d1022)
  (closed d1739)
  (closed d27)
  (closed d233)
  (closed d2832)
  (closed d623)
  (closed d2733)
  (in o1 r36)
  (in o2 r42)
  (in o3 r41)
  (in o4 r28)
  (in o5 r33)
  (in o6 r3)
  (in o7 r26)
  (in o8 r1)
  (in o9 r23)
  (in o10 r48)
  (in o11 r27)
  (in o12 r47)
  (in o13 r19)
  (in o14 r35)
  (in o15 r36)
  (in o16 r17)
  (in o17 r48)
  (in o18 r2)
  (in o19 r17)
  (in o20 r36)
  (in o21 r19)
  (in o22 r26)
  (in o23 r30)
  (in o24 r29)
  (in o25 r34)
  (in o26 r4)
  (in o27 r48)
  (in o28 r3)
  (in o29 r5)
  (in o30 r28))
 (:goal (and
         (in o1 r22)
         (in o2 r10)
         (in o3 r26)
         (in o4 r35)
         (in o5 r18)
         (in o6 r29)
         (in o7 r41)
         (in o8 r50)
         (in o9 r13)
         (in o10 r7)
         (in o11 r40)
         (in o12 r24)
         (in o13 r32)
         (in o14 r5)
         (in o15 r21)
         (in o16 r13)
         (in o17 r27)
         (in o18 r25)
         (in o19 r1)
         (in o20 r39)
         (in o21 r42)
         (in o22 r48)
         (in o23 r1)
         (in o24 r4)
         (in o25 r6)
         (in o26 r8)
         (in o27 r27)
         (in o28 r3)
         (in o29 r37)
         (in o30 r1))))