(define
 (problem pfile_50_50)
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
           - ROOM
           d046
           d041
           d050
           d030
           d1530
           d1315
           d3048
           d1538
           d3538
           d2338
           d1438
           d435
           d420
           d835
           d822
           d1638
           d1636
           d1720
           d641
           d624
           d224
           d244
           d1244
           d1219
           d1926
           d2649
           d4049
           d740
           d4045
           d734
           d2540
           d3134
           d2134
           d549
           d2233
           d2749
           d3950
           d443
           d2948
           d3244
           d3247
           d1016
           d1028
           d1144
           d1843
           d122
           d313
           d2842
           d949
           d837
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r30 d030)
  (door c r41 d041)
  (door c r46 d046)
  (door c r50 d050)
  (door r1 r22 d122)
  (door r2 r24 d224)
  (door r2 r44 d244)
  (door r3 r13 d313)
  (door r4 r20 d420)
  (door r4 r35 d435)
  (door r4 r43 d443)
  (door r5 r49 d549)
  (door r6 r24 d624)
  (door r6 r41 d641)
  (door r7 r34 d734)
  (door r7 r40 d740)
  (door r8 r22 d822)
  (door r8 r35 d835)
  (door r8 r37 d837)
  (door r9 r49 d949)
  (door r10 r16 d1016)
  (door r10 r28 d1028)
  (door r11 r44 d1144)
  (door r12 r19 d1219)
  (door r12 r44 d1244)
  (door r13 r3 d313)
  (door r13 r15 d1315)
  (door r14 r38 d1438)
  (door r15 r13 d1315)
  (door r15 r30 d1530)
  (door r15 r38 d1538)
  (door r16 r10 d1016)
  (door r16 r36 d1636)
  (door r16 r38 d1638)
  (door r17 r20 d1720)
  (door r18 r43 d1843)
  (door r19 r12 d1219)
  (door r19 r26 d1926)
  (door r20 r4 d420)
  (door r20 r17 d1720)
  (door r21 r34 d2134)
  (door r22 r1 d122)
  (door r22 r8 d822)
  (door r22 r33 d2233)
  (door r23 r38 d2338)
  (door r24 r2 d224)
  (door r24 r6 d624)
  (door r25 r40 d2540)
  (door r26 r19 d1926)
  (door r26 r49 d2649)
  (door r27 r49 d2749)
  (door r28 r10 d1028)
  (door r28 r42 d2842)
  (door r29 r48 d2948)
  (door r30 c d030)
  (door r30 r15 d1530)
  (door r30 r48 d3048)
  (door r31 r34 d3134)
  (door r32 r44 d3244)
  (door r32 r47 d3247)
  (door r33 r22 d2233)
  (door r34 r7 d734)
  (door r34 r21 d2134)
  (door r34 r31 d3134)
  (door r35 r4 d435)
  (door r35 r8 d835)
  (door r35 r38 d3538)
  (door r36 r16 d1636)
  (door r37 r8 d837)
  (door r38 r14 d1438)
  (door r38 r15 d1538)
  (door r38 r16 d1638)
  (door r38 r23 d2338)
  (door r38 r35 d3538)
  (door r39 r50 d3950)
  (door r40 r7 d740)
  (door r40 r25 d2540)
  (door r40 r45 d4045)
  (door r40 r49 d4049)
  (door r41 c d041)
  (door r41 r6 d641)
  (door r42 r28 d2842)
  (door r43 r4 d443)
  (door r43 r18 d1843)
  (door r44 r2 d244)
  (door r44 r11 d1144)
  (door r44 r12 d1244)
  (door r44 r32 d3244)
  (door r45 r40 d4045)
  (door r46 c d046)
  (door r47 r32 d3247)
  (door r48 r29 d2948)
  (door r48 r30 d3048)
  (door r49 r5 d549)
  (door r49 r9 d949)
  (door r49 r26 d2649)
  (door r49 r27 d2749)
  (door r49 r40 d4049)
  (door r50 c d050)
  (door r50 r39 d3950)
  (closed d046)
  (closed d050)
  (closed d1315)
  (closed d3048)
  (closed d3538)
  (closed d2338)
  (closed d1438)
  (closed d435)
  (closed d420)
  (closed d835)
  (closed d1638)
  (closed d1720)
  (closed d641)
  (closed d624)
  (closed d1926)
  (closed d2649)
  (closed d4049)
  (closed d2233)
  (closed d2749)
  (closed d443)
  (closed d3247)
  (closed d1016)
  (closed d1028)
  (closed d122)
  (closed d2842)
  (in o1 r1)
  (in o2 r49)
  (in o3 r23)
  (in o4 r50)
  (in o5 r41)
  (in o6 r20)
  (in o7 r1)
  (in o8 r22)
  (in o9 r20)
  (in o10 r1)
  (in o11 r31)
  (in o12 r30)
  (in o13 r11)
  (in o14 r30)
  (in o15 r27)
  (in o16 r49)
  (in o17 r22)
  (in o18 r23)
  (in o19 r48)
  (in o20 r21)
  (in o21 r47)
  (in o22 r43)
  (in o23 r47)
  (in o24 r9)
  (in o25 r24)
  (in o26 r20)
  (in o27 r22)
  (in o28 r29)
  (in o29 r3)
  (in o30 r47)
  (in o31 r14)
  (in o32 r48)
  (in o33 r42)
  (in o34 r30)
  (in o35 r31)
  (in o36 r34)
  (in o37 r12)
  (in o38 r15)
  (in o39 r33)
  (in o40 r25)
  (in o41 r25)
  (in o42 r8)
  (in o43 r2)
  (in o44 r1)
  (in o45 r34)
  (in o46 r41)
  (in o47 r19)
  (in o48 r34)
  (in o49 r48)
  (in o50 r12))
 (:goal (and
         (in o1 r2)
         (in o2 r45)
         (in o3 r1)
         (in o4 r4)
         (in o5 r12)
         (in o6 r38)
         (in o7 r12)
         (in o8 r3)
         (in o9 r39)
         (in o10 r26)
         (in o11 r13)
         (in o12 r11)
         (in o13 r37)
         (in o14 r2)
         (in o15 r14)
         (in o16 r9)
         (in o17 r4)
         (in o18 r6)
         (in o19 r28)
         (in o20 r17)
         (in o21 r18)
         (in o22 r35)
         (in o23 r13)
         (in o24 r36)
         (in o25 r40)
         (in o26 r49)
         (in o27 r41)
         (in o28 r48)
         (in o29 r50)
         (in o30 r15)
         (in o31 r13)
         (in o32 r24)
         (in o33 r3)
         (in o34 r41)
         (in o35 r49)
         (in o36 r39)
         (in o37 r27)
         (in o38 r36)
         (in o39 r15)
         (in o40 r37)
         (in o41 r8)
         (in o42 r40)
         (in o43 r22)
         (in o44 r39)
         (in o45 r1)
         (in o46 r8)
         (in o47 r18)
         (in o48 r7)
         (in o49 r24)
         (in o50 r38))))