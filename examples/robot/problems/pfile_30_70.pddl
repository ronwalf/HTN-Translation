(define
 (problem pfile_30_70)
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
           - ROOM
           d129
           d110
           d1026
           d114
           d111
           d711
           d57
           d1115
           d1523
           d527
           d718
           d027
           d017
           d1725
           d317
           d23
           d1430
           d1226
           d1222
           d422
           d413
           d1328
           d2122
           d1925
           d67
           d1316
           d1020
           d2324
           d819
           d916
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r17 d017)
  (door c r27 d027)
  (door r1 r10 d110)
  (door r1 r11 d111)
  (door r1 r14 d114)
  (door r1 r29 d129)
  (door r2 r3 d23)
  (door r3 r2 d23)
  (door r3 r17 d317)
  (door r4 r13 d413)
  (door r4 r22 d422)
  (door r5 r7 d57)
  (door r5 r27 d527)
  (door r6 r7 d67)
  (door r7 r5 d57)
  (door r7 r6 d67)
  (door r7 r11 d711)
  (door r7 r18 d718)
  (door r8 r19 d819)
  (door r9 r16 d916)
  (door r10 r1 d110)
  (door r10 r20 d1020)
  (door r10 r26 d1026)
  (door r11 r1 d111)
  (door r11 r7 d711)
  (door r11 r15 d1115)
  (door r12 r22 d1222)
  (door r12 r26 d1226)
  (door r13 r4 d413)
  (door r13 r16 d1316)
  (door r13 r28 d1328)
  (door r14 r1 d114)
  (door r14 r30 d1430)
  (door r15 r11 d1115)
  (door r15 r23 d1523)
  (door r16 r9 d916)
  (door r16 r13 d1316)
  (door r17 c d017)
  (door r17 r3 d317)
  (door r17 r25 d1725)
  (door r18 r7 d718)
  (door r19 r8 d819)
  (door r19 r25 d1925)
  (door r20 r10 d1020)
  (door r21 r22 d2122)
  (door r22 r4 d422)
  (door r22 r12 d1222)
  (door r22 r21 d2122)
  (door r23 r15 d1523)
  (door r23 r24 d2324)
  (door r24 r23 d2324)
  (door r25 r17 d1725)
  (door r25 r19 d1925)
  (door r26 r10 d1026)
  (door r26 r12 d1226)
  (door r27 c d027)
  (door r27 r5 d527)
  (door r28 r13 d1328)
  (door r29 r1 d129)
  (door r30 r14 d1430)
  (closed d129)
  (closed d111)
  (closed d711)
  (closed d57)
  (closed d1523)
  (closed d718)
  (closed d027)
  (closed d1430)
  (closed d1226)
  (closed d422)
  (closed d1328)
  (closed d2122)
  (closed d67)
  (closed d1316)
  (in o1 r22)
  (in o2 r27)
  (in o3 r6)
  (in o4 r13)
  (in o5 r2)
  (in o6 r4)
  (in o7 r24)
  (in o8 r26)
  (in o9 r22)
  (in o10 r9)
  (in o11 r30)
  (in o12 r9)
  (in o13 r8)
  (in o14 r17)
  (in o15 r11)
  (in o16 r4)
  (in o17 r28)
  (in o18 r2)
  (in o19 r6)
  (in o20 r21)
  (in o21 r4)
  (in o22 r5)
  (in o23 r30)
  (in o24 r19)
  (in o25 r2)
  (in o26 r27)
  (in o27 r3)
  (in o28 r14)
  (in o29 r17)
  (in o30 r8)
  (in o31 r30)
  (in o32 r15)
  (in o33 r12)
  (in o34 r3)
  (in o35 r3)
  (in o36 r21)
  (in o37 r2)
  (in o38 r7)
  (in o39 r19)
  (in o40 r18)
  (in o41 r12)
  (in o42 r27)
  (in o43 r14)
  (in o44 r1)
  (in o45 r23)
  (in o46 r20)
  (in o47 r29)
  (in o48 r14)
  (in o49 r13)
  (in o50 r26)
  (in o51 r26)
  (in o52 r12)
  (in o53 r6)
  (in o54 r3)
  (in o55 r25)
  (in o56 r6)
  (in o57 r10)
  (in o58 r29)
  (in o59 r2)
  (in o60 r27)
  (in o61 r1)
  (in o62 r6)
  (in o63 r17)
  (in o64 r28)
  (in o65 r14)
  (in o66 r11)
  (in o67 r22)
  (in o68 r27)
  (in o69 r14)
  (in o70 r23))
 (:goal (and
         (in o1 r26)
         (in o2 r28)
         (in o3 r9)
         (in o4 r30)
         (in o5 r14)
         (in o6 r24)
         (in o7 r24)
         (in o8 r21)
         (in o9 r8)
         (in o10 r17)
         (in o11 r21)
         (in o12 r13)
         (in o13 r24)
         (in o14 r23)
         (in o15 r1)
         (in o16 r16)
         (in o17 r25)
         (in o18 r27)
         (in o19 r20)
         (in o20 r30)
         (in o21 r3)
         (in o22 r28)
         (in o23 r9)
         (in o24 r1)
         (in o25 r12)
         (in o26 r20)
         (in o27 r21)
         (in o28 r8)
         (in o29 r24)
         (in o30 r4)
         (in o31 r2)
         (in o32 r26)
         (in o33 r14)
         (in o34 r27)
         (in o35 r7)
         (in o36 r21)
         (in o37 r22)
         (in o38 r13)
         (in o39 r1)
         (in o40 r4)
         (in o41 r9)
         (in o42 r29)
         (in o43 r24)
         (in o44 r1)
         (in o45 r11)
         (in o46 r29)
         (in o47 r7)
         (in o48 r17)
         (in o49 r10)
         (in o50 r1)
         (in o51 r27)
         (in o52 r3)
         (in o53 r21)
         (in o54 r2)
         (in o55 r27)
         (in o56 r5)
         (in o57 r25)
         (in o58 r5)
         (in o59 r27)
         (in o60 r6)
         (in o61 r20)
         (in o62 r5)
         (in o63 r2)
         (in o64 r20)
         (in o65 r19)
         (in o66 r4)
         (in o67 r3)
         (in o68 r25)
         (in o69 r27)
         (in o70 r15))))