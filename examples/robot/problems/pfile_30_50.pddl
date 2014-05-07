(define
 (problem pfile_30_50)
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
           - ROOM
           d921
           d920
           d1920
           d2025
           d918
           d618
           d1922
           d321
           d1418
           d1225
           d34
           d323
           d2328
           d528
           d513
           d1317
           d717
           d1013
           d78
           d023
           d024
           d1726
           d226
           d1128
           d1517
           d2329
           d927
           d816
           d116
           d730
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r23 d023)
  (door c r24 d024)
  (door r1 r16 d116)
  (door r2 r26 d226)
  (door r3 r4 d34)
  (door r3 r21 d321)
  (door r3 r23 d323)
  (door r4 r3 d34)
  (door r5 r13 d513)
  (door r5 r28 d528)
  (door r6 r18 d618)
  (door r7 r8 d78)
  (door r7 r17 d717)
  (door r7 r30 d730)
  (door r8 r7 d78)
  (door r8 r16 d816)
  (door r9 r18 d918)
  (door r9 r20 d920)
  (door r9 r21 d921)
  (door r9 r27 d927)
  (door r10 r13 d1013)
  (door r11 r28 d1128)
  (door r12 r25 d1225)
  (door r13 r5 d513)
  (door r13 r10 d1013)
  (door r13 r17 d1317)
  (door r14 r18 d1418)
  (door r15 r17 d1517)
  (door r16 r1 d116)
  (door r16 r8 d816)
  (door r17 r7 d717)
  (door r17 r13 d1317)
  (door r17 r15 d1517)
  (door r17 r26 d1726)
  (door r18 r6 d618)
  (door r18 r9 d918)
  (door r18 r14 d1418)
  (door r19 r20 d1920)
  (door r19 r22 d1922)
  (door r20 r9 d920)
  (door r20 r19 d1920)
  (door r20 r25 d2025)
  (door r21 r3 d321)
  (door r21 r9 d921)
  (door r22 r19 d1922)
  (door r23 c d023)
  (door r23 r3 d323)
  (door r23 r28 d2328)
  (door r23 r29 d2329)
  (door r24 c d024)
  (door r25 r12 d1225)
  (door r25 r20 d2025)
  (door r26 r2 d226)
  (door r26 r17 d1726)
  (door r27 r9 d927)
  (door r28 r5 d528)
  (door r28 r11 d1128)
  (door r28 r23 d2328)
  (door r29 r23 d2329)
  (door r30 r7 d730)
  (closed d918)
  (closed d1418)
  (closed d34)
  (closed d2328)
  (closed d528)
  (closed d1317)
  (closed d717)
  (closed d78)
  (closed d023)
  (closed d1726)
  (closed d1128)
  (closed d2329)
  (closed d927)
  (closed d816)
  (closed d730)
  (in o1 r18)
  (in o2 r8)
  (in o3 r8)
  (in o4 r2)
  (in o5 r9)
  (in o6 r11)
  (in o7 r4)
  (in o8 r24)
  (in o9 r13)
  (in o10 r7)
  (in o11 r25)
  (in o12 r25)
  (in o13 r19)
  (in o14 r16)
  (in o15 r21)
  (in o16 r22)
  (in o17 r8)
  (in o18 r5)
  (in o19 r28)
  (in o20 r24)
  (in o21 r10)
  (in o22 r28)
  (in o23 r16)
  (in o24 r11)
  (in o25 r23)
  (in o26 r28)
  (in o27 r5)
  (in o28 r12)
  (in o29 r3)
  (in o30 r2)
  (in o31 r25)
  (in o32 r17)
  (in o33 r10)
  (in o34 r11)
  (in o35 r28)
  (in o36 r16)
  (in o37 r13)
  (in o38 r21)
  (in o39 r20)
  (in o40 r22)
  (in o41 r4)
  (in o42 r23)
  (in o43 r26)
  (in o44 r30)
  (in o45 r5)
  (in o46 r9)
  (in o47 r27)
  (in o48 r18)
  (in o49 r8)
  (in o50 r29))
 (:goal (and
         (in o1 r13)
         (in o2 r25)
         (in o3 r1)
         (in o4 r26)
         (in o5 r28)
         (in o6 r15)
         (in o7 r11)
         (in o8 r7)
         (in o9 r23)
         (in o10 r5)
         (in o11 r24)
         (in o12 r14)
         (in o13 r26)
         (in o14 r28)
         (in o15 r17)
         (in o16 r28)
         (in o17 r28)
         (in o18 r1)
         (in o19 r11)
         (in o20 r13)
         (in o21 r30)
         (in o22 r16)
         (in o23 r19)
         (in o24 r19)
         (in o25 r10)
         (in o26 r30)
         (in o27 r14)
         (in o28 r12)
         (in o29 r8)
         (in o30 r12)
         (in o31 r25)
         (in o32 r15)
         (in o33 r27)
         (in o34 r22)
         (in o35 r17)
         (in o36 r1)
         (in o37 r11)
         (in o38 r19)
         (in o39 r17)
         (in o40 r16)
         (in o41 r13)
         (in o42 r13)
         (in o43 r9)
         (in o44 r9)
         (in o45 r18)
         (in o46 r26)
         (in o47 r24)
         (in o48 r12)
         (in o49 r10)
         (in o50 r29))))