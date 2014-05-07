(define
 (problem pfile_30_10)
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
           - ROOM
           d1214
           d1422
           d822
           d823
           d1523
           d2326
           d2330
           d2730
           d1130
           d1113
           d230
           d630
           d610
           d1021
           d1721
           d315
           d1218
           d1216
           d026
           d04
           d45
           d819
           d1129
           d112
           d17
           d69
           d820
           d2024
           d2428
           d725
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r4 d04)
  (door c r26 d026)
  (door r1 r7 d17)
  (door r1 r12 d112)
  (door r2 r30 d230)
  (door r3 r15 d315)
  (door r4 c d04)
  (door r4 r5 d45)
  (door r5 r4 d45)
  (door r6 r9 d69)
  (door r6 r10 d610)
  (door r6 r30 d630)
  (door r7 r1 d17)
  (door r7 r25 d725)
  (door r8 r19 d819)
  (door r8 r20 d820)
  (door r8 r22 d822)
  (door r8 r23 d823)
  (door r9 r6 d69)
  (door r10 r6 d610)
  (door r10 r21 d1021)
  (door r11 r13 d1113)
  (door r11 r29 d1129)
  (door r11 r30 d1130)
  (door r12 r1 d112)
  (door r12 r14 d1214)
  (door r12 r16 d1216)
  (door r12 r18 d1218)
  (door r13 r11 d1113)
  (door r14 r12 d1214)
  (door r14 r22 d1422)
  (door r15 r3 d315)
  (door r15 r23 d1523)
  (door r16 r12 d1216)
  (door r17 r21 d1721)
  (door r18 r12 d1218)
  (door r19 r8 d819)
  (door r20 r8 d820)
  (door r20 r24 d2024)
  (door r21 r10 d1021)
  (door r21 r17 d1721)
  (door r22 r8 d822)
  (door r22 r14 d1422)
  (door r23 r8 d823)
  (door r23 r15 d1523)
  (door r23 r26 d2326)
  (door r23 r30 d2330)
  (door r24 r20 d2024)
  (door r24 r28 d2428)
  (door r25 r7 d725)
  (door r26 c d026)
  (door r26 r23 d2326)
  (door r27 r30 d2730)
  (door r28 r24 d2428)
  (door r29 r11 d1129)
  (door r30 r2 d230)
  (door r30 r6 d630)
  (door r30 r11 d1130)
  (door r30 r23 d2330)
  (door r30 r27 d2730)
  (closed d1214)
  (closed d2330)
  (closed d1130)
  (closed d230)
  (closed d630)
  (closed d1021)
  (closed d1721)
  (closed d026)
  (closed d45)
  (closed d1129)
  (closed d17)
  (closed d69)
  (closed d2428)
  (closed d725)
  (in o1 r3)
  (in o2 r18)
  (in o3 r13)
  (in o4 r19)
  (in o5 r8)
  (in o6 r27)
  (in o7 r6)
  (in o8 r18)
  (in o9 r18)
  (in o10 r19))
 (:goal (and
         (in o1 r23)
         (in o2 r30)
         (in o3 r19)
         (in o4 r6)
         (in o5 r8)
         (in o6 r9)
         (in o7 r1)
         (in o8 r8)
         (in o9 r1)
         (in o10 r8))))