(define
 (problem pfile_30_30)
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
           - ROOM
           d524
           d2224
           d1022
           d710
           d79
           d713
           d1021
           d122
           d117
           d1723
           d623
           d628
           d2829
           d324
           d626
           d1629
           d1428
           d214
           d215
           d212
           d1230
           d318
           d1620
           d428
           d010
           d1827
           d011
           d828
           d625
           d1929
           - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r10 d010)
  (door c r11 d011)
  (door r1 r17 d117)
  (door r1 r22 d122)
  (door r2 r12 d212)
  (door r2 r14 d214)
  (door r2 r15 d215)
  (door r3 r18 d318)
  (door r3 r24 d324)
  (door r4 r28 d428)
  (door r5 r24 d524)
  (door r6 r23 d623)
  (door r6 r25 d625)
  (door r6 r26 d626)
  (door r6 r28 d628)
  (door r7 r9 d79)
  (door r7 r10 d710)
  (door r7 r13 d713)
  (door r8 r28 d828)
  (door r9 r7 d79)
  (door r10 c d010)
  (door r10 r7 d710)
  (door r10 r21 d1021)
  (door r10 r22 d1022)
  (door r11 c d011)
  (door r12 r2 d212)
  (door r12 r30 d1230)
  (door r13 r7 d713)
  (door r14 r2 d214)
  (door r14 r28 d1428)
  (door r15 r2 d215)
  (door r16 r20 d1620)
  (door r16 r29 d1629)
  (door r17 r1 d117)
  (door r17 r23 d1723)
  (door r18 r3 d318)
  (door r18 r27 d1827)
  (door r19 r29 d1929)
  (door r20 r16 d1620)
  (door r21 r10 d1021)
  (door r22 r1 d122)
  (door r22 r10 d1022)
  (door r22 r24 d2224)
  (door r23 r6 d623)
  (door r23 r17 d1723)
  (door r24 r3 d324)
  (door r24 r5 d524)
  (door r24 r22 d2224)
  (door r25 r6 d625)
  (door r26 r6 d626)
  (door r27 r18 d1827)
  (door r28 r4 d428)
  (door r28 r6 d628)
  (door r28 r8 d828)
  (door r28 r14 d1428)
  (door r28 r29 d2829)
  (door r29 r16 d1629)
  (door r29 r19 d1929)
  (door r29 r28 d2829)
  (door r30 r12 d1230)
  (closed d2224)
  (closed d713)
  (closed d1021)
  (closed d122)
  (closed d1723)
  (closed d2829)
  (closed d626)
  (closed d1629)
  (closed d214)
  (closed d212)
  (closed d1230)
  (closed d318)
  (closed d1929)
  (in o1 r26)
  (in o2 r25)
  (in o3 r9)
  (in o4 r12)
  (in o5 r29)
  (in o6 r13)
  (in o7 r26)
  (in o8 r7)
  (in o9 r9)
  (in o10 r4)
  (in o11 r13)
  (in o12 r22)
  (in o13 r1)
  (in o14 r13)
  (in o15 r8)
  (in o16 r14)
  (in o17 r11)
  (in o18 r21)
  (in o19 r24)
  (in o20 r22)
  (in o21 r30)
  (in o22 r17)
  (in o23 r7)
  (in o24 r2)
  (in o25 r25)
  (in o26 r26)
  (in o27 r10)
  (in o28 r9)
  (in o29 r30)
  (in o30 r26))
 (:goal (and
         (in o1 r9)
         (in o2 r18)
         (in o3 r7)
         (in o4 r20)
         (in o5 r3)
         (in o6 r26)
         (in o7 r18)
         (in o8 r10)
         (in o9 r26)
         (in o10 r21)
         (in o11 r10)
         (in o12 r13)
         (in o13 r14)
         (in o14 r2)
         (in o15 r5)
         (in o16 r28)
         (in o17 r28)
         (in o18 r8)
         (in o19 r9)
         (in o20 r22)
         (in o21 r9)
         (in o22 r11)
         (in o23 r11)
         (in o24 r26)
         (in o25 r22)
         (in o26 r1)
         (in o27 r18)
         (in o28 r15)
         (in o29 r18)
         (in o30 r17))))