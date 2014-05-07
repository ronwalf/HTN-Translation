(define
 (problem pfile_10_30)
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
           c r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 - ROOM
           d02 d25 d35 d36 d010 d16 d410 d47 d59 d89 - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r2 d02)
  (door c r10 d010)
  (door r1 r6 d16)
  (door r2 c d02)
  (door r2 r5 d25)
  (door r3 r5 d35)
  (door r3 r6 d36)
  (door r4 r7 d47)
  (door r4 r10 d410)
  (door r5 r2 d25)
  (door r5 r3 d35)
  (door r5 r9 d59)
  (door r6 r1 d16)
  (door r6 r3 d36)
  (door r7 r4 d47)
  (door r8 r9 d89)
  (door r9 r5 d59)
  (door r9 r8 d89)
  (door r10 c d010)
  (door r10 r4 d410)
  (closed d25)
  (closed d010)
  (closed d47)
  (closed d89)
  (in o1 r2)
  (in o2 r8)
  (in o3 r10)
  (in o4 r6)
  (in o5 r10)
  (in o6 r1)
  (in o7 r2)
  (in o8 r2)
  (in o9 r6)
  (in o10 r10)
  (in o11 r1)
  (in o12 r9)
  (in o13 r9)
  (in o14 r10)
  (in o15 r4)
  (in o16 r2)
  (in o17 r3)
  (in o18 r6)
  (in o19 r3)
  (in o20 r4)
  (in o21 r3)
  (in o22 r6)
  (in o23 r10)
  (in o24 r9)
  (in o25 r3)
  (in o26 r8)
  (in o27 r3)
  (in o28 r6)
  (in o29 r4)
  (in o30 r4))
 (:goal (and
         (in o1 r8)
         (in o2 r7)
         (in o3 r8)
         (in o4 r1)
         (in o5 r2)
         (in o6 r7)
         (in o7 r9)
         (in o8 r3)
         (in o9 r1)
         (in o10 r10)
         (in o11 r5)
         (in o12 r5)
         (in o13 r8)
         (in o14 r6)
         (in o15 r10)
         (in o16 r1)
         (in o17 r10)
         (in o18 r10)
         (in o19 r7)
         (in o20 r9)
         (in o21 r9)
         (in o22 r6)
         (in o23 r9)
         (in o24 r7)
         (in o25 r5)
         (in o26 r4)
         (in o27 r3)
         (in o28 r7)
         (in o29 r10)
         (in o30 r3))))