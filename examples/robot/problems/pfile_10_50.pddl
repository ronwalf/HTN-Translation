(define
 (problem pfile_10_50)
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
           c r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 - ROOM
           d410 d45 d47 d79 d27 d17 d18 d38 d68 d05 - ROOMDOOR)
 (:init
  (rloc c)
  (armempty)
  (door c r5 d05)
  (door r1 r7 d17)
  (door r1 r8 d18)
  (door r2 r7 d27)
  (door r3 r8 d38)
  (door r4 r5 d45)
  (door r4 r7 d47)
  (door r4 r10 d410)
  (door r5 c d05)
  (door r5 r4 d45)
  (door r6 r8 d68)
  (door r7 r1 d17)
  (door r7 r2 d27)
  (door r7 r4 d47)
  (door r7 r9 d79)
  (door r8 r1 d18)
  (door r8 r3 d38)
  (door r8 r6 d68)
  (door r9 r7 d79)
  (door r10 r4 d410)
  (closed d47)
  (closed d79)
  (closed d38)
  (closed d05)
  (in o1 r6)
  (in o2 r3)
  (in o3 r1)
  (in o4 r4)
  (in o5 r4)
  (in o6 r7)
  (in o7 r10)
  (in o8 r4)
  (in o9 r9)
  (in o10 r9)
  (in o11 r6)
  (in o12 r5)
  (in o13 r1)
  (in o14 r7)
  (in o15 r9)
  (in o16 r6)
  (in o17 r4)
  (in o18 r7)
  (in o19 r1)
  (in o20 r2)
  (in o21 r10)
  (in o22 r8)
  (in o23 r7)
  (in o24 r4)
  (in o25 r2)
  (in o26 r9)
  (in o27 r6)
  (in o28 r3)
  (in o29 r6)
  (in o30 r1)
  (in o31 r1)
  (in o32 r2)
  (in o33 r10)
  (in o34 r9)
  (in o35 r3)
  (in o36 r2)
  (in o37 r4)
  (in o38 r10)
  (in o39 r6)
  (in o40 r4)
  (in o41 r1)
  (in o42 r1)
  (in o43 r6)
  (in o44 r5)
  (in o45 r1)
  (in o46 r3)
  (in o47 r3)
  (in o48 r8)
  (in o49 r10)
  (in o50 r7))
 (:goal (and
         (in o1 r1)
         (in o2 r8)
         (in o3 r7)
         (in o4 r4)
         (in o5 r9)
         (in o6 r6)
         (in o7 r6)
         (in o8 r8)
         (in o9 r4)
         (in o10 r4)
         (in o11 r8)
         (in o12 r1)
         (in o13 r8)
         (in o14 r4)
         (in o15 r8)
         (in o16 r3)
         (in o17 r4)
         (in o18 r8)
         (in o19 r7)
         (in o20 r6)
         (in o21 r2)
         (in o22 r9)
         (in o23 r6)
         (in o24 r10)
         (in o25 r8)
         (in o26 r4)
         (in o27 r3)
         (in o28 r7)
         (in o29 r1)
         (in o30 r9)
         (in o31 r7)
         (in o32 r4)
         (in o33 r9)
         (in o34 r10)
         (in o35 r4)
         (in o36 r5)
         (in o37 r2)
         (in o38 r9)
         (in o39 r4)
         (in o40 r6)
         (in o41 r5)
         (in o42 r1)
         (in o43 r1)
         (in o44 r5)
         (in o45 r4)
         (in o46 r10)
         (in o47 r3)
         (in o48 r9)
         (in o49 r8)
         (in o50 r5))))