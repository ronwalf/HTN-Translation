(define (problem pfile_med10_3_1)
  (:domain medical)
  (:objects
    i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 - ILLNESS
    p1 p2 p3 - PERSON
  )
  (:init
    (ndead p1)
    (ndead p2)
    (ndead p3)
    (unknown (ill p1 i5))
    (unknown (ill p1 i7))
    (unknown (ill p1 i8))
    (unknown (ill p1 i10))
    (unknown (ill p2 i3))
    (unknown (ill p2 i5))
    (unknown (ill p2 i6))
    (unknown (ill p2 i8))
    (unknown (ill p2 i9))
    (unknown (ill p3 i1))
    (unknown (ill p3 i3))
    (unknown (ill p3 i5))
    (unknown (ill p3 i7))
    (unknown (ill p3 i10))
    (oneof 
      (ill p1 i5)
      (ill p1 i7)
      (ill p1 i8)
      (ill p1 i10))
    (oneof 
      (ill p2 i3)
      (ill p2 i5)
      (ill p2 i6)
      (ill p2 i8)
      (ill p2 i9))
    (oneof 
      (ill p3 i1)
      (ill p3 i3)
      (ill p3 i5)
      (ill p3 i7)
      (ill p3 i10))
 
 )
  (:goal
    (and
      (ill p1 i0)
      (ill p2 i0)
      (ill p3 i0)
      (ndead p1)
      (ndead p2)
      (ndead p3)
    )
  )
)
