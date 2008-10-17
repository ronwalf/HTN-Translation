(defdomain towers (
 (:method (shiftTower ?t1 ?t2 ?t3)
  ; shift tower from ?t1 to ?t3 using ?t2
  ((towerTop ?r ?t1))
  (
   (markSizes)
   (!!assert ((goalClear ?t1) (goalClear ?t2)))
   (selectDirection ?r ?t1 ?t2 ?t3)
  ))

 (:method (markSizes)
  ringsVsTowers
  ((ring ?r) (tower ?t) (not (smallerThan ?r ?t)))
  ((!!assert ((smallerThan ?r ?t))) (markSizes))
  
  direct
  ((st ?r1 ?r2) (not (smallerThan ?r1 ?r2)))
  ((!!assert ((smallerThan ?r1 ?r2))) (markSizes))

  transitive
  ((st ?r1 ?r2) (smallerThan ?r2 ?o3) (not (smallerThan ?r1 ?o3)))
  ((!!assert ((smallerThan ?r1 ?o3))) (markSizes))

  finished
  ())

 (:method (selectDirection ?r ?t1 ?t2 ?t3)
  startShift
  (on ?r ?t1)
  ((rotateTower ?t1 ?t3 ?t2))

  switchDirection
  (on ?r ?r1)
  ((selectDirection ?r1 ?t1 ?t3 ?t2)))

 (:method (rotateTower ?t1 ?t2 ?t3)
  allClear
  (
   (towerTop ?t2 ?t2) (towerTop ?t3 ?t3) 
   (goalClear ?t2) (goalClear ?t3)
  )
  ()

  rotateTop
  ()
  ((exchange ?t1 ?t2)
   (exchange ?t1 ?t3)
   (rotateTower ?t2 ?t3 ?t1)
  ))



 (:method (exchange ?t1 ?t2)
  empty
  ((towerTop ?t1 ?t1) (towerTop ?t2 ?t2))
  ()

  lr
  ((towerTop ?r ?t1) (on ?r ?o1) (towerTop ?o2 ?t2) (smallerThan ?r ?o2))
  ((!move ?r ?o1 ?t1 ?o2 ?t2))

  rl
  ((towerTop ?r ?t2) (on ?r ?o2) (towerTop ?o1 ?t1) (smallerThan ?r ?o1))
  ((!move ?r ?o2 ?t2 ?o1 ?t1)))


 (:operator (!move ?r ?o1 ?t1 ?o2 ?t2)
  (
   ;(on ?r ?o1) 
   ;(towerTop ?r ?t1)
   ;(smallerThan ?r ?o2) 
   ;(towerTop ?o2 ?t2)
  )
  ((on ?r ?o1) (towerTop ?r ?t1) (towerTop ?o2 ?t2))
  ((on ?r ?o2) (towerTop ?o1 ?t1) (towerTop ?r ?t2)))
   

 ;(:- (smallerThan ?r ?o)
 ; ringsVsTowers
 ; ((ring ?r) (tower ?o))
 ; st
 ; ((st ?r ?o))
 ; transitivity
 ; ((st ?r ?r1) (smallerThan ?r1 ?o)))

 (:operator (!!assert ?g)
  ()
  ()
  ?g
  0)

 (:operator (!!retract ?g)
  () 
  ?g 
  () 
  0)

))
