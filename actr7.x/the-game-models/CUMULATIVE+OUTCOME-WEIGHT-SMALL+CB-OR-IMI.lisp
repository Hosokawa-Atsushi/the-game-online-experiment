(clear-all)

(define-model CUMULATIVE+OUTCOME-WEIGHT-SMALL+CB-OR-IMI

  ;; do not change these parameters
  (sgp :esc t :bll .5 :ol t :sim-hook "the-game-number-sims" :cache-sim-hook-results t :er t :lf 0)

  ;; adjust these as needed
  (sgp :v nil :ans .2 :mp 10.0 :rt -60)

  (sgp :do-not-harvest imaginal)

  (chunk-type addition-fact addend1 addend2 sum)

  (ADD-DM (FACT11 ISA ADDITION-FACT ADDEND1 1 ADDEND2 1 SUM 2)
          (FACT21 ISA ADDITION-FACT ADDEND1 2 ADDEND2 1 SUM 3)
          (FACT31 ISA ADDITION-FACT ADDEND1 3 ADDEND2 1 SUM 4)
          (FACT41 ISA ADDITION-FACT ADDEND1 4 ADDEND2 1 SUM 5)
          (FACT51 ISA ADDITION-FACT ADDEND1 5 ADDEND2 1 SUM 6)
          (FACT61 ISA ADDITION-FACT ADDEND1 6 ADDEND2 1 SUM 7)
          (FACT71 ISA ADDITION-FACT ADDEND1 7 ADDEND2 1 SUM 8)
          (FACT81 ISA ADDITION-FACT ADDEND1 8 ADDEND2 1 SUM 9)
          (FACT91 ISA ADDITION-FACT ADDEND1 9 ADDEND2 1 SUM 10)
          (FACT101 ISA ADDITION-FACT ADDEND1 10 ADDEND2 1 SUM 11)
          (FACT111 ISA ADDITION-FACT ADDEND1 11 ADDEND2 1 SUM 12)
          (FACT121 ISA ADDITION-FACT ADDEND1 12 ADDEND2 1 SUM 13)
          (FACT131 ISA ADDITION-FACT ADDEND1 13 ADDEND2 1 SUM 14)
          (FACT12 ISA ADDITION-FACT ADDEND1 1 ADDEND2 2 SUM 3)
          (FACT22 ISA ADDITION-FACT ADDEND1 2 ADDEND2 2 SUM 4)
          (FACT32 ISA ADDITION-FACT ADDEND1 3 ADDEND2 2 SUM 5)
          (FACT42 ISA ADDITION-FACT ADDEND1 4 ADDEND2 2 SUM 6)
          (FACT52 ISA ADDITION-FACT ADDEND1 5 ADDEND2 2 SUM 7)
          (FACT62 ISA ADDITION-FACT ADDEND1 6 ADDEND2 2 SUM 8)
          (FACT72 ISA ADDITION-FACT ADDEND1 7 ADDEND2 2 SUM 9)
          (FACT82 ISA ADDITION-FACT ADDEND1 8 ADDEND2 2 SUM 10)
          (FACT92 ISA ADDITION-FACT ADDEND1 9 ADDEND2 2 SUM 11)
          (FACT102 ISA ADDITION-FACT ADDEND1 10 ADDEND2 2 SUM 12)
          (FACT112 ISA ADDITION-FACT ADDEND1 11 ADDEND2 2 SUM 13)
          (FACT122 ISA ADDITION-FACT ADDEND1 12 ADDEND2 2 SUM 14)
          (FACT13 ISA ADDITION-FACT ADDEND1 1 ADDEND2 3 SUM 4)
          (FACT23 ISA ADDITION-FACT ADDEND1 2 ADDEND2 3 SUM 5)
          (FACT33 ISA ADDITION-FACT ADDEND1 3 ADDEND2 3 SUM 6)
          (FACT43 ISA ADDITION-FACT ADDEND1 4 ADDEND2 3 SUM 7)
          (FACT53 ISA ADDITION-FACT ADDEND1 5 ADDEND2 3 SUM 8)
          (FACT63 ISA ADDITION-FACT ADDEND1 6 ADDEND2 3 SUM 9)
          (FACT73 ISA ADDITION-FACT ADDEND1 7 ADDEND2 3 SUM 10)
          (FACT83 ISA ADDITION-FACT ADDEND1 8 ADDEND2 3 SUM 11)
          (FACT93 ISA ADDITION-FACT ADDEND1 9 ADDEND2 3 SUM 12)
          (FACT103 ISA ADDITION-FACT ADDEND1 10 ADDEND2 3 SUM 13)
          (FACT113 ISA ADDITION-FACT ADDEND1 11 ADDEND2 3 SUM 14)
          (FACT14 ISA ADDITION-FACT ADDEND1 1 ADDEND2 4 SUM 5)
          (FACT24 ISA ADDITION-FACT ADDEND1 2 ADDEND2 4 SUM 6)
          (FACT34 ISA ADDITION-FACT ADDEND1 3 ADDEND2 4 SUM 7)
          (FACT44 ISA ADDITION-FACT ADDEND1 4 ADDEND2 4 SUM 8)
          (FACT54 ISA ADDITION-FACT ADDEND1 5 ADDEND2 4 SUM 9)
          (FACT64 ISA ADDITION-FACT ADDEND1 6 ADDEND2 4 SUM 10)
          (FACT74 ISA ADDITION-FACT ADDEND1 7 ADDEND2 4 SUM 11)
          (FACT84 ISA ADDITION-FACT ADDEND1 8 ADDEND2 4 SUM 12)
          (FACT94 ISA ADDITION-FACT ADDEND1 9 ADDEND2 4 SUM 13)
          (FACT104 ISA ADDITION-FACT ADDEND1 10 ADDEND2 4 SUM 14)
          (FACT15 ISA ADDITION-FACT ADDEND1 1 ADDEND2 5 SUM 6)
          (FACT25 ISA ADDITION-FACT ADDEND1 2 ADDEND2 5 SUM 7)
          (FACT35 ISA ADDITION-FACT ADDEND1 3 ADDEND2 5 SUM 8)
          (FACT45 ISA ADDITION-FACT ADDEND1 4 ADDEND2 5 SUM 9)
          (FACT55 ISA ADDITION-FACT ADDEND1 5 ADDEND2 5 SUM 10)
          (FACT65 ISA ADDITION-FACT ADDEND1 6 ADDEND2 5 SUM 11)
          (FACT75 ISA ADDITION-FACT ADDEND1 7 ADDEND2 5 SUM 12)
          (FACT85 ISA ADDITION-FACT ADDEND1 8 ADDEND2 5 SUM 13)
          (FACT95 ISA ADDITION-FACT ADDEND1 9 ADDEND2 5 SUM 14))
  (SET-BASE-LEVELS (FACT11 1000) (FACT21 1000) (FACT31 1000) (FACT41 1000)
                   (FACT51 1000) (FACT61 1000) (FACT71 1000) (FACT81 1000)
                   (FACT91 1000) (FACT101 1000) (FACT111 1000) (FACT121 1000)
                   (FACT131 1000) (FACT12 1000) (FACT22 1000) (FACT32 1000)
                   (FACT42 1000) (FACT52 1000) (FACT62 1000) (FACT72 1000)
                   (FACT82 1000) (FACT92 1000) (FACT102 1000) (FACT112 1000)
                   (FACT122 1000) (FACT13 1000) (FACT23 1000) (FACT33 1000)
                   (FACT43 1000) (FACT53 1000) (FACT63 1000) (FACT73 1000)
                   (FACT83 1000) (FACT93 1000) (FACT103 1000) (FACT113 1000)
                   (FACT14 1000) (FACT24 1000) (FACT34 1000) (FACT44 1000)
                   (FACT54 1000) (FACT64 1000) (FACT74 1000) (FACT84 1000)
                   (FACT94 1000) (FACT104 1000) (FACT15 1000) (FACT25 1000)
                   (FACT35 1000) (FACT45 1000) (FACT55 1000) (FACT65 1000)
                   (FACT75 1000) (FACT85 1000) (FACT95 1000))

  ;; The game-state type has the information needed to select the card that the model submits.
  (chunk-type game-state state has-1 has-2 has-3 has-4 has-5 m-1st m-2nd m-3rd m-4th m-5th o-1st o-2nd o-3rd o-4th o-5th want)
  ;; The game-result type has the result of a block.
  (chunk-type game-result state m-1st m-2nd m-3rd m-4th m-5th o-1st o-2nd o-3rd o-4th o-5th mresult)

  ;; This chunk-type should be modified to contain the information needed
  ;; for your model's learning strategy

  (chunk-type learned-info m-1st m-2nd m-3rd m-4th o-1st o-2nd o-3rd o-4th mresult)

  ;; Declare the slots used for the goal buffer since it is
  ;; not set in the model defintion or by the productions.
  ;; See the experiment code text for more details.

  (declare-buffer-usage goal game-state :all)

  ;; Create chunks for the items used in the slots for the game
  ;; information and state

  (define-chunks start opponent-adding retrieving subtracting confirming random memorizing model-adding submitting result win lose draw)
  (set-similarities (win lose -1) (win draw -0.5) (lose draw -0.5))

  ;; Provide a keyboard for the model's motor module to use
  (install-device '("motor" "keyboard"))

  (p 1st-start
     =goal>
       isa game-state
       state start
       m-1st nil
    ==>
     =goal>
       state retrieving
     +retrieval>
       isa learned-info
     - mresult nil  )

  (p 2nd-start
     =goal>
       isa game-state
       state start
     - m-1st nil
       m-2nd nil
       o-1st =o
     =imaginal>
       isa learned-info
       m-1st =m
    ==>
     =goal>
       state retrieving
     =imaginal>
       o-1st =o
     +retrieval>
       isa learned-info
       m-1st =m
       o-1st =o
       )

  (p 3rd-opponent-add-before
     =goal>
       isa game-state
       state start
     - m-2nd nil
       m-3rd nil
       o-2nd =act
     =imaginal>
       isa learned-info
       o-1st =last
    ==>
     =goal>
       state opponent-adding
     +retrieval>
       isa addition-fact
       addend1 =last
       addend2 =act)
  (p 3rd-opponent-add-after
     =goal>
       isa game-state
       state opponent-adding
     - m-2nd nil
       m-3rd nil
     =imaginal>
       isa learned-info
       m-2nd =m
     =retrieval>
       isa addition-fact
       sum =o
    ==>
     =goal>
       state retrieving
     =imaginal>
       o-2nd =o
     +retrieval>
       isa learned-info
       m-2nd =m
       o-2nd =o
       )

  (p 4th-opponent-add-before
     =goal>
       isa game-state
       state start
     - m-3rd nil
       m-4th nil
       o-3rd =act
     =imaginal>
       isa learned-info
       o-2nd =last
    ==>
     =goal>
       state opponent-adding
     +retrieval>
       isa addition-fact
       addend1 =last
       addend2 =act)
  (p 4th-opponent-add-after
     =goal>
       isa game-state
       state opponent-adding
     - m-3rd nil
       m-4th nil
     =imaginal>
       isa learned-info
       m-3rd =m
     =retrieval>
       isa addition-fact
       sum =o
    ==>
     =goal>
       state retrieving
     =imaginal>
       o-3rd =o
     +retrieval>
       isa learned-info
       m-3rd =m
       o-3rd =o
       )

  (p 5th-opponent-add-before
     =goal>
       isa game-state
       state start
     - m-4th nil
       m-5th nil
       o-4th =act
     =imaginal>
       isa learned-info
       o-3rd =last
    ==>
     =goal>
       state opponent-adding
     +retrieval>
       isa addition-fact
       addend1 =last
       addend2 =act)
  (p 5th-opponent-add-after
     =goal>
       isa game-state
       state opponent-adding
     - m-4th nil
       m-5th nil
     =imaginal>
     =retrieval>
       isa addition-fact
       sum =o
    ==>
     =goal>
       state random
     =imaginal>
       o-4th =o)

  (p case-base-remember-game-1st
     =goal>
       isa game-state
       state retrieving
       m-1st nil
     =retrieval>
       isa learned-info
       mresult win
       m-1st =act
    ==>
     =goal>
       state confirming
       want =act
     @retrieval>)

  (p imitation-remember-game-1st
     =goal>
       isa game-state
       state retrieving
       m-1st nil
     =retrieval>
       isa learned-info
       mresult lose
       o-1st =act
    ==>
     =goal>
       state confirming
       want =act
     @retrieval>)

  (p case-base-remember-game-2nd
     =goal>
       isa game-state
       state retrieving
     - m-1st nil
       m-2nd nil
     =imaginal>
       isa learned-info
       m-1st =last
     =retrieval>
       isa learned-info
       mresult win
       m-2nd =sum
    ==>
     =goal>
       state subtracting
     @retrieval>
     +retrieval>
       isa addition-fact
       addend1 =last
       sum =sum)

  (p imitation-remember-game-2nd
     =goal>
       isa game-state
       state retrieving
     - m-1st nil
       m-2nd nil
     =imaginal>
       isa learned-info
       o-1st =last
     =retrieval>
       isa learned-info
       mresult lose
       o-2nd =sum
    ==>
     =goal>
       state subtracting
     @retrieval>
     +retrieval>
       isa addition-fact
       addend1 =last
       sum =sum)

  (p case-base-remember-game-3rd
     =goal>
       isa game-state
       state retrieving
     - m-2nd nil
       m-3rd nil
     =imaginal>
       isa learned-info
       m-2nd =last
     =retrieval>
       isa learned-info
       mresult win
       m-3rd =sum
    ==>
     =goal>
       state subtracting
     @retrieval>
     +retrieval>
       isa addition-fact
       addend1 =last
       sum =sum)

  (p imitation-remember-game-3rd
     =goal>
       isa game-state
       state retrieving
     - m-2nd nil
       m-3rd nil
     =imaginal>
       isa learned-info
       o-2nd =last
     =retrieval>
       isa learned-info
       mresult lose
       o-3rd =sum
    ==>
     =goal>
       state subtracting
     @retrieval>
     +retrieval>
       isa addition-fact
       addend1 =last
       sum =sum)

  (p case-base-remember-game-4th
     =goal>
       isa game-state
       state retrieving
     - m-3rd nil
       m-4th nil
     =imaginal>
       isa learned-info
       m-3rd =last
     =retrieval>
       isa learned-info
       mresult win
       m-4th =sum
    ==>
     =goal>
       state subtracting
     @retrieval>
     +retrieval>
       isa addition-fact
       addend1 =last
       sum =sum)

  (p imitation-remember-game-4th
     =goal>
       isa game-state
       state retrieving
     - m-3rd nil
       m-4th nil
     =imaginal>
       isa learned-info
       o-3rd =last
     =retrieval>
       isa learned-info
       mresult lose
       o-4th =sum
    ==>
     =goal>
       state subtracting
     @retrieval>
     +retrieval>
       isa addition-fact
       addend1 =last
       sum =sum)



  (p remember-draw-game
     =goal>
       isa game-state
       state retrieving
     =retrieval>
       isa learned-info
       mresult draw
    ==>
     =goal>
       state random
     @retrieval>)

  (p cannot-remember-game
     =goal>
       isa game-state
       state retrieving
     ?retrieval>
       buffer failure
    ==>
     =goal>
       state random)

  (p subtract-after
     =goal>
       isa game-state
       state subtracting
     =retrieval>
       isa addition-fact
       addend2 =act
    ==>
     =goal>
       state confirming
       want =act)

  (p want-and-have-1
     =goal>
       isa game-state
       state confirming
       want 1
       has-1 t
    ==>
     =goal>
       state memorizing)

  (p want-and-have-2
     =goal>
       isa game-state
       state confirming
       want 2
       has-2 t
    ==>
     =goal>
       state memorizing)

  (p want-and-have-3
     =goal>
       isa game-state
       state confirming
       want 3
       has-3 t
    ==>
     =goal>
       state memorizing)

  (p want-and-have-4
     =goal>
       isa game-state
       state confirming
       want 4
       has-4 t
    ==>
     =goal>
       state memorizing)

  (p want-and-have-5
     =goal>
       isa game-state
       state confirming
       want 5
       has-5 t
    ==>
     =goal>
       state memorizing)

  (p want-but-not-have
     =goal>
       isa game-state
       state confirming
     - want nil
    ==>
     =goal>
       state random
       want nil)
  (spp want-but-not-have :u -1000)

  (p choose-1
     =goal>
       isa game-state
       state random
       has-1 t
    ==>
     =goal>
       state memorizing
       want 1)

  (p choose-2
     =goal>
       isa game-state
       state random
       has-2 t
    ==>
     =goal>
       state memorizing
       want 2)

  (p choose-3
     =goal>
       isa game-state
       state random
       has-3 t
    ==>
     =goal>
       state memorizing
       want 3)

  (p choose-4
     =goal>
       isa game-state
       state random
       has-4 t
    ==>
     =goal>
       state memorizing
       want 4)

  (p choose-5
     =goal>
       isa game-state
       state random
       has-5 t
    ==>
     =goal>
       state memorizing
       want 5)

  (p 1st-memorize
     =goal>
       isa game-state
       state memorizing
       m-1st nil
       want =act
    ==>
     =goal>
       state submitting
     +imaginal>
       isa learned-info
       m-1st =act)

  (p 2nd-model-add-before
     =goal>
       isa game-state
       state memorizing
     - m-1st nil
       m-2nd nil
       want =act
     =imaginal>
       isa learned-info
       m-1st =last
    ==>
     =goal>
       state model-adding
     +retrieval>
       isa addition-fact
       addend1 =last
       addend2 =act)

  (p 2nd-model-add-after
     =goal>
       isa game-state
       state model-adding
     - m-1st nil
       m-2nd nil
     =imaginal>
       isa learned-info
     =retrieval>
       isa addition-fact
       sum =sum
    ==>
     =goal>
       state submitting
     =imaginal>
       m-2nd =sum)

  (p 3rd-model-add-before
     =goal>
       isa game-state
       state memorizing
     - m-2nd nil
       m-3rd nil
       want =act
     =imaginal>
       isa learned-info
       m-2nd =last
    ==>
     =goal>
       state model-adding
     +retrieval>
       isa addition-fact
       addend1 =last
       addend2 =act)

  (p 3rd-model-add-after
     =goal>
       isa game-state
       state model-adding
     - m-2nd nil
       m-3rd nil
     =imaginal>
       isa learned-info
     =retrieval>
       isa addition-fact
       sum =sum
    ==>
     =goal>
       state submitting
     =imaginal>
       m-3rd =sum)

  (p 4th-model-add-before
     =goal>
       isa game-state
       state memorizing
     - m-3rd nil
       m-4th nil
       want =act
     =imaginal>
       isa learned-info
       m-3rd =last
    ==>
     =goal>
       state model-adding
     +retrieval>
       isa addition-fact
       addend1 =last
       addend2 =act)

  (p 4th-model-add-after
     =goal>
       isa game-state
       state model-adding
     - m-3rd nil
       m-4th nil
     =imaginal>
       isa learned-info
     =retrieval>
       isa addition-fact
       sum =sum
    ==>
     =goal>
       state submitting
     =imaginal>
       m-4th =sum)

  (p 5th-memorize
     =goal>
       isa game-state
       state memorizing
     - m-4th nil
       m-5th nil
    ==>
     =goal>
       state submitting)

  (p submit
     =goal>
       isa game-state
       state submitting
       want =act
     ?manual>
       state free
    ==>
     @goal>
     +manual>
       cmd press-key
       key =act)

  (p imagine-result
     =goal>
       isa game-result
       state result
       mresult =res
     =imaginal>
       isa learned-info
    ==>
     =goal>
       state clearing
     =imaginal>
       mresult =res)

  (p clear-imaginal-chunk
     =goal>
       isa game-result
       state clearing
     ?imaginal>
       state free
       buffer full
    ==>
     @goal>
     -imaginal>)
)
