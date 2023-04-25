(clear-all)

(define-model random

  ;; do not change these parameters
  (sgp :esc t :bll .5 :ol t :sim-hook "the-game-number-sims" :cache-sim-hook-results t :er t :lf 0)

  ;; adjust these as needed
  (sgp :v nil :ans .2 :mp 10.0 :rt -60)

  ;; The game-state type has the information needed to select the card that the model submits.
  (chunk-type game-state has-1 has-2 has-3 has-4 has-5 m-1st m-2nd m-3rd m-4th m-5th o-1st o-2nd o-3rd o-4th o-5th state)
  ;; The game-result type has the result of a block.
  (chunk-type game-result m-1st m-2nd m-3rd m-4th m-5th o-1st o-2nd o-3rd o-4th o-5th mresult state)

  ;; This chunk-type should be modified to contain the information needed
  ;; for your model's learning strategy

  (chunk-type learned-info mc1 action)

  ;; Declare the slots used for the goal buffer since it is
  ;; not set in the model defintion or by the productions.
  ;; See the experiment code text for more details.

  (declare-buffer-usage goal game-state :all)

  ;; Create chunks for the items used in the slots for the game
  ;; information and state

  (define-chunks start result win lose draw)

  ;; Provide a keyboard for the model's motor module to use
  (install-device '("motor" "keyboard"))

  (p submit-1
     =goal>
       isa game-state
       state start
       has-1 t
     ?manual>
       state free
    ==>
     =goal>
       state nil
     +manual>
       cmd press-key
       key 1)

  (p submit-2
     =goal>
       isa game-state
       state start
       has-2 t
     ?manual>
       state free
    ==>
     =goal>
       state nil
     +manual>
       cmd press-key
       key 2)

  (p submit-3
     =goal>
       isa game-state
       state start
       has-3 t
     ?manual>
       state free
    ==>
     =goal>
       state nil
     +manual>
       cmd press-key
       key 3)

  (p submit-4
     =goal>
       isa game-state
       state start
       has-4 t
     ?manual>
       state free
    ==>
     =goal>
       state nil
     +manual>
       cmd press-key
       key 4)

  (p submit-5
     =goal>
       isa game-state
       state start
       has-5 t
     ?manual>
       state free
    ==>
     =goal>
       state nil
     +manual>
       cmd press-key
       key 5)
)
