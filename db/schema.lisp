(in-package :cl-user)
(ql:quickload :datafly)
(ql:quickload :sxql)
(defpackage the-game.db
  (:use :cl :datafly :sxql)
  (:export :get-number :insert-trial-and-get-number :set-start-time :set-finish-time :insert-result :retrieve-another-action :insert-grade :insert-memory :update-grade :update-memory))
(in-package :the-game.db)

(unless *connection*
  (connect-toplevel :sqlite3 :database-name #P"db/result.db" :busy-timeout 60000))

(defun initialize-database ()
  (execute
    (create-table :trial
      ((trial-number :type 'integer :primary-key t)
        (player-a :not-null t) (player-b :not-null t)
        (start-time) (finish-time))))
  (execute
    (create-table :result
      ((trial-number) (block) (round) (player)
        (card :not-null t) (time :not-null t))
      (foreign-key '(:trial-number) :references '(:trial :trial-number))
      (primary-key '(:trial-number :block :round :player))))
  (execute
    (create-table :grade
      ((trial-number) (player) (won-times) (lost-times) (drew-times))
      (foreign-key '(:trial-number) :references '(:trial :trial-number))
      (primary-key '(:trial-number :player))))
  (execute
    (create-table :memory
      ((trial-number) (player) (dm-length))
      (foreign-key '(:trial-number) :references '(:trial :trial-number))
      (primary-key '(:trial-number :player)))))

(defun get-number (player-a player-b)
  (let ((it (retrieve-all-values
              (select :trial-number (from :trial)
                (where (:and (:= :player-a player-a) (:= :player-b player-b)))))))
    (if it (apply #'max it) it)))

(defun insert-trial-and-get-number (player-a player-b)
  (execute
    (insert-into :trial
      (set= :player-a player-a :player-b player-b)))
  (get-number player-a player-b))

(defun set-start-time (trial-number)
  (execute
    (update :trial
      (set= :start-time (:datetime "now" "localtime"))
      (where (:= :trial-number trial-number)))))

(defun set-finish-time (trial-number)
  (execute
    (update :trial
      (set= :finish-time (:datetime "now" "localtime"))
      (where (:= :trial-number trial-number)))))

(defun insert-result (trial-number block round player card time)
  (execute
    (insert-into :result
      (set= :trial-number trial-number :block block :round round :player player :card card :time time))))

(defun retrieve-another-action (trial-number block round player)
  (retrieve-one-value
    (select :card (from :result)
      (where
        (:and (:= :trial-number trial-number)
          (:= :block block) (:= :round round) (:= :player player))))))

(defun insert-grade (trial-number player)
  (execute
    (insert-into :grade
      (set= :trial-number trial-number :player player))))

(defun insert-memory (trial-number player)
  (execute
    (insert-into :memory
      (set= :trial-number trial-number :player player))))

(defun update-grade (trial-number player won-times lost-times drew-times)
  (execute
    (update :grade
      (set= :won-times won-times :lost-times lost-times :drew-times drew-times)
      (where (:and (:= :trial-number trial-number) (:= :player player))))))

(defun update-memory (trial-number player dm-length)
  (execute
    (update :memory
      (set= :dm-length dm-length)
      (where (:and (:= :trial-number trial-number) (:= :player player))))))
