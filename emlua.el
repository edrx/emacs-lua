;;; This file:
;;;   http://angg.twu.net/emacs-lua/emlua.el.html
;;;   http://angg.twu.net/emacs-lua/emlua.el
;;;           (find-angg "emacs-lua/emlua.el")
;;;      See: (find-angg "emacs-lua/emlua.cpp")
;;; Author: Eduardo Ochs <eduardoochs@gmail.com>
;;; Version: 2021may11
;;; License: public domain.
;;;
;;; (defun e () (interactive) (find-angg "emacs-lua/emlua.cpp"))
;;; (defun l () (interactive) (find-angg "emacs-lua/emlua.el"))
;;;
;;; This file contains some support functions for Nerditation's
;;; emlua.cpp.


;; «.emlua-format»		(to "emlua-format")
;; «.emlua-quote»		(to "emlua-quote")
;; «.eepitch-emlua»		(to "eepitch-emlua")
;; «.eepitch-emlua-repl»	(to "eepitch-emlua-repl")




;;;                 _                                     _       
;;;   ___ _ __ ___ | |_   _  __ _        __ _ _   _  ___ | |_ ___ 
;;;  / _ \ '_ ` _ \| | | | |/ _` |_____ / _` | | | |/ _ \| __/ _ \
;;; |  __/ | | | | | | |_| | (_| |_____| (_| | |_| | (_) | ||  __/
;;;  \___|_| |_| |_|_|\__,_|\__,_|      \__, |\__,_|\___/ \__\___|
;;;                                        |_|                    
;;
;; «emlua-format»  (to ".emlua-format")
;; «emlua-quote»  (to ".emlua-quote")

;; Example: (emlua-format "a[%s] = %s"   2.34 "foo")
;;                  --> "a[2.34] = [=[foo]=]"
;;
(defun emlua-format (fmt &rest rest)
  (apply 'format fmt (mapcar 'emlua-quote rest)))

;; Examples: (emlua-quote  2.4)
;;                    --> "2.4"
;;           (emlua-quote "foo")
;;                 --> "[=[foo]=]"
;;
(defun emlua-quote (o)
  (if (stringp o) (lua-bracket-quote o) (format "%S" o)))

;; Example: (lua-bracket-quote ".[=[ FOO ]===].")
;;                     --> "[==[.[=[ FOO ]===].]==]"
;;
(defun lua-bracket-quote (string)
  (let ((core (lua-bracket-new-core (lua-bracket-hash-table string))))
    (format "[%s[%s]%s]" core string core)))

;; Based on: (find-efunction 'replace-regexp-in-string)
;;      See: (find-elnode "Creating Hash")
;;           (find-elnode "Hash Access")
;;     Test: (lua-bracket-hash-table "[==[]===]")
;;
(defun lua-bracket-hash-table (string)
  (let* ((regexp (rx (any "[]") (one-or-more "=") (any "[]")))
         (hash-table (make-hash-table))
         (l (length string))
	 (start 0)
         mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
        (puthash (- me mb 2) t hash-table)
	(setq start me))
      hash-table)))

;; Tests:
;; (setq ht (lua-bracket-hash-table "[==[]===]"))
;; (setq ht (lua-bracket-hash-table "[=[ ]===]"))
;; (gethash 1 ht)
;; (gethash 2 ht)
;; (lua-bracket-new-core ht)
;;
(defun lua-bracket-new-core (hash-table)
  (cl-loop for k from 1
           do (if (not (gethash k hash-table))
		  (cl-return (make-string k ?=)))))




;;;                  _ _       _                          _             
;;;   ___  ___ _ __ (_) |_ ___| |__         ___ _ __ ___ | |_   _  __ _ 
;;;  / _ \/ _ \ '_ \| | __/ __| '_ \ _____ / _ \ '_ ` _ \| | | | |/ _` |
;;; |  __/  __/ |_) | | || (__| | | |_____|  __/ | | | | | | |_| | (_| |
;;;  \___|\___| .__/|_|\__\___|_| |_|      \___|_| |_| |_|_|\__,_|\__,_|
;;;           |_|                                                       
;;
;; «eepitch-emlua»  (to ".eepitch-emlua")
;; See: (find-eev "eepitch.el" "eepitch-vterm")
;;      (find-eev "eepitch.el" "eepitch-this-line")
;;
(defun eepitch-emlua ()
  "(This function is a prototype that only works in a controlled setting!)"
  (interactive)
  (eepitch '(find-ebuffer "*emlua*"))
  (setq eepitch-line 'eepitch-line-emlua))

(defun eepitch-line-emlua (line)
  "Send LINE to the emlua buffer (whatever that means)."
  (eepitch-eval-at-target-window
   '(progn (goto-char (point-max))
	   (insert (emlua-process line)))))

(defun emlua-process (inputline)
  "This is a stub - replace it by something less trivial."
  (format "%S\n" (emlua-dostring inputline)))

;; (load "~/emacs-lua/emlua.so")
;; (emlua-dostring "return 22")





;;;                 _                                  _ 
;;;   ___ _ __ ___ | |_   _  __ _       _ __ ___ _ __ | |
;;;  / _ \ '_ ` _ \| | | | |/ _` |_____| '__/ _ \ '_ \| |
;;; |  __/ | | | | | | |_| | (_| |_____| | |  __/ |_) | |
;;;  \___|_| |_| |_|_|\__,_|\__,_|     |_|  \___| .__/|_|
;;;                                             |_|      
;;
;; «eepitch-emlua-repl»  (to ".eepitch-emlua-repl")

(defun eepitch-emluarepl ()
  "(This function is a prototype that only works in a controlled setting!)"
  (interactive)
  (eepitch '(find-ebuffer "*emlua*"))
  (setq eepitch-line 'eepitch-line-emluarepl))

(defun eepitch-emluarepl-insert (str)
  "Insert STR at the end of the emlua buffer."
  (eepitch-eval-at-target-window
   '(progn (goto-char (point-max))
	   (insert str))))

(defun eepitch-emluarepl-prompt ()
  (eepitch-emluarepl-insert "> "))

(defun eepitch-line-emluarepl (line)
  (eepitch-emluarepl-insert line)
  (eepitch-emluarepl-insert "\nOUTPUT\n")
  (eepitch-emluarepl-prompt))



(defun eepitch-emluarepl-prompt ()
  (eepitch-emluarepl-insert
   (aref (emlua-dostring "return REPL:eprompt()") 0)))

(defun eepitch-emluarepl-esend0 (line)
  (setq eepitch-emluarepl-rets
   (emlua-dostring
    (format "return REPL:esend(%s)"
     (emlua-quote line)))))

(defun eepitch-emluarepl-esend1 ()
  (let* ((rets eepitch-emluarepl-rets)
         (ret0 (aref rets 0)))
    (if (equal ret0 "(success: =)")
        (eepitch-emluarepl-insert (format "%s\n" (aref rets 1))))
    (if (equal ret0 "(comp error)")
        (eepitch-emluarepl-insert (format "%s\n" (aref rets 1))))
    (if (equal ret0 "(exec error)")
        (eepitch-emluarepl-insert (format "%s\n" (aref rets 1))))
    ))
  
(defun eepitch-line-emluarepl (line)
  (eepitch-emluarepl-insert (format "%s\n" line))
  (eepitch-emluarepl-esend0 line)
  (eepitch-emluarepl-esend1)
  (eepitch-emluarepl-prompt))

