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
;; «.load-everything»		(to "load-everything")
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





;;;  _                 _ 
;;; | | ___   __ _  __| |
;;; | |/ _ \ / _` |/ _` |
;;; | | (_) | (_| | (_| |
;;; |_|\___/ \__,_|\__,_|
;;;                      
;; «load-everything»  (to ".load-everything")

(setq emlua-dot-so       "~/emacs-lua/emlua.so")
(setq emlua-dot-el       "~/emacs-lua/emlua.el")
(setq emlua-luainit-file "~/LUA/lua50init.lua")
(setq emlua-edrxrepl-dir "~/edrxrepl/")

;; Test: (find-estring (emlua-init-lua-0))
;;
(defun emlua-init-lua-0 ()
  (format "
     dofile '%s'
     package.path = '%s?.lua;'..package.path
     require 'edrxrepl'
     REPL = EdrxRepl.new()
     return EdrxRepl, PP
     "
     (ee-expand emlua-luainit-file)
     (ee-expand emlua-edrxrepl-dir)))

;; Test: (emlua-load-all)
;;
(defun emlua-load-all ()
  (list (load emlua-dot-so)
	(emlua-dostring (emlua-init-lua-0))))




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
(defface emlua-prompt-face
  '((t (:foreground "RoyalBlue3")))
  "")

(defface emlua-user-input-face
  '((t (:foreground "orange1")))
  "")




;;;                 _                                  _ 
;;;   ___ _ __ ___ | |_   _  __ _       _ __ ___ _ __ | |
;;;  / _ \ '_ ` _ \| | | | |/ _` |_____| '__/ _ \ '_ \| |
;;; |  __/ | | | | | | |_| | (_| |_____| | |  __/ |_) | |
;;;  \___|_| |_| |_|_|\__,_|\__,_|     |_|  \___| .__/|_|
;;;                                             |_|      
;;
;; «eepitch-emlua-repl»  (to ".eepitch-emlua-repl")

(defun eepitch-emlua ()
  "Setup eepitch-ing to an emlua buffer.
This function is a prototype that only works in a controlled setting."
  (interactive)
  (eepitch '(find-ebuffer "*emlua*"))
  (setq eepitch-line 'eepitch-emlua-esend))

;; See: (find-angg "edrxrepl/edrxrepl.lua" "EdrxRepl-emacs")
;;      (find-angg "edrxrepl/edrxrepl.lua" "EdrxRepl-emacs" "erepltest =")
;;      (find-angg "edrxrepl/edrxrepl.lua" "EdrxRepl-emacs" "esend =")
;;
(defun eepitch-emlua-insert (str &optional face)
  "Insert STR at the end of the emlua buffer."
  (if face (setq str (propertize str 'face face)))
  (eepitch-eval-at-target-window
   '(progn (goto-char (point-max))
	   (insert str))))

(defun eepitch-emlua-prompt ()
  "Insert the result of REPL:eprompt() at the end of the *emlua* buffer."
  (eepitch-emlua-insert
   (aref (emlua-dostring "return REPL:eprompt()") 0)
   'emlua-prompt-face))

(defvar eepitch-emlua-esend0 nil
  "The results of the last call to `eepitch-emlua-esend0'.")

(defun eepitch-emlua-esend0 (line)
  "Run REPL:esend(LINE) and save the results in `eepitch-emlua-esend0'."
  (setq eepitch-emlua-esend0
	(emlua-dostring
	 (format "return REPL:esend(%s)"
		 (emlua-quote line)))))

(defun eepitch-emlua-esend1 ()
  "Insert the results in `eepitch-emlua-esend0' in the right way."
  (let* ((rets eepitch-emlua-esend0)
	 (len  (length rets))
         (ret0 (aref rets 0))
	 (ret1 (and (< 1 (length rets)) (aref rets 1))))
    (if ret1 (eepitch-emlua-insert ret1))))

(defun eepitch-emlua-esend (line)
  (eepitch-emlua-insert (format "%s\n" line) 'emlua-user-input-face)
  (eepitch-emlua-esend0 line)
  (eepitch-emlua-esend1)
  (eepitch-emlua-prompt))

