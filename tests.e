#######
#
# E-scripts on emacs-lua, a.k.a. emlua.
#
# This file is at:
# <https://github.com/edrx/emacs-lua/>
#     <http://angg.twu.net/emacs-lua/tests.e.html>
#     <http://angg.twu.net/emacs-lua/tests.e>
#              (find-angg "emacs-lua/tests.e")
#
# In the .html the sexp hyperlinks work.
# See: http://angg.twu.net/#eev
#      (find-here-links-intro)
#      (find-eev-quick-intro "6. Controlling shell-like programs")
#
#######

(defun e  () (interactive) (find-angg "emacs-lua/emlua.cpp"))
(defun l  () (interactive) (find-angg "emacs-lua/emlua.el"))
(defun et () (interactive) (find-angg "emacs-lua/tests.e"))
(defun o  () (interactive) (find-angg "emacs-lua/README.org"))




# «.compile-so»		(to "compile-so")
# «.emlua-dostring»	(to "emlua-dostring")
# «.eepitch-emlua»	(to "eepitch-emlua")
# «.eval-this»		(to "eval-this")





#####
#
# Compile emlua.so
# 2021aug24
#
#####

# «compile-so»  (to ".compile-so")
# (find-es "emacs" "vterm-recompile")
# (find-sh "locate emacs-module.h")
# (find-sh "locate lua.hpp")
# (find-sh "locate liblua5.3")
# (find-fline "~/bigsrc/emacs27/src/" "emacs-module.h")
# (find-fline "/usr/include/lua5.3/" "lua.hpp")
# (find-fline "/usr/lib/x86_64-linux-gnu/" "liblua5.3.so")

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)

# (find-fline "~/emacs-lua/")

cd ~/emacs-lua/
ls -lAF
g++ -I$HOME/bigsrc/emacs28/src \
    -I/usr/include/lua5.3 \
    -shared \
    emlua.cpp \
    -o emlua.so \
    -llua5.3

# cd ~/emacs-lua/
# ls -lAF
# g++ -I$HOME/bigsrc/emacs27/src \
#     -I/usr/include/lua5.3 \
#     -shared \
#     emlua.cpp \
#     -o emlua.so \
#     -llua5.3




#####
#
# Basic tests for emlua-dostring
# 2021aug25
#
#####

# «emlua-dostring»  (to ".emlua-dostring")
# (find-fline "~/emacs-lua/")
# (find-fline "~/emacs-lua/" "emlua.so")

 (load       "~/emacs-lua/emlua.so")

 (emlua-dostring "return 22")
 (emlua-dostring "return 22+33")
 (emlua-dostring "return 22+33, 44")
 (emlua-dostring "return 22+33, '44', {}")
 (emlua-dostring "a = nil")
 (emlua-dostring "a = 22")
 (emlua-dostring "return a+33, '44', {}")
 (emlua-dostring "return a")
 (emlua-dostring "return nil, nil, nil")

 (emlua-dostring "print(22, 33, 44)")





#####
#
# Basic tests for eepitch-emlua
# 2021aug25
#
#####

# «eepitch-emlua»  (to ".eepitch-emlua")
# (find-angg "emacs-lua/emlua.el")
# (find-angg "emacs-lua/emlua.el" "eepitch-emlua")

 (load "~/emacs-lua/emlua.el")
 (emlua-load-all)

 (eepitch-emlua)
 (eepitch-kill)
 (eepitch-emlua)
 (eepitch-emlua-prompt)
= 1 + 2
= 1 +
  2
print(22, 33)
print(22 + nil)

print(22,
      33); print(22 + nil)

=
= nil, nil, 40, nil, nil




#####
#
# eval-this
# 2021aug25
#
#####

# «eval-this»  (to ".eval-this")

 (eepitch-emlua)
 (eepitch-kill)
 (eepitch-emlua)
 (eepitch-emlua-prompt)

eval_this = '(find-angg "LUA/lua50init.lua" "split")'

                      (emlua-dostring "return eval_this")
                (aref (emlua-dostring "return eval_this") 0)
       (ee-read (aref (emlua-dostring "return eval_this") 0))
 (eval (ee-read (aref (emlua-dostring "return eval_this") 0)))

 ;; (find-angg "emacs-lua/emlua.el" "emlua-eval-this")
 (emlua-eval-this)





#  Local Variables:
#  coding: utf-8-unix
#  End:

