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
# Warning: in some places I use ~/emacs-lua/, in others I use
# ~/usrc/emacs-lua/... I need to clean that up.
#
#######

(defun e  () (interactive) (find-angg "emacs-lua/emlua.cpp"))
(defun l  () (interactive) (find-angg "emacs-lua/emlua.el"))
(defun et () (interactive) (find-angg "emacs-lua/tests.e"))
(defun o  () (interactive) (find-angg "emacs-lua/README.org"))




# «.find-angg-and-find-es»	(to "find-angg-and-find-es")
# «.clone-emacs-repo»		(to "clone-emacs-repo")
# «.compile-emacs28»		(to "compile-emacs28")
#
# «.download-emlua»		(to "download-emlua")
# «.compile-so»			(to "compile-so")
# «.emlua-dostring»		(to "emlua-dostring")
# «.eepitch-emlua»		(to "eepitch-emlua")
# «.eval-this»			(to "eval-this")





#####
#
# Make the `find-angg' and `find-es' hyperlinks use `find-wget'
# 2021sep10
#
#####

# «find-angg-and-find-es»  (to ".find-angg-and-find-es")
# From: (find-angg ".emacs.templates" "find-anggwget-links")
# (find-anggwget-links)

;; Versions that only work in my machine:
;;
(progn

  (code-c-d "angg" "~/" :anchor :grep)
  (code-c-d "es"   "$ES/")
  (defun find-es (file &rest rest)
    (apply 'find-anchor (ee-esfile (concat file ".e")) rest))

)

;; Versions that use `find-wget' to access the
;; public copies of my files at angg.twu.net:
;;
(progn

  (defun find-angg (fname &rest rest)
    (apply 'find-wgeta (format "http://angg.twu.net/%s" fname) rest))
  (defun find-es (fname &rest rest)
    (apply 'find-wgeta (format "http://angg.twu.net/e/%s.e" fname) rest))

)

;; Tests:
;; (find-angg "e/bullseye.e")
;; (find-angg "e/bullseye.e" "2021aug16")
;; (find-es     "bullseye")
;; (find-es     "bullseye"   "2021aug16")





#####
#
# Make a clone of the git repository of Emacs in ~/bigsrc/emacs/
# 2021sep10
#
#####

# «clone-emacs-repo»  (to ".clone-emacs-repo")
# From: (find-es "git" "emacs-from-git")
#  See: (find-git-links "git://git.sv.gnu.org/emacs" "emacs")
#       (find-man "1 git-clone")
#       (find-man "1 git-pull")

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)

# (find-fline "~/bigsrc/")
# (find-fline "~/bigsrc/emacs/")

rm -Rfv  ~/bigsrc/emacs/
mkdir -p ~/bigsrc/emacs/
cd       ~/bigsrc/
# The "git clone" below takes a long time.
git clone git://git.sv.gnu.org/emacs
cd       ~/bigsrc/emacs/

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
cd       ~/bigsrc/emacs/
git pull

# In my machine I have these directories:
#   ~/bigsrc/emacs/   - just a clone of the git repository 
#   ~/bigsrc/emacs26/ - I compile Emacs26 here (mainly for tests)
#   ~/bigsrc/emacs27/ - I compile Emacs27 here (mainly for tests)
#   ~/bigsrc/emacs28/ - I compile Emacs28 here (the version that I use).

# I am sure that there are better ways to keep several versions of a
# program available at the same time, but I don't know git well and I
# have lots of free space in my HD, so I use this.




#####
#
# Compile Emacs 28 (using a copy of ~/bigsrc/emacs/)
# 2021sep10
#
#####

# «compile-emacs28»  (to ".compile-emacs28")
# From: (find-es "git" "emacs28-from-git")
#  See: (find-git-links "git://git.sv.gnu.org/emacs" "emacs")
#       (find-elnode "Dynamic Modules")
#       (find-elnode "Dynamic Modules" "--with-modules")

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
# (find-fline "~/bigsrc/emacs28/")
rm -Rfv    ~/bigsrc/emacs28/
mkdir      ~/bigsrc/emacs28/
cd         ~/bigsrc/emacs28/

cd         ~/bigsrc/emacs/
cp -a .* * ~/bigsrc/emacs28/
cd         ~/bigsrc/emacs28/
git clean -dfx
# git pull


 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
sudo apt-get build-dep emacs
sudo apt-get install   lua5.3 liblua5.3-dev



 (eepitch-shell2)
 (eepitch-kill)
 (eepitch-shell2)
cd         ~/bigsrc/emacs28/

export PAGER=cat
git branch --list -a
git for-each-ref
git log --oneline --graph --all -20

# (find-gitk    "~/bigsrc/emacs28/")
# (magit-status "~/bigsrc/emacs28/")
# (find-fline   "~/bigsrc/emacs28/")

git checkout master
./autogen.sh                2>&1 | tee oa
# ./configure               2>&1 | tee oc
./configure --with-modules  2>&1 | tee oc
make                        2>&1 | tee om
make TAGS                   2>&1 | tee omT

# (find-fline   "~/bigsrc/emacs28/src/" " emacs")

~/bigsrc/emacs28/src/emacs




#####
#
# Download emacs-lua and Compile emlua.so
# 2021aug24
#
#####

# «download-emlua»  (to ".download-emlua")
# «compile-so»  (to ".compile-so")
# (find-git-links "https://github.com/edrx/emacs-lua" "emacslua")
# (find-es "emacs" "vterm-recompile")
# (find-sh "locate emacs-module.h")
# (find-sh "locate lua.hpp")
# (find-sh "locate liblua5.3")
# (find-fline "~/bigsrc/emacs28/src/" "emacs-module.h")
# (find-fline "/usr/include/lua5.3/" "lua.hpp")
# (find-fline "/usr/lib/x86_64-linux-gnu/" "liblua5.3.so")

# (find-fline "~/emacs-lua/")
# (find-fline "~/usrc/emacs-lua/")

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)

rm -Rfv ~/usrc/emacs-lua/
cd      ~/usrc/
git clone https://github.com/edrx/emacs-lua
cd      ~/usrc/emacs-lua/

ls -lAF
g++ -I$HOME/bigsrc/emacs28/src \
    -I/usr/include/lua5.3 \
    -shared \
    emlua.cpp \
    -o emlua.so \
    -llua5.3
ls -lAF

# (find-fline "~/usrc/emacs-lua/")
# (find-gitk  "~/usrc/emacs-lua/")
# (code-c-d "emacslua" "~/usrc/emacs-lua/")
# (find-emacsluafile "")




#####
#
# Basic tests for emlua-dostring
# 2021aug25
#
#####

# «emlua-dostring»  (to ".emlua-dostring")
# (find-fline "~/usrc/emacs-lua/")
# (find-fline "~/usrc/emacs-lua/" "emlua.so")

 (load       "~/usrc/emacs-lua/emlua.so")
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

 (load "~/usrc/emacs-lua/emlua.el")
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

