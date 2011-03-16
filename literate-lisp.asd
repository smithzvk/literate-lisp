
(asdf:defsystem :literate-lisp
  :author "Zach Smith"
  :license "GPLv3 or later"
  :serial t
  :components ((:file "packages")
               (:file "literate-lisp") )
  :depends-on (:cl-ppcre :toolbox :editor-hints :trivial-shell) )

