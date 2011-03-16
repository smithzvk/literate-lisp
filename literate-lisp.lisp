
;; @\documentclass{article}

;; @\begin{document}

;; \title{Literate Lisp}

;; \maketitle

;; @\begin{abstract}

;; This package aims to provide very basic, yet competent literate
;; programming for Common Lisp.  It does this by having the user embed
;; typesetting commands into the comments of their document.  This
;; means that the file may be loaded/compiled by standard means.
;; Currently the only working backend is for \LaTeX.  We aim to, at
;; some point, add a backend for HTML, and possibly some of the
;; following: Exscribe, CL-Typesetting, and/or CL-PDF.  We use the
;; word backend a little liberally, as the `backend' often times shows
;; up in the frontend.

;; @\end{abstract}

(in-package :literate-lisp)

(defparameter *preamble* ""
  "A set of boiler plate typesetting code.

Currently nothing here.  I think the user should set up their document
inside the source file." )

;; @\section{Basic Principles}

;; The <<weave>> functionality creates a document ready for input to a
;; typsetting backend.

;;<<>>=
(defun read-line-from-string (string &optional (eof-error-p t) eof-value recursive-p
                              &key (start 0) end )
  (let ((ret (read-line (make-string-input-stream string start end)
                        eof-error-p eof-value recursive-p )))
    (values ret (+ start 1 (length ret))) ))

;;<<>>=
(defun weave (f-name)
  (let ((str (alexandria:read-file-into-string f-name))
        (in-chunk nil)
        (in-text nil)
        (pos 0) )
    (handler-case
        (labels ((read-line2 (string &optional (advance t))
                   (multiple-value-bind (ret new-pos)
                       (read-line-from-string string nil nil nil :start pos)
                     (when advance (setf pos new-pos))
                     ret ))
                 (read2 (string &optional (advance t))
                   (multiple-value-bind (ret new-pos)
                       (read-from-string string nil nil :start pos)
                     (when advance (setf pos new-pos))
                     ret ))
                 (read-with-comments (string &optional (advance t))
                   (let ((new-pos (nth-value 1 (read-from-string string nil nil :start pos))))
                     (let ((ret (let ((str (make-string-input-stream str pos)))
                                  (coerce (iter (repeat (- new-pos pos))
                                                (collect (read-char str)))
                                          'string ))))
                       (when advance (setf pos new-pos))
                       ret ))))
          (iter (for line = (read-line2 str)) (while line)
                (cond ((scan "^\\s*;+\\s*<<(.*?)>>\\s*=" line)
                       (setf in-chunk t
                             in-text nil )
                       (multiple-value-bind (start end m-starts m-ends)
                           (scan "^\\s*;+\\s*<<(.*?),?(\\d*?)>>\\s*=" line)
                         (declare (ignore start end))
                         (let* ((n-forms (or (parse-integer
                                              (subseq line (aref m-starts 1)
                                                      (aref m-ends 1) )
                                              :junk-allowed t )
                                             1 ))
                                (chunk-form (iter (for i below n-forms)
                                                  (collect (read2 str nil)) ))
                                (chunk (iter (for i below n-forms)
                                             (collect (read-with-comments str)) ))
                                (chunk-name
                                 (if (= 0 (length (subseq line (aref m-starts 0) (aref m-ends 0))))
                                     (second (first chunk-form))
                                     (read-from-string
                                      (subseq line (aref m-starts 0) (aref m-ends 0)) ))))
                           (collect (cons chunk-name chunk)
                             into chunks )
                           (collect chunk-name into text) )))
                      ((scan ";+\\s*@" line)
                       (setf in-chunk nil
                             in-text t )
                       (multiple-value-bind (start end m-starts m-ends)
                           (scan ";+\\s*@(.*)$" line)
                         (declare (ignore start end))
                         (collect (subseq line (aref m-starts 0) (aref m-ends 0))
                           into text )))
                      ((and in-text (scan "^\\s*$" line))
                       (collect "" into text) )
                      ((and in-text (scan "^\\s*;+" line))
                       (multiple-value-bind (start end m-starts m-ends)
                           (scan ";+(.*)" line)
                         (declare (ignore start end))
                         (collect (subseq line (aref m-starts 0) (aref m-ends 0))
                           into text )))
                      (t (setf in-chunk nil in-text nil)) )
                (finally
                 ;; Returning the text and chunks, but first we need to
                 ;; replace occurances like <<function>> with latex
                 (return
                   (list
                    (mapcar
                     (/. (line)
                        (if (stringp line)
                            (regex-replace-all
                             "<<(?i)(.*?)>>"
                             line
                             "$\\\\ll${\\\\sc \\1}$\\\\gg$" )
                            line ))
                     text )
                    chunks )))))
      (error ()
        (print (list pos in-chunk in-text))
        (error "Failed to weave document") ))))

;; @\subsection{Syntax}

;; The syntax of literate-lisp is similar to Web's.  An `@' as the
;; first non-whitespace character following a series of semicolons
;; puts the <<weave>> parser into a state where it interpretes
;; comments and blank lines as typesetting commands.  This state will
;; continue until a character is encountered that is not whitespace or
;; a Lisp comment.

;; {\em It would seem that there are instances where one would like to
;; explicitely state that we are done inputting documentation, like in
;; the case where there is a big chunk of code that is commented for
;; convenience, and not actually documentation.  Right now you can
;; throw a loose {\tt ()} in and it will tell the parser to switch out
;; of documentation parsing mode.}

;; Code chunks are designated by a label wrapped in `$<$$<$' and
;; `$>$$>$'.

;; @\subsection{User Interface}

;; The main interface to literate-lisp is via the
;; <<output-documentation>> and <<compile-documentation>> functions.
;; <<output-documentation>> takes a source file as input and writes
;; typesetting commands ready for processing to a file.  The
;; <<compile-documentation>> function calls <<output-documentation>>
;; and then passes the typesetting file to the typesetting engine,
;; \LaTeX.

;;<<>>=
(defun output-documentation (source-file
                             &optional (out-file
                                        (make-pathname :type "tex" :defaults source-file) ))
  (destructuring-bind (text chunks)
      (weave source-file)
    (with-open-file (out out-file :direction :output :if-exists :rename)
      (princ *preamble* out)
      (iter (for line in text)
            (cond ((stringp line)
                   (princ line out) )
                  ((symbolp line)
                   (let ((*print-case* :downcase))
                     (format out "~%$\\ll${\\sc ~A}$\\gg =$~%\\begin{verbatim}~%"
                             line ))
                   (iter (for source in (cdr (assoc line chunks)))
                         (princ source out) )
                   (terpri out)
                     ;; I am splitting up \end and {verbatim} here to
                     ;; make sure latex doesn't interpret it as the
                     ;; end of the environment.
                     (princ "\\end" out) (princ "{verbatim}" out) ))
            (terpri out) ))))

;;<<>>=
(defun compile-documentation (source-file)
  (output-documentation source-file)
  (trivial-shell:shell-command
   (strcat "bash -c \"cd " (directory-namestring source-file) " && "
           "latex " (namestring (make-pathname :type "tex" :defaults source-file)) "\"" )))

;; @\end{document}
