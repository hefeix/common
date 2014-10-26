;;; Code for processing Unix file names.
;;; Copyright (c) August, 1992 by Olin Shivers (shivers@csd.hku.hk).
;;; Please imagine a long, tedious, legalistic 5-page gnu-style copyright
;;; notice appearing here to the effect that you may use this code any
;;; way you like, as long as you don't charge money for it, remove this
;;; notice, or hold me liable for its results.

;;; Dependencies: home-directory %get-homedir getenv error
(define (%get-homedir user)
  &{/home/&[user]})

(define getenv get-environment-variable)

;;; Relevant bits of CScheme:
;;;    pathnm sfile strnin unxcwd unxdir unxpar unxprm unxpth unxunp wrkdir

;;; This code is not very efficient.

;;; Terminology:
;;; A *directory* is a file-name ending in a slash: /, or lib/tex/macros/.
;;; When a directory is being treated as simply a file, we remove the trailing
;;;   slash. This implies that the file system root, viewed as a file, not
;;;   a directory, is named by the empty string. In other words, slashes
;;;   function solely as *separators* of file-name components.
;;;
;;; A file-name is a sequence of slash-delimited directory components,
;;;   followed by an optional file component. Examples:
;;;
;;;   File name		Dir components		File component
;;;   ---------		--------------		--------------
;;;   src/des/main.c    ("src" "des")		"main.c"
;;;   /src/des/main.c   ("" "src" "des")	"main.c"
;;;   main.c		()			"main.c"
;;;   src/des/		("src" "des")		N/A
;;;
;;; Note that the relative path src/des/main.c and the absolute path
;;; /src/des/main.c are distinguished by the presence of the root component
;;; "" in the absolute path. A root component in a path always resets the
;;; directory search back to root, so /foo/bar/baz//src/des/main.c
;;; and /src/des/main.c are equivalent file-names.
;;;
;;; Put another way: in a directory path list, the empty string has a
;;; special meaning, just as "." and ".." do -- "" means "reset to root."

;;; Procedures:
;;; (file-name-as-directory fname) ; Convert a file-name to a directory.
;;;     ; Basically, add a trailing slash if needed:
;;;     ; (file-name-as-directory "src/des")  --> "src/des/"
;;;     ; (file-name-as-directory "src/des/") --> "src/des/"
;;;
;;; (directory-as-file-name fname) ; Convert a directory to a simple file-name.
;;;	; Basically, kill a trailing slash if one is present:
;;;     ; (directory-as-file-name "foo/bar/") --> "foo/bar"
;;;	; (directory-as-file-name "/")        --> ""
;;;     ; A screw case:
;;;     ; (directory-as-file-name "foo//")    --> "" (i.e., root, not "foo/")
;;;
;;; (file-name-absolute? fname) ; Does fname contain a root or ~ component?
;;;	; (file-name-absolute? "/usr/shivers")	--> #t
;;;	; (file-name-absolute? "src/des")	--> #f
;;;     ; (file-name-absolute? "~/src/des")	--> #t
;;;     ; An internal // or ~ resets to root or $HOME, respectively:
;;;     ; (file-name-absolute? "foo/~/src/des")	--> #t
;;;	; (file-name-absolute? "src//des")     --> #t (i.e., /des)
;;;	; Non-obvious case:
;;;     ; (file-name-absolute? "")             --> #t (i.e., root)
;;;
;;; (file-name-directory fname) ; Return the directory component of fname.
;;;	; (file-name-directory "/usr/shivers")   --> "/usr/"
;;;	; (file-name-directory "/usr/shivers/")  --> "/usr/shivers/"
;;;	; (file-name-directory "shivers/.login") --> "shivers/"
;;;	; ...or, #f if no directory:
;;;	; (file-name-directory "main.c")	--> #f
;;;	; (file-name-directory "")              --> #f
;;;
;;; (file-name-nondirectory fname) ; Return non-directory component of fname.
;;;	; (file-name-directory "/usr/shivers")   --> "shivers"
;;;	; (file-name-directory "/usr/shivers/")  --> ""
;;;	; (file-name-directory "shivers/.login") --> ".login"
;;;	; (file-name-directory "main.c")         --> "main.c"
;;;	; (file-name-directory "")               --> ""
;;;
;;; (split-file-name fname) ; Split a file-name into its components.
;;;     ; (split-file-name "src/des/main.c")  --> ("src" "des" "main.c")
;;;     ; (split-file-name "/src/des/main.c") --> ("" "src" "des" "main.c")
;;;     ; (split-file-name "main.c")          --> ("main.c")
;;;
;;; (path-list->file-name path-list [dir]) ; Inverse of SPLIT-FILE-NAME.
;;;     ; (path-list->file-name '("src" "des" "main.c"))    --> src/des/main.c
;;;     ; (path-list->file-name '("" "src" "des" "main.c")) --> /src/des/main.c
;;;     ; Empty components reset to root:
;;;     ; (path-list->file-name '("src" "" "des" "main.c")) --> /des/main.c
;;;	; Optional DIR arg anchors relative path-lists:
;;;     ; (path-list->file-name '("src" "des" "main.c") "/usr/shivers/")
;;;	;       --> "/usr/shivers/src/des/main.c"
;;;     ; But empty components still reset to root:
;;;     ; (path-list->file-name '("src" "" "des" "main.c") "/usr/shivers/")
;;;	;	--> /des/main.c
;;;
;;; The optional DIR argument is usefully (PWD).
;;; Note that PATH-LIST->FILE-NAME returns a file-name, not a directory.
;;; No trailing slash, including the root case.
;;;
;;; (file-name-extension fname) ; Return the file-name's extension.
;;;     ; (file-name-extension "main.c")        --> ".c"
;;;     ; (file-name-extension "main.c.old")    --> ".old"
;;;     ; (file-name-extension "/usr/shivers")	--> ""
;;;     ; Weird cases:
;;;     ; (file-name-extension "foo.")          --> "."
;;;     ; (file-name-extension "foo..")         --> "."
;;;     ; "Dot files" are not extensions:
;;;     ; (file-name-extension "/usr/shivers/.login")   --> ""
;;;
;;;
;;; (file-name-sans-extension fname) ; Return everything but the extension.
;;;     ; (file-name-sans-extension "main.c")              --> "main"
;;;     ; (file-name-sans-extension "main.c.old")          --> "main.c""
;;;     ; (file-name-sans-extension "/usr/shivers")        --> "/usr/shivers"
;;;     ; Weird cases:
;;;     ; (file-name-sans-extension "foo.")     --> "foo"
;;;     ; (file-name-sans-extension "foo..")    --> "foo."
;;;     ; "Dot files" are not extensions:
;;;     ; (file-name-sans-extension "/usr/shivers/.login")
;;;             --> "/usr/shivers/.login
;;;
;;; Note that appending the results of FILE-NAME-EXTENSION and
;;; FILE-NAME-SANS-EXTENSION in all cases produces the original file-name.
;;;
;;; (parse-file-name fname) ; Multiple values: [dir name extension]
;;; This procedure returns the three values:
;;; 1. (file-name-directory fname)
;;; 2. (file-name-sans-extension (file-name-non-directory fname))
;;; 3. (file-name-extension fname)
;;; The inverse of this procedure, in all cases, is STRING-APPEND.
;;;
;;; (expand-file-name fname [dir])
;;; - Do ~ expansion.
;;; - Simplify away occurences of ., .., and //.
;;; - If FNAME is relative, convert it to an absolute file-name, relative
;;;   to directory DIR. DIR defaults to the current working directory.
;;;
;;; The ~ expansion is not currently complete. Only the simple ~/foo
;;; case is handled, not the ~shivers/foo case. Not that ~, like //,
;;; can appear in the middle of a file-name, causing the path to its
;;; left to be discarded.
;;;
;;; (substitute-env-vars fname)
;;; Replace occurences of environment variables with their values.
;;; An environment variable is denoted by a dollar sign followed by
;;; alphanumeric chars and underscores, or is surrounded by braces.
;;;     ; (substitute-env-vars "$USER/.login") --> "shivers/.login"
;;;     ; (substitute-env-vars "${USER}_log") --> "shivers_log"
;;;
;;; This procedure is not currently implemented.
;;; ===========================================================================


(define (file-name-as-directory fname)
  (let ((len (string-length fname)))
    (if (and (> len 0)
             (char=? #\/ (string-ref fname (- len 1))))
        fname
        (string-append fname "/"))))

(define (directory-as-file-name fname)
  (let ((len (string-length fname)))
    (if (and (> len 0) (char=? #\/ (string-ref fname (- len 1))))
        (if (and (> len 1) (char=? #\/ (string-ref fname (- len 2))))
            "" ; Screw case: FNAME ended with a //.
            (substring fname 0 (- len 1))) ; Trim the trailing /.
        fname))) ; No trailing /.

(define (file-name-absolute? fname)
  (let ((path (split-file-name fname)))
    (or (member "" path)	; Reset to root.
        (member "~" path))))	; Reset to $HOME.

(define (file-name-directory fname)
  (let ((rslash (rindex fname #\/)))
    (and rslash (substring fname 0 (+ 1 rslash)))))

(define (file-name-nondirectory fname)
  (cond ((rindex fname #\/) =>
         (lambda (rslash) (substring fname
                                     (+ rslash 1)
                                     (string-length fname))))
        (else fname)))

(define (split-file-name fname)
  (letrec ((split (lambda (fname)
             (let ((slash (index fname #\/)))
               (if slash
                   (cons (substring fname 0 slash)
                         (split (substring fname (+ slash 1)
                                           (string-length fname))))
                   (list fname))))))

    ;; Before disassembly make sure it's a file-name, not a directory.
    (split (directory-as-file-name fname))))


(define (path-list->file-name pathlist . maybe-dir)
  (let ((dir (if (pair? maybe-dir)
                 (file-name-as-directory (car maybe-dir))
                 "")))
    (letrec ((insert-slashes (lambda (pathlist ans)
                       (let ((elt (car pathlist))
                             (pathlist (cdr pathlist)))
                         (cond ((null? pathlist) (cons elt ans))
                               ((string=? elt "")
                                (insert-slashes pathlist '("/")))
                               (else
                                (insert-slashes pathlist
                                                `( "/" ,elt . ,ans ))))))))
      (apply string-append (reverse (insert-slashes pathlist (list dir)))))))

(define (parse-file-name fname)
  (let ((nd (file-name-nondirectory fname)))
    (values (file-name-directory fname)
            (file-name-sans-extension nd)
            (file-name-extension nd))))


;;; Return the index of the . separating the extension from the rest of
;;; the file name. If no extension, returns an index pointing off the
;;; end of the string, i.e. (string-length fname). "Dot-files," such as
;;; /usr/shivers/.login are not considered extensions.

(define (file-name-extension-index fname)
  (let ((dot (rindex fname #\.)))
    (if (and dot
             (> dot 0)
             (not (char=? #\/ (string-ref fname (- dot 1)))))
        dot
        (string-length fname))))

(define (file-name-sans-extension fname)
  (substring fname 0 (file-name-extension-index fname)))

(define (file-name-extension fname)
  (substring fname (file-name-extension-index fname)
                   (string-length fname)))

;;; Need to handle the relative->absolute default case when (PWD) is available.
;;; Maybe this guy shouldn't convert relatives to absolutes.

(define (expand-file-name fname . maybe-dir)
  (let* ((dir (if (null? maybe-dir) "./" ; Should be (pwd).
                  (file-name-as-directory (car maybe-dir))))
         (path-list (split-file-name fname))
         ;; Now, make PATH-LIST an absolute one:
         (path-list (split-file-name (path-list->file-name path-list dir))))

    ;; Now, scan PATH-LIST chopping out occurrences of . and ..
    (let lp ((ans '()) (path-list path-list))
      (if (null? path-list) (path-list->file-name (reverse ans))
          (let* ((elt (car path-list))
                 (path-list (cdr path-list))
                 (ans (cond
                       ;; . removal
                       ((and (string=? "." elt)
                             (or (pair? path-list)  ; Ugh
                                 (pair? ans)))
                        ans)

                       ;; .. removal
                       ((and (string=? ".." elt) (pair? ans))
                        (cdr ans))

                       ; ~ expansion
                       ((and (> (string-length elt) 0)
                             (char=? #\~ (string-ref elt 0)))
                        (let ((len (string-length elt)))
                          (list (directory-as-file-name
                                 (if (= len 1)
                                     (file-name-as-directory home-directory)
                                     (home-dir (substring elt 1 len)))))))

                       (else (cons elt ans)))))
            (lp ans path-list))))))


(define (home-dir . maybe-user)
  (if (pair? maybe-user)
      (let ((user (car maybe-user)))
        (file-name-as-directory (or (%get-homedir user)
                                    (error "Cannot get user's home directory"
                                           user))))
      (file-name-as-directory home-directory)))

;;; (home-file [user] fname)

(define (home-file arg1 . maybe-arg2)
  (call-with-values
      (lambda () (if (pair? maybe-arg2)
                     (values (home-dir arg1) (car maybe-arg2))
                     (values (file-name-as-directory home-directory) arg1)))
    string-append))

;;; Sadly, Unix open(2) thinks empty strings means the pwd, not root.
;;; Too bad. Root is the consistent definition, and there is already
;;; a special notation for pwd: ".". In order to interface to the open
;;; syscall or other external agents, we have the following function,
;;; which maps "" --> "/".

(define (rootify fname) (if (string=? "" fname) "/" fname))

;;; Ugh.
(define (substitute-env-vars str)
  (let lp ((ans '()) (s str))
    (let ((len (string-length s)))
      (cond
        ((zero? len) (apply string-append (reverse! ans)))
        ((index s #\$) =>
         (lambda (i)
           (let ((ans (cons (substring s 0 i) ans))
                 (s (substring s (+ i 1) len))
                 (len (- len (+ i 1))))
             (if (zero? len) (lp ans "")
                 (let ((next-char (string-ref s 0)))
                   (cond ((char=? #\{ next-char)
                          (cond ((index s #\}) =>
                                 (lambda (i)
                                   (lp (cons (getenv (substring s 1 i)) ans)
                                       (substring s (+ i 1) len))))
                                (else (error "Unbalanced ${ delimiter in string" s))))
                         (else
                          (let ((i (or (index s #\/) len)))
                            (lp (cons (getenv (substring s 0 i)) ans)
                                (substring s i len))))))))))
        (else (lp (cons s ans) ""))))))


;;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (index str c)
  (do ((len (string-length str))
       (i 0 (+ 1 i)))
      ((or (= i len)
           (char=? c (string-ref str i)))
       (and (< i len) i))))

(define (rindex str c)
  (do ((i (- (string-length str) 1) (- i 1)))
      ((or (< i 0)
           (char=? c (string-ref str i)))
       (and (>= i 0) i))))

(define (reverse! lis)
  (let lp ((lis lis) (prev '()))
    (if (not (pair? lis)) prev
        (let ((tail (cdr lis)))
          (set-cdr! lis prev)
          (lp tail lis)))))
