;;;; Setting sane default function aliases for clarity Ignoring the rest of the
;;;
;;;Lisp world for a moment, few of the naming conventions make sense to the
;;;novice reader.  This is partly because Lisp was originally created long
;;;before C was, and the world has generally inheritted a C mindset. It is also
;;;because, as the language evolved, various limitations of original function
;;;implementations caused new general use functions to be defined, without
;;;removing or rewriting the originals. This means things like 'setf' (for
;;;setting symbol values) is the normal function rather than simply 'set'. This
;;;does not lend itself to clear reading or writing, as the various renamed
;;;versions of basic functionality are the new de facto 'basic' versions of the
;;;functions. 
;;;
;;; I generally subscribe to the idea that 'The best code is the most readable
;;; code'. Readable code helps to eliminate low hanging errors by providing
;;; fewer translation steps in the mind of the average reader. By renaming some
;;; common functions we can leverage the greater understanding a reader is
;;; likely to have.
;;; 
;;; The problem of having multiple versions of *basic* functions in common-lisp
;;; is actually the biggest problem in readability.  Thus far in my learning
;;; experience of common lisp it seems there's a vagary of stumbling blocks
;;; about how to write Lisp created from this problem. Thus I've made one or
;;; two aliases for some of the more painful ones 
;;; 
;;; Mostly, however, the names I've chosen for aliases are thus because I find
;;; the name much more clearly defines what the function *does*. This is
;;; particularly true with defparameter (alias global) and defconstant
;;; (const-global).
;;; 

;; unlock packages so we can modify them
(unlock-package "COMMON-LISP-USER")
(unlock-package "COMMON-LISP")
(unlock-package "SB-EXT")


;; ensure we use the proper set version. Basic (set) only works with values,
;; (setf) seems it can set basically anything (including function definitions).
;; Don't even know what (setq) does.  What's funny is that I would actually
;; prefer to use (set), but I can't because that risks breaking something
;; internal to common-lisp :(. While one of the more minor problems, it is
;; confusing to have the basic 'set equal to' operator be a macro 'version' of
;; the core operators.
;;
;; first preserve the old usage of (=)
;(setf (fdefinition 'old-=) #'=)
;; then set our new one
;(setf (macro-function '=) (macro-function 'setf)) 
(setf (macro-function 'put) (macro-function 'setf))

;; there's like 5 ways to check equivalency, I want the standard c/java style
;; for the general usecase
;(setf (fdefinition '==) #'equal) 

;; Use the right exponent function.
(setf (fdefinition 'pow) #'expt) 

;; fairly close alignment of naming and meaning
(setf (macro-function 'global) (macro-function 'defparameter))
(setf (macro-function 'const-global) (macro-function 'defconstant))


;;; Possibly insane aliases 
 
;; symbols created via (defvar) will only get "set" once by a defvar function
;; call, subsequent calls to defvar for an existing symbol will not do
;; anything. However, it's not a constant, as it can still be changed by actual
;; (set) type calls or (defparameter).  Basically, I understand the usecase for
;; defvar (safely initializing a variable only once without erroring), but
;; I feel like the confusion it creates is mostly misguided and should be
;; generally avoided in favor of the clarity provided by defparameter and
;; defconstant (see new aliases above).
(setf (macro-function 'init-global) (macro-function 'defvar))


;; lock packages to protect against further modification
(lock-package "COMMON-LISP-USER")
(lock-package "COMMON-LISP")
(lock-package "SB-EXT")
