;;;; Setting sane default function aliases for clarity
;;; Ignoring the rest of the Lisp world for a moment, few of the naming conventions make sense to the novice reader.
;;; This is partly because Lisp was originally created long before C was, and the world has generally inheritted a 
;;; C mindset. It is also because, as the language evolved, various limitations of original function 
;;; implementations caused new general use functions to be defined, without removing or rewriting the originals. This
;;; means things like 'setf' (for setting symbol values) is the normal function rather than simply 'set'. This does 
;;; not lend itself to clear reading or writing, as the various renamed versions of basic functionality are the new de facto 
;;; 'basic' versions of the functions. 
;;;
;;; I think following the 'rule of least suprise' is useful here. Yes, Lisp is its own universe, but
;;; the C world is is so much more ubiqutous I think it's beneficial to write code that's easier to understand from a
;;; C naming mindset. Obviously, implementing a 'c syntax' would break the common-lisp definition (besides being undesirable
;;; for other reasons). However, because lisp is built around making a very customizable language (assuming stringent basic 
;;; syntax), renaming for readability is well within Lisp's wheelhouse.
;;;
;;; I generally subscribe to the idea that 'The best code is the most readable code'. Readable code helps to eliminate
;;; low hanging errors by providing fewer translation steps in the mind of the average reader. By renaming some common 
;;; functions to follow the c standards (the de facto modern standard that most everyone else builds off in some way) we 
;;; can leverage the greater understanding a reader is likely to have. C inheritted a lot from Lisp (and modern languages 
;;; are still taking new inspiration from it) but almost no one has inheritted the *naming conventions* of Lisp.
;;; 
;;; The problem of having multiple versions of *basic* functions in common-lisp is actually the biggest problem in readability. 
;;; Thus far in my learning experience of common lisp it seems there's a vagary of stumbling blocks about how to write 
;;; Lisp created from this problem. This is my attempt to create a personal unified *basic* lisp naming conventions. 
;;; 
;;; Definition of basic: 
;;; For additional clarity, for now, I limit what I consider 'basic' to what might be considered an operator, keyword, or an 
;;; exceptionally common function in either lisp or a C style language.

;; ensure we use the proper set version. Basic (set) only works with values, (setf) seems it can set basically anything 
;; (including function definitions)
(setf (fdefinition '=) #'setf) 

;; there's like 5 ways to check equivalency, I want the standard c/java style for the general usecase
(setf (fdefinition '==) #'equal) 

;; ^ seems associated to exponents in the c family, but mostly the pow() function is used actually there. 
(setf (fdefinition 'pow) #'expt) 

;; fairly close alignment of naming and meaning
(setf (fdefinition 'global) #'defparameter)
(setf (fdefinition 'const-global) #'defconstant)


;;; Possibly insane aliases, especially ones that are not guaranteed to have a one-to-one meaning with c style operators or keywords
 
;; symbols created via (defvar) will only get "set" once by a defvar function call, subsequent calls to defvar for an existing symbol
;; will not do anything. However, it's not a constant, as it can still be changed by actual (set) type calls or (defparameter). 
;; Basically, I understand the usecase for defvar (safely initializing a variable only once without erroring), but I feel like the 
;; confusion it creates is mostly misguided and should be generally avoided in favor of the clarity provided by defparameter and 
;; defconstant (see new aliases above).
(setf (fdefinition 'init-global) #'defvar)


