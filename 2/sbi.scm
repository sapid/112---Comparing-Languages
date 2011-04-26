#!/usr/bin/env mzscheme -qr
;#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket-5.1/bin/mzscheme -qr
;; AUTHORS
;;   Will Crawford <wacrawfo@ucsc.edu>
;;   Ben Ross     <bpross@ucsc.edu>
;;   Based on code by Wesley Mackey
;;
;; NAME
;;   sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;   sbi.scm filename.sbir
;;
;; DESCRIPTION
;;   The file mentioned in argv[1] is read and assumed to be an
;;   SBIR program, which is the executed.  Currently it is only
;;   printed.
;;
;; ========================
;;    Mackey's functions
;; ========================
; Define *stderr*
(define *stderr* (current-error-port))
; Function: Find the basename of the filename provided.
(define *run-file*
   (let-values
      (((dirpath basepath root?)
         (split-path (find-system-path 'run-file))))
      (path->string basepath))
)
; Function: Exit and print the error provided.
(define (die list)
   (for-each (lambda (item) (display item *stderr*)) list)
   (newline *stderr*)
   (exit 1)
)
; Function: Print usage information and die.
(define (usage-exit)
   (die `("Usage: " ,*run-file* " filename"))
)
; Function: Read in the file.
(define (readlist-from-inputfile filename)
   (let ((inputfile (open-input-file filename)))
       (if (not (input-port? inputfile))
          (die `(,*run-file* ": " ,filename ": open failed"))
          (let ((program (read inputfile)))
              (close-input-port inputfile)
                   program)))
)
(define *symbol-table* (make-hash)) ; Symbol hash table
(define (symbol-get key) ; Example: (symbol-get 'log10)
        (hash-ref *symbol-table* key))
(define (symbol-put! key value)
        (hash-set! *symbol-table* key value))
; Initialize the symbol table.
(for-each
    (lambda (pair)
            (symbol-put! (car pair) (cadr pair)))
    `(
        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (sqrt    ,sqrt)
     ))
;; ======================= 
;;      Our functions
;; ========================
(define n-hash (make-hash)) ; Native function translation table

(define l-hash (make-hash)) ; Label hash table

(define (h_eval expr)
  (printf "DEBUG: Evaluating...~n")
  (printf "       ~s~n" expr)
  (cond
    ((string? expr)
      (printf "       is a string~n")
      expr)
    ((number? expr)
      (printf "       is a number~n")
      expr)
    ((list? expr)
      (printf "       is a list~n")
      (if (> (length expr) 1)
        (h_eval (cadr expr))
        (printf "       test~n"))
      (if (not (null? (cdr expr)))
        (h_eval (cdr expr))
        expr))
    (else 
       (printf "       hit else~n")))

)

(define (sb_print expr)
  (printf "DEBUG: Stub: Printing an expression.~n")
)

(define (sb_dim expr)
  (printf "DEBUG: Stub: Declaring an array.~n")
)

(define (sb_let mem expr)
  (printf "DEBUG: Stub: Declaring a variable.~n")
)

(define (sb_input expr)
  (printf "DEBUG: Stub: Read in numbers.~n")
)

(define (sb_if expr label)
  (printf "DEBUG: Stub: Conditional goto.~n")
)

(define (sb_goto label)
  (printf "DEBUG: Stub: Goto.~n")
)
(for-each
  (lambda (pair)
          (hash-set! n-hash (car pair) (cadr pair)))
  `(      ; This hash table translates SB functions to our functions.
      (print ,sb_print)
      (dim   ,sb_dim)
      (let   ,sb_let)
      (input ,sb_input)
      (if    ,sb_if)
      (goto  ,sb_goto)
   ))
; Function: Execute a line passed by eval-line.
(define (exec-line instr program line-nr)
  (when (not (hash-has-key? n-hash (car instr))) ; Die if invalid.
        (die "~s is not a valid instruction." (car instr)))
  (cond
        ((= (car instr) goto)
         (eval-line program (cdr instr)))
        ((= (car instr) 'if)
         (if (#f );h_eval (cdr (car instr)))
           (eval-line program (cdr (cdr instr)))
           (eval-line program (+ line-nr 1))))
        (else
          ;(,(hash-ref n-hash (car instr)) (cdr instr))
          (eval-line program (+ line-nr 1))))
)

; Function: Walk through program and execute it. 
; This function takes a line number to execute.
(define (eval-line program line-nr)
   (printf "DEBUG: Executing line.~n")
   (printf "       ~s~n" (list-ref program line-nr))
   (let((line (list-ref program line-nr)))
   (cond
     ((= (length line) 3)
      (set! line (cddr line))
      (printf "DEBUG: Line had 3 elements.~n        ~s~n" line)
      (printf "CDR: ~s~n"(car line))
      ;(exec-line instr program line-nr))
      (eval-line program (+1 line-nr 1)))
     ((= (length line) 2)
      (set! line (cdr line))
      (let((instr (car line)))
      (printf "DEBUG: Line had 2 elements.~n       ~s~n" line)
      (printf "       ~s~n" instr))
      (eval-line program (+ line-nr 1)))
     (else 
       (eval-line program (+ line-nr 1)))
   ))
)
; Function: Find the length of a list.
(define length
   (lambda (ls)
     (if (null? ls)
         0
         (+ (length (cdr ls)) 1)))
)
; Push the labels into the hash table.
(define (hash-labels program)
;   (printf "Hashing labels:~n")
;   (printf "==================================================~n")
   (map (lambda (line) 
          (when (not (null? line))
            (when (= 3 (length line))
;                (printf "~a: ~s~n" (- (car line) 1) (cadr line))
;                (printf "    ~s~n" (list-ref program (- (car line) 1)))
                (hash-set! l-hash (cadr line) (- (car line) 1 ))
                ))) program)
;   (printf "==================================================~n")
;   (printf "Dumping label table...~n")
;   (map (lambda (el) (printf "~s~n" el))(hash->list l-hash))
)

; This is the main function that gets called.
(define (main arglist)
   (if (or (null? arglist) (not (null? (cdr arglist))))
      ; Case 1. Number of args != 1.
      (usage-exit) 
      ; Case 2. Set sbprogfile = filename from argument
      (let* ((sbprogfile (car arglist))
            ; Set program = The list of commands in the inputfile.
            (program (readlist-from-inputfile sbprogfile))) 
        ; Fetch all the labels that occur in program
        (hash-labels program) 
        ; Execute the program.
        (eval-line program 0)
        ))
)

; Execute the program!
(main (vector->list (current-command-line-arguments)))
