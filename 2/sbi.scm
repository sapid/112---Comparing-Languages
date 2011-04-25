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
; Function: Exit and print the list provided.
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
; Print each line of the program.
(define (write-program-by-line filename program)
   (printf "==================================================~n")
   (printf "~a: ~s~n" *run-file* filename)
   (printf "==================================================~n")
   (printf "(~n")
   ; For each list element in program, print each element.
   (map (lambda (line) (printf "~s~n" line)) program)
   (printf ")~n")
   (printf "==================================================~n")
)

(define *symbol-table* (make-hash)) ; Symbol hash table
(define (symbol-get key)
        (hash-ref *symbol-table* key))
(define (symbol-put! key value)
        (hash-set! *symbol-table* key value))

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
(define l-hash (make-hash)) ; Label hash table

; Function: Walk through program and execute it.
(define (exec-program program)
   (printf "DEBUG: Stub: Executing the program.~n")
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
   (printf "Hashing labels:~n")
   (printf "==================================================~n")
   (map (lambda (line) 
          (when (not (null? line))
            (when (= 3 (length line))
                (printf "~a: ~s~n" (- (car line) 1) (cadr line))
                (printf "    ~s~n" (list-ref program (- (car line) 1)))
                (hash-set! l-hash (cadr line) (- (car line) 1 ))
                ))) program)
   (printf "==================================================~n")
   (printf "Dumping label table...~n")
   (map (lambda (el) (printf "~s~n" el))(hash->list l-hash))
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
        ;Initialize the symbol table with native symbols.
        ;(initialize-s-hash)
        ; DEBUG
        (printf "Symbol table initialized~n")
        (map (lambda (el) (printf "~s~n" el))(hash->list s-hash))
        (printf "Symbol table dump complete.~n")
        ; END DEBUG
        ; Call write-program-by-line(sbprogfile, program)
        ; Pretty much a debug function.
        (write-program-by-line sbprogfile program) 
        ; Fetch all the labels that occur in program
        (hash-labels program) 
        ; Execute the program.
        (exec-program program)
        (collect-garbage)
        ))
)
; Execute the program!
(main (vector->list (current-command-line-arguments)))


