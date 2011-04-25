#!/usr/bin/env mzscheme
#lang scheme
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
; Helper function to find the length of a list.
(define length
   (lambda (ls)
     (if (null? ls)
         0
         (+ (length (cdr ls)) 1)))
)
; Print the labels.
(define (write-labels program)
   (printf "Labels:~n")
   (printf "==================================================~n")
   (map (lambda (line) 
          (when (not (null? line))
            (when (= 3 (length line))
                (printf "~s: ~s~n" (car line) (cadr line))))) program)
   (printf "==================================================~n")
   (printf "(~n")
)

; Print each line of the program.
(define (write-program-by-line filename program)
   (printf "==================================================~n")
   (printf "~a: ~s~n" *run-file* filename)
   (printf "==================================================~n")
   (write-labels program)
   ; For each list element in program, print each element.
   (map (lambda (line) (printf "~s~n" line)) program)
   (printf ")~n")
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
      ; Call write-program-by-line(sbprogfile, program)
      (write-program-by-line sbprogfile program)))
)
; Execute the main function.
(main (vector->list (current-command-line-arguments)))


