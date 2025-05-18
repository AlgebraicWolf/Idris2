;; For profiling

(define blodwen-call-stack '())

(define-syntax blodwen-cost-centre
  (syntax-rules ()
    [(_ name body)
     (with-continuation-mark
       'blodwen-call-stack
       (cons name (blodwen-current-call-stack))
       body)]))

; To avoid losing information about functions in stack trace when
; there are calls in tail position, we make sure to create a new
; continuation frame for every cost centre.
(define (blodwen-current-call-stack)
  (continuation-marks-first
    (current-continuation-marks)
    'blodwen-call-stack))

; In Chez, there is no way to inspect the current continuation
; from another thread. To record call stacks, we need
; a routine that will keep globally accessible stack trace.
(define (blodwen-keep-stack-trace)
  ; The number of ticks specifies how often should the
  ; globally-accessible stack trace be updated.
  (define ticks 10000)
  (timer-interrupt-handler
    (lambda ()
      (set! blodwen-call-stack (blodwen-current-call-stack))
      (set-timer ticks)))
  (set-timer ticks))

(define blodwen-running (make-parameter #t))

; Main profiling routine launched in a separate thread.
; It should grab the current stack trace, and write it
; down in a file.
; It's worth investigating if there should be a (customised)
; hash map to reduce the disk write frequency.
(define (blodwen-profiler sleep-duration-ns)
  (define profile-filename "profile.folded")
  (define (display-trace trace output-port)
    ; Here, we omit the empty stack traces
    (if (pair? trace)
      (begin
        (fold-right (lambda (fn _)
                      (display fn output-port)
                      (display ";" output-port))
                    '()
                    trace)
        (display " 1\n" output-port))))
  (define (timed thunk)
    (let ((start (current-time)))
      (thunk)
      (time-difference (current-time) start)))
  (define sleep-duration
    (make-time 'time-duration sleep-duration-ns 0))
  (define (sampling-loop port)
    ; Write the current trace, measuring the writing time.
    ; Then sleep for however much is left.
    (let ((write-duration
            (timed
              (lambda () (display-trace blodwen-call-stack port)))))
      (sleep (time-difference sleep-duration write-duration)))
    (if (blodwen-running)
      (sampling-loop port)))
  (delete-file profile-filename)
  (lambda () (call-with-output-file profile-filename sampling-loop)))

; Run the computation in an environment suitable for profiling.
; This should set up the continuation marks for tracking the
; call stack, lauch the updater routine, lauch the profiling
; thread, etc.
(define-syntax blodwen-with-profile
  (syntax-rules ()
    [(_ sleep-duration body)
     (begin
       (let ((profiler-thread (fork-thread (blodwen-profiler sleep-duration))))
         (blodwen-keep-stack-trace)
         (with-continuation-mark
           'blodwen-call-stack
           '()
           body)
         (blodwen-running #f)
         (thread-join profiler-thread)))]))
