#lang racket
(provide
 defgeneric
 defmethod
 defsubtype
 generic-function-parameters
 generic-function-methods
 method-types)

(define order (make-hash))

(define-syntax-rule (defsubtype subtype supertype)
  (hash-set! order (car (list subtype)) (car (list supertype))))

(define-syntax-rule (defgeneric name parameters)
  (define name (make-generic-function 'name 'parameters)))

(define-syntax-rule (defmethod name ((argument predicate) ...) body ...)
  (let* ([types (list predicate ...)]
         [function (lambda (argument ...) body ...)]
         [methods (generic-function-methods name)]
         [method (make-method types function)])
    (set-generic-function-methods! name (update-methods methods method))))

(struct generic-function (name parameters [methods #:auto])
  #:mutable
  #:auto-value '()
  #:constructor-name make-generic-function
  #:property prop:procedure (lambda (self first . rest)
                              (let ([method (most-specific-method (generic-function-methods self) (append (list first) rest))])
                                (apply (method-function method) (append (list first) rest)))))

(struct method (types function)
  #:constructor-name make-method)

(define (applicable? predicates parameters)
  (if (and (eq? '() predicates) (eq? '() parameters))
      #t
      (if (eq? (length predicates) (length parameters))
          (if (with-handlers ([exn:fail:contract? (lambda (exn) #f)])
                (apply (car predicates) (list (car parameters))))
              (applicable? (cdr predicates) (cdr parameters))
              #f)
          #f)))

(define (subtype? subtype supertype)
  (let ([parent (hash-ref order subtype [lambda () #f])])
    (if (eq? parent #f)
        #f
        (if (eq? supertype parent)
            #t
            (subtype? parent supertype)))))

(define (more-specific? predicates1 predicates2)
  (let ([predicate1 (car predicates1)]
        [predicate2 (car predicates2)])
    (cond
      [(subtype? predicate2 predicate1) #f]
      [(subtype? predicate1 predicate2) #t]
      [(eq? (cdr predicates1) '()) #f]
      [else (more-specific? (cdr predicates1) (cdr predicates2))])))

(define (most-specific-method methods parameters)
  (let ([sorted-methods (sort methods (lambda (m1 m2) (more-specific? (method-types m1) (method-types m2))))])
    (find-most-specific sorted-methods parameters)))

(define (find-most-specific methods parameters)
  (if (eq? '() methods)
      (error "Method missing for arguments" parameters)
      (if (applicable? (method-types (car methods)) parameters)
          (car methods)
          (find-most-specific (cdr methods) parameters))))

(define (update-methods methods method)
  (cond
    [(eq? '() methods) (list method)]
    [(eq-predicates? (method-types (car methods)) (method-types method)) (append (list method) (cdr methods))]
    [else (append (list (car methods)) (update-methods (cdr methods) method))]))
 
(define (eq-predicates? predicates1 predicates2)
  (cond
    [(not (eq? (length predicates1) (length predicates2))) #f]
    [(= (length predicates1) 0) #t]
    [(eq? (car predicates1) (car predicates2)) (eq-predicates? (cdr predicates1) (cdr predicates2))]
    [else #f]))