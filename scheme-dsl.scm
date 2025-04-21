;; scheme-dsl.scm
;; Scheme-native DSL for New Babylon Architecture using S-expressions

(define-module (new-babylon scheme-dsl)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib let-on)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 keyword)
  #:use-module (srfi srfi-9)
  #:use-module (new-babylon)
  #:use-module (new-babylon geometry)
  #:export (with-project
            define-project
            define-material
            define-structure
            define-space
            define-connection
            define-environment
            define-system
            define-evolution
            simulate
            visualize
            query
            ->
            ref
            geometry
            current-project))

;; Current project reference (for use within a model)
(define current-project (make-parameter #f))

;; =========================================
;; Advanced DSL syntax via syntax-case
;; =========================================

;; Project context macro
(define-syntax with-project
  (syntax-rules ()
    ((_ project-expr body ...)
     (parameterize ((current-project project-expr))
       body ...))))

;; Enhanced reference syntax - shorter and more expressive
(define-syntax ->
  (syntax-rules ()
    ((_ type name)
     (ref type name))))

;; Core reference function
(define (ref type name)
  (if (or (symbol? type) (string? type))
      (let ((project (current-project)))
        (unless project
          (error "No current project: use with-project or define-project first"))
        ($ project 'get-element 
           (if (string? type) (string->symbol type) type)
           (if (symbol? name) (symbol->name name) name)))
      (error "Invalid reference type" type)))

;; Syntax for geometry creation
(define-syntax geometry
  (lambda (x)
    (syntax-case x ()
      ((_ type args ...)
       #'(make-geometry 'type args ...)))))

;; Unified geometry constructor
(define (make-geometry type . args)
  (case type
    ((point) (apply make-point args))
    ((polygon) (apply make-polygon args))
    ((box) (apply make-box args))
    ((cylinder) (apply make-cylinder args))
    (else (error "Unknown geometry type" type))))

;; =========================================
;; Cleaner DSL with keyword arguments
;; =========================================

;; Define a project with keywords
(define-syntax define-project
  (lambda (x)
    (syntax-case x ()
      ((_ name location base-date timespan units)
       #'(begin
           (let ((project (make-project name location base-date timespan units)))
             (current-project project)
             project)))
      ((_ name #:location location #:base-date base-date 
          #:timespan timespan #:units units)
       #'(begin
           (let ((project (make-project name location base-date timespan units)))
             (current-project project)
             project))))))

;; Define material with property list
(define-syntax define-material
  (lambda (x)
    (syntax-case x ()
      ((_ name property ...)
       #'(let ((project (current-project)))
           (unless project
             (error "No current project defined. Use define-project or with-project first."))
           (define-material* project name (list property ...)))))))

;; Helper function for define-material
(define (define-material* project name properties)
  (define-material project name properties))

;; Define structure with a cleaner syntax
(define-syntax define-structure
  (lambda (x)
    (syntax-case x (geometry material load-capacity connections)
      ((_ name clauses ...)
       (with-syntax (((processed-clauses ...)
                      (map (lambda (clause)
                             (syntax-case clause (geometry material load-capacity connections)
                               ((geometry type args ...)
                                #'(geometry-value (make-geometry 'type args ...)))
                               ((material material-ref)
                                #'(material-value material-ref))
                               ((load-capacity values ...)
                                #'(load-capacity-values (list values ...)))
                               ((connections conn-values ...)
                                #'(connection-values (list conn-values ...)))))
                           #'(clauses ...))))
         #'(process-structure-definition name processed-clauses ...))))))

;; Define space with a cleaner syntax
(define-syntax define-space
  (lambda (x)
    (syntax-case x (geometry function capacity social-attributes)
      ((_ name clauses ...)
       (with-syntax (((processed-clauses ...)
                      (map (lambda (clause)
                             (syntax-case clause (geometry function capacity social-attributes)
                               ((geometry type args ...)
                                #'(geometry-value (make-geometry 'type args ...)))
                               ((function func-value)
                                #'(function-value func-value))
                               ((capacity cap-value)
                                #'(capacity-value cap-value))
                               ((social-attributes attrs ...)
                                #'(social-attributes-values (list attrs ...)))))
                           #'(clauses ...))))
         #'(process-space-definition name processed-clauses ...))))))

;; Define connection with a cleaner syntax
(define-syntax define-connection
  (lambda (x)
    (syntax-case x (type elements points structure load-transfer)
      ((_ name clauses ...)
       (with-syntax (((processed-clauses ...)
                      (map (lambda (clause)
                             (syntax-case clause (type elements points structure load-transfer)
                               ((type type-value)
                                #'(type-value type-value))
                               ((elements elems ...)
                                #'(elements-values (list elems ...)))
                               ((points point-values ...)
                                #'(points-values (list point-values ...)))
                               ((structure struct-value)
                                #'(structure-value struct-value))
                               ((load-transfer transfer-values ...)
                                #'(transfer-values (list transfer-values ...)))))
                           #'(clauses ...))))
         #'(process-connection-definition name processed-clauses ...))))))

;; Define environment with a cleaner syntax
(define-syntax define-environment
  (lambda (x)
    (syntax-case x (geometry wind solar thermal water)
      ((_ name clauses ...)
       (with-syntax (((processed-clauses ...)
                      (map (lambda (clause)
                             (syntax-case clause (geometry wind solar thermal water)
                               ((geometry type args ...)
                                #'(geometry-value (make-geometry 'type args ...)))
                               ((wind props ...)
                                #'(wind-values (list props ...)))
                               ((solar props ...)
                                #'(solar-values (list props ...)))
                               ((thermal props ...)
                                #'(thermal-values (list props ...)))
                               ((water props ...)
                                #'(water-values (list props ...)))))
                           #'(clauses ...))))
         #'(process-environment-definition name processed-clauses ...))))))

;; Define system with a cleaner syntax
(define-syntax define-system
  (lambda (x)
    (syntax-case x (type capacity nodes connections)
      ((_ name clauses ...)
       (with-syntax (((processed-clauses ...)
                      (map (lambda (clause)
                             (syntax-case clause (type capacity nodes connections)
                               ((type type-value)
                                #'(type-value type-value))
                               ((capacity cap-value)
                                #'(capacity-value cap-value))
                               ((nodes node-values ...)
                                #'(nodes-values (list node-values ...)))
                               ((connections conn-values ...)
                                #'(connections-values (list conn-values ...)))))
                           #'(clauses ...))))
         #'(process-system-definition name processed-clauses ...))))))

;; Define evolution with a cleaner syntax
(define-syntax define-evolution
  (lambda (x)
    (syntax-case x (phase)
      ((_ name phase-clauses ...)
       (with-syntax (((processed-phases ...)
                      (map (lambda (phase-clause)
                             (syntax-case phase-clause (phase)
                               ((phase time props ...)
                                #'(list time (list props ...)))))
                           #'(phase-clauses ...))))
         #'(let ((project (current-project)))
             (unless project 
               (error "No current project defined. Use define-project or with-project first."))
             (define-evolution project name (list processed-phases ...))))))))

;; Define simulation with a cleaner syntax
(define-syntax simulate
  (lambda (x)
    (syntax-case x (target conditions analyze)
      ((_ name clauses ...)
       (with-syntax (((processed-clauses ...)
                      (map (lambda (clause)
                             (syntax-case clause (target conditions analyze)
                               ((target target-ref)
                                #'(target-value target-ref))
                               ((conditions cond-values ...)
                                #'(conditions-values (list cond-values ...)))
                               ((analyze analysis-types ...)
                                #'(analyze-values (list 'analysis-types ...)))))
                           #'(clauses ...))))
         #'(process-simulation-definition name processed-clauses ...))))))

;; Define visualization with a cleaner syntax
(define-syntax visualize
  (lambda (x)
    (syntax-case x (target mode property)
      ((_ name target-expr mode-expr property-expr)
       #'(let ((project (current-project)))
           (unless project 
             (error "No current project defined. Use define-project or with-project first."))
           (visualize* project name target-expr mode-expr property-expr)))
      ((_ name 
          (target target-ref)
          (mode viz-mode)
          (property viz-prop))
       #'(let ((project (current-project)))
           (unless project 
             (error "No current project defined. Use define-project or with-project first."))
           (visualize* project name target-ref viz-mode viz-prop))))))

;; Define query with a cleaner syntax
(define-syntax query
  (lambda (x)
    (syntax-case x (select from where)
      ((_ name select-expr from-expr where-expr)
       #'(let ((project (current-project)))
           (unless project 
             (error "No current project defined. Use define-project or with-project first."))
           (query* project name select-expr from-expr where-expr)))
      ((_ name 
          (select select-type)
          (from from-ref)
          (where condition))
       #'(let ((project (current-project)))
           (unless project 
             (error "No current project defined. Use define-project or with-project first."))
           (query* project name select-type from-ref condition))))))

;; =========================================
;; Helper procedures for processing syntax
;; =========================================

;; Process structure definition
(define (process-structure-definition name . clauses)
  (let ((project (current-project))
        (geometry-val #f)
        (material-val #f)
        (load-capacity-vals '())
        (connection-vals '()))
    
    (unless project
      (error "No current project defined. Use define-project or with-project first."))
    
    ;; Process the clauses
    (for-each (lambda (clause)
                (match clause
                  (('geometry-value val) (set! geometry-val val))
                  (('material-value val) (set! material-val val))
                  (('load-capacity-values vals) (set! load-capacity-vals vals))
                  (('connection-values vals) (set! connection-vals vals))))
              clauses)
    
    ;; Resolve material reference if needed
    (when (and (pair? material-val) (eq? (car material-val) '->))
      (set! material-val (apply -> (cdr material-val))))
    
    ;; Create the structure
    (define-structure project name geometry-val material-val load-capacity-vals connection-vals)))

;; Process space definition
(define (process-space-definition name . clauses)
  (let ((project (current-project))
        (geometry-val #f)
        (function-val #f)
        (capacity-val #f)
        (social-attributes-vals '()))
    
    (unless project
      (error "No current project defined. Use define-project or with-project first."))
    
    ;; Process the clauses
    (for-each (lambda (clause)
                (match clause
                  (('geometry-value val) (set! geometry-val val))
                  (('function-value val) (set! function-val val))
                  (('capacity-value val) (set! capacity-val val))
                  (('social-attributes-values vals) (set! social-attributes-vals vals))))
              clauses)
    
    ;; Create the space
    (define-space project name geometry-val function-val capacity-val social-attributes-vals)))

;; Process connection definition
(define (process-connection-definition name . clauses)
  (let ((project (current-project))
        (type-val #f)
        (elements-vals '())
        (points-vals '())
        (structure-val #f)
        (transfer-vals '()))
    
    (unless project
      (error "No current project defined. Use define-project or with-project first."))
    
    ;; Process the clauses
    (for-each (lambda (clause)
                (match clause
                  (('type-value val) (set! type-val val))
                  (('elements-values vals) (set! elements-vals vals))
                  (('points-values vals) (set! points-vals vals))
                  (('structure-value val) (set! structure-val val))
                  (('transfer-values vals) (set! transfer-vals vals))))
              clauses)
    
    ;; Resolve references
    (let ((resolved-elements
           (map (lambda (elem)
                  (if (and (pair? elem) (eq? (car elem) '->))
                      (apply -> (cdr elem))
                      elem))
                elements-vals))
          (resolved-structure 
           (if (and (pair? structure-val) (eq? (car structure-val) '->))
               (apply -> (cdr structure-val))
               structure-val)))
      
      ;; Create the connection
      (define-connection project name type-val resolved-elements points-vals 
                         resolved-structure transfer-vals))))

;; Process environment definition
(define (process-environment-definition name . clauses)
  (let ((project (current-project))
        (geometry-val #f)
        (wind-vals '())
        (solar-vals '())
        (thermal-vals '())
        (water-vals '()))
    
    (unless project
      (error "No current project defined. Use define-project or with-project first."))
    
    ;; Process the clauses
    (for-each (lambda (clause)
                (match clause
                  (('geometry-value val) (set! geometry-val val))
                  (('wind-values vals) (set! wind-vals vals))
                  (('solar-values vals) (set! solar-vals vals))
                  (('thermal-values vals) (set! thermal-vals vals))
                  (('water-values vals) (set! water-vals vals))))
              clauses)
    
    ;; Create the environment
    (define-environment project name geometry-val wind-vals solar-vals thermal-vals water-vals)))

;; Process system definition
(define (process-system-definition name . clauses)
  (let ((project (current-project))
        (type-val #f)
        (capacity-val #f)
        (nodes-vals '())
        (connections-vals '()))
    
    (unless project
      (error "No current project defined. Use define-project or with-project first."))
    
    ;; Process the clauses
    (for-each (lambda (clause)
                (match clause
                  (('type-value val) (set! type-val val))
                  (('capacity-value val) (set! capacity-val val))
                  (('nodes-values vals) (set! nodes-vals vals))
                  (('connections-values vals) (set! connections-vals vals))))
              clauses)
    
    ;; Create the system
    (define-system project name type-val capacity-val nodes-vals connections-vals)))

;; Process simulation definition
(define (process-simulation-definition name . clauses)
  (let ((project (current-project))
        (target-val #f)
        (conditions-vals '())
        (analyze-vals '()))
    
    (unless project
      (error "No current project defined. Use define-project or with-project first."))
    
    ;; Process the clauses
    (for-each (lambda (clause)
                (match clause
                  (('target-value val) (set! target-val val))
                  (('conditions-values vals) (set! conditions-vals vals))
                  (('analyze-values vals) (set! analyze-vals vals))))
              clauses)
    
    ;; Resolve target reference
    (let ((resolved-target 
           (if (and (pair? target-val) (eq? (car target-val) '->))
               (apply -> (cdr target-val))
               target-val)))
      
      ;; Create the simulation
      (simulate project name resolved-target conditions-vals analyze-vals))))

;; Helper function for define-visualization
(define (visualize* project name target mode property)
  ;; Resolve target reference if needed
  (let ((resolved-target 
         (if (and (pair? target) (eq? (car target) '->))
             (apply -> (cdr target))
             target)))
    
    ;; Create the visualization
    (visualize project name resolved-target mode property)))

;; Helper function for define-query
(define (query* project name select-type from where-condition)
  ;; Resolve from reference if needed
  (let ((resolved-from 
         (if (and (pair? from) (eq? (car from) '->))
             (apply -> (cdr from))
             from)))
    
    ;; Create the query
    (query project name select-type resolved-from where-condition)))

;; =========================================
;; Model evaluation and loading
;; =========================================

;; Convert symbol to string
(define (symbol->name sym)
  (symbol->string sym))

;; Load a Babylon model from a file
(define (load-babylon-file filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((result #f))
        (let ((expr (read port)))
          (if (eof-object? expr)
              result
              (let ((val (eval expr (current-module))))
                (loop val))))))))