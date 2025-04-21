;; dsl.scm
;; DSL parser for New Babylon Architecture DSL

(define-module (new-babylon dsl)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib let-on)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (new-babylon)
  #:use-module (new-babylon geometry)
  #:export (parse-dsl
            parse-project
            parse-material
            parse-structure
            parse-space
            parse-connection
            parse-environment
            parse-system
            parse-evolution
            parse-simulate
            parse-visualize
            parse-query
            load-dsl-file))

;; =========================================
;; Parsing utilities
;; =========================================

;; Define the basic token types in the DSL
(define-record-type <token>
  (make-token type value line)
  token?
  (type token-type)
  (value token-value)
  (line token-line))

;; Tokenize the input DSL text
(define (tokenize input)
  (let ((lines (string-split input #\newline))
        (tokens '())
        (line-num 1))
    ;; Process each line
    (for-each
     (lambda (line)
       ;; Skip comments and empty lines
       (unless (or (string-null? line)
                  (string-match "^\\s*//.*$" line))
         ;; Trim whitespace
         (let ((trimmed (string-trim-both line)))
           ;; Match different token patterns
           (cond
            ;; Block start
            ((string-match "\\{" trimmed)
             (set! tokens (cons (make-token 'block-start "{" line-num) tokens)))
            ;; Block end
            ((string-match "\\}" trimmed)
             (set! tokens (cons (make-token 'block-end "}" line-num) tokens)))
            ;; Array start
            ((string-match "\\[" trimmed)
             (set! tokens (cons (make-token 'array-start "[" line-num) tokens)))
            ;; Array end
            ((string-match "\\]" trimmed)
             (set! tokens (cons (make-token 'array-end "]" line-num) tokens)))
            ;; Property assignment
            ((string-match "^([a-zA-Z0-9_-]+)\\s*:\\s*(.+)$" trimmed)
             => (lambda (m)
                  (let ((prop-name (match:substring m 1))
                        (prop-value (match:substring m 2)))
                    (set! tokens (cons (make-token 'property prop-name line-num) tokens))
                    (set! tokens (cons (make-token 'colon ":" line-num) tokens))
                    ;; Parse the value appropriately
                    (cond
                     ;; String
                     ((string-match "^\"([^\"]*)\"" prop-value)
                      => (lambda (m2)
                           (set! tokens (cons (make-token 'string (match:substring m2 1) line-num) tokens))))
                     ;; Number
                     ((string-match "^[0-9]+(\\.[0-9]+)?([a-zA-Z]+)?$" prop-value)
                      (set! tokens (cons (make-token 'number prop-value line-num) tokens)))
                     ;; Reference
                     ((string-match "^@\"([^\"]*)\"$" prop-value)
                      => (lambda (m2)
                           (set! tokens (cons (make-token 'reference (match:substring m2 1) line-num) tokens))))
                     ;; Block start or array start (handled elsewhere)
                     ((or (string-match "^\\{" prop-value)
                          (string-match "^\\[" prop-value))
                      'skip) ; Skip, as brackets will be handled separately
                     ;; Symbol/keyword
                     (else
                      (set! tokens (cons (make-token 'symbol prop-value line-num) tokens))))))))
            ;; Element definition
            ((string-match "^([A-Z][a-zA-Z0-9]*)\\s+\"([^\"]*)\"\\s*\\{?$" trimmed)
             => (lambda (m)
                  (let ((elem-type (match:substring m 1))
                        (elem-name (match:substring m 2)))
                    (set! tokens (cons (make-token 'element-type elem-type line-num) tokens))
                    (set! tokens (cons (make-token 'element-name elem-name line-num) tokens))
                    ;; Handle if there's a bracket on the same line
                    (when (string-match "\\{$" trimmed)
                      (set! tokens (cons (make-token 'block-start "{" line-num) tokens))))))
            ;; Command
            ((string-match "^([a-zA-Z]+)\\s+\"([^\"]*)\"\\s*\\{?$" trimmed)
             => (lambda (m)
                  (let ((cmd-type (match:substring m 1))
                        (cmd-name (match:substring m 2)))
                    (set! tokens (cons (make-token 'command cmd-type line-num) tokens))
                    (set! tokens (cons (make-token 'command-name cmd-name line-num) tokens))
                    ;; Handle if there's a bracket on the same line
                    (when (string-match "\\{$" trimmed)
                      (set! tokens (cons (make-token 'block-start "{" line-num) tokens))))))
            ;; Other keywords, handles cases not captured above
            (else
             (set! tokens (cons (make-token 'unknown trimmed line-num) tokens))))))
       (set! line-num (1+ line-num)))
     lines)
    ;; Return tokens in the right order
    (reverse tokens)))

;; Parse a property value from a token
(define (parse-value token tokens)
  (case (token-type token)
    ((string) (token-value token))
    ((number) 
     (let ((val-str (token-value token)))
       (cond
        ;; With units
        ((string-match "([0-9.]+)([a-zA-Z]+)" val-str)
         => (lambda (m)
              (cons (string->number (match:substring m 1))
                    (match:substring m 2))))
        ;; Plain number
        (else (string->number val-str)))))
    ((reference) 
     (cons 'reference (token-value token)))
    ((symbol) (string->symbol (token-value token)))
    (else (token-value token))))

;; Parse a block of tokens into a property list
(define (parse-block tokens)
  (let loop ((toks tokens)
             (properties '()))
    (if (null? toks)
        properties
        (let ((token (car toks)))
          (case (token-type token)
            ((property)
             (let* ((prop-name (token-value token))
                    (rest (cddr toks)) ; Skip the colon token
                    (val-token (car rest))
                    (val-type (token-type val-token)))
               (case val-type
                 ((block-start)
                  (let-values (((block-contents remaining) (extract-block (cdr rest))))
                    (loop remaining
                          (cons (cons (string->symbol prop-name)
                                      (parse-block block-contents))
                                properties))))
                 ((array-start)
                  (let-values (((array-contents remaining) (extract-array (cdr rest))))
                    (loop remaining
                          (cons (cons (string->symbol prop-name)
                                      array-contents)
                                properties))))
                 (else
                  (loop (cdr rest)
                        (cons (cons (string->symbol prop-name)
                                    (parse-value val-token rest))
                              properties))))))
            (else (loop (cdr toks) properties)))))))

;; Extract tokens for a complete block (between { and })
(define (extract-block tokens)
  (let loop ((toks tokens)
             (block-tokens '())
             (nesting 0))
    (if (null? toks)
        (values (reverse block-tokens) '())
        (let ((token (car toks)))
          (case (token-type token)
            ((block-start)
             (loop (cdr toks)
                   (cons token block-tokens)
                   (1+ nesting)))
            ((block-end)
             (if (zero? nesting)
                 (values (reverse block-tokens) (cdr toks))
                 (loop (cdr toks)
                       (cons token block-tokens)
                       (1- nesting))))
            (else
             (loop (cdr toks)
                   (cons token block-tokens)
                   nesting)))))))

;; Extract tokens for an array (between [ and ])
(define (extract-array tokens)
  (let loop ((toks tokens)
             (array-items '())
             (current-item '())
             (nesting 0))
    (if (null? toks)
        (values (reverse array-items) '())
        (let ((token (car toks)))
          (case (token-type token)
            ((array-start)
             (loop (cdr toks)
                   array-items
                   (cons token current-item)
                   (1+ nesting)))
            ((array-end)
             (if (zero? nesting)
                 (let ((final-items (if (null? current-item)
                                       array-items
                                       (cons (parse-value (car current-item) current-item)
                                             array-items))))
                   (values (reverse final-items) (cdr toks)))
                 (loop (cdr toks)
                       array-items
                       (cons token current-item)
                       (1- nesting))))
            ((comma)
             (if (zero? nesting)
                 (loop (cdr toks)
                       (cons (parse-value (car current-item) current-item) array-items)
                       '()
                       nesting)
                 (loop (cdr toks)
                       array-items
                       (cons token current-item)
                       nesting)))
            (else
             (loop (cdr toks)
                   array-items
                   (cons token current-item)
                   nesting)))))))

;; =========================================
;; DSL parsing functions
;; =========================================

;; Parse a Project definition
(define (parse-project tokens project-name)
  (let ((properties (parse-block tokens)))
    (let ((location (assoc-ref properties 'location))
          (base-date (assoc-ref properties 'baseDate))
          (timespan (assoc-ref properties 'timeSpan))
          (units (assoc-ref properties 'units)))
      ;; Create the project
      (make-project project-name location base-date timespan units))))

;; Parse a Material definition
(define (parse-material tokens project material-name)
  (let ((properties (parse-block tokens)))
    ;; Create the material
    (define-material project material-name properties)))

;; Parse a Structure definition
(define (parse-structure tokens project structure-name)
  (let ((properties (parse-block tokens)))
    (let ((geometry-props (assoc-ref properties 'geometry))
          (material-ref (assoc-ref properties 'material))
          (load-capacity (assoc-ref properties 'loadCapacity))
          (connections (assoc-ref properties 'connections)))
      ;; Process geometry
      (let ((geometry-type (car geometry-props))
            (geometry-params (cdr geometry-props)))
        ;; Resolve material reference
        (let ((material (if (and (pair? material-ref)
                                (eq? (car material-ref) 'reference))
                           ($ project 'get-element 'material (cdr material-ref))
                           material-ref)))
          ;; Create the geometry
          (let ((geometry (create-geometry geometry-type geometry-params)))
            ;; Create the structure
            (define-structure project structure-name geometry material load-capacity connections)))))))

;; Parse a Space definition
(define (parse-space tokens project space-name)
  (let ((properties (parse-block tokens)))
    (let ((geometry-props (assoc-ref properties 'geometry))
          (function (assoc-ref properties 'function))
          (capacity (assoc-ref properties 'capacity))
          (social-attrs (assoc-ref properties 'socialAttributes)))
      ;; Process geometry
      (let ((geometry-type (car geometry-props))
            (geometry-params (cdr geometry-props)))
        ;; Create the geometry
        (let ((geometry (create-geometry geometry-type geometry-params)))
          ;; Create the space
          (define-space project space-name geometry function capacity social-attrs))))))

;; Parse a Connection definition
(define (parse-connection tokens project connection-name)
  (let ((properties (parse-block tokens)))
    (let ((type (assoc-ref properties 'type))
          (elements (assoc-ref properties 'elements))
          (points (assoc-ref properties 'points))
          (structure (assoc-ref properties 'structure))
          (load-transfer (assoc-ref properties 'loadTransfer)))
      ;; Resolve element references
      (let ((resolved-elements 
             (map (lambda (elem-ref)
                    (if (and (pair? elem-ref)
                           (eq? (car elem-ref) 'reference))
                        ($ project 'get-element 'structure (cdr elem-ref))
                        elem-ref))
                  elements)))
        ;; Resolve structure reference
        (let ((resolved-structure 
               (if (and (pair? structure)
                      (eq? (car structure) 'reference))
                   ($ project 'get-element 'structure (cdr structure))
                   structure)))
          ;; Create the connection
          (define-connection project connection-name type 
                            resolved-elements points 
                            resolved-structure load-transfer))))))

;; Parse an Environment definition
(define (parse-environment tokens project environment-name)
  (let ((properties (parse-block tokens)))
    (let ((geometry-props (assoc-ref properties 'geometry))
          (wind (assoc-ref properties 'wind))
          (solar (assoc-ref properties 'solar))
          (thermal (assoc-ref properties 'thermal))
          (water (assoc-ref properties 'water)))
      ;; Process geometry
      (let ((geometry-type (car geometry-props))
            (geometry-params (cdr geometry-props)))
        ;; Create the geometry
        (let ((geometry (create-geometry geometry-type geometry-params)))
          ;; Create the environment
          (define-environment project environment-name geometry wind solar thermal water))))))

;; Parse a System definition
(define (parse-system tokens project system-name)
  (let ((properties (parse-block tokens)))
    (let ((type (assoc-ref properties 'type))
          (capacity (assoc-ref properties 'capacity))
          (nodes (assoc-ref properties 'nodes))
          (connections (assoc-ref properties 'connections)))
      ;; Create the system
      (define-system project system-name type capacity nodes connections))))

;; Parse an Evolution definition
(define (parse-evolution tokens project evolution-name)
  (let ((properties (parse-block tokens)))
    (let ((phases (map (lambda (phase-prop)
                        (let ((time (car phase-prop))
                              (actions (cdr phase-prop)))
                          (list time actions)))
                      (filter (lambda (prop)
                               (string-match "^phase" (symbol->string (car prop))))
                              properties))))
      ;; Create the evolution
      (define-evolution project evolution-name phases))))

;; Parse a Simulate command
(define (parse-simulate tokens project simulation-name)
  (let ((properties (parse-block tokens)))
    (let ((target-ref (assoc-ref properties 'target))
          (conditions (assoc-ref properties 'conditions))
          (analyze (assoc-ref properties 'analyze)))
      ;; Resolve target reference
      (let ((target (if (and (pair? target-ref)
                           (eq? (car target-ref) 'reference))
                       ($ project 'get-element 'structure (cdr target-ref))
                       target-ref)))
        ;; Create the simulation
        (simulate project simulation-name target conditions analyze)))))

;; Parse a Visualize command
(define (parse-visualize tokens project visualization-name)
  (let ((properties (parse-block tokens)))
    (let ((target-ref (assoc-ref properties 'target))
          (mode (assoc-ref properties 'mode))
          (property (assoc-ref properties 'property)))
      ;; Resolve target reference
      (let ((target (if (and (pair? target-ref)
                           (eq? (car target-ref) 'reference))
                       ($ project 'get-element 'structure (cdr target-ref))
                       target-ref)))
        ;; Create the visualization
        (visualize project visualization-name target mode property)))))

;; Parse a Query command
(define (parse-query tokens project query-name)
  (let ((properties (parse-block tokens)))
    (let ((select-type (assoc-ref properties 'select))
          (from-ref (assoc-ref properties 'from))
          (where-condition (assoc-ref properties 'where)))
      ;; Resolve from reference
      (let ((from-element (if (and (pair? from-ref)
                                 (eq? (car from-ref) 'reference))
                             ($ project 'get-element 'structure (cdr from-ref))
                             from-ref)))
        ;; Create the query
        (query project query-name select-type from-element where-condition)))))

;; Helper function to create geometry objects
(define (create-geometry type params)
  (case type
    ((Point) 
     (let ((x (assoc-ref params 'x))
           (y (assoc-ref params 'y))
           (z (assoc-ref params 'z)))
       (make-point x y z)))
    ((Polygon)
     (let ((points (assoc-ref params 'points))
           (height (assoc-ref params 'height)))
       (make-polygon points height)))
    ((Box)
     (let ((origin (assoc-ref params 'origin))
           (dimensions (assoc-ref params 'dimensions)))
       (make-box origin dimensions)))
    ((Cylinder)
     (let ((center (assoc-ref params 'center))
           (radius (assoc-ref params 'radius))
           (height (assoc-ref params 'height)))
       (make-cylinder center radius height)))
    (else
     (error "Unknown geometry type:" type))))

;; Parse the entire DSL
(define (parse-dsl input)
  (let ((tokens (tokenize input))
        (project #f))
    ;; Process tokens sequentially
    (let loop ((toks tokens))
      (if (null? toks)
          project
          (let ((token (car toks)))
            (case (token-type token)
              ((element-type)
               (let* ((elem-type (token-value token))
                      (name-token (cadr toks))
                      (elem-name (token-value name-token))
                      (remaining (cddr toks)))
                 ;; Handle block after name
                 (let-values (((block-contents rest-tokens) 
                              (extract-block (if (eq? (token-type (car remaining)) 'block-start)
                                              (cdr remaining)
                                              remaining))))
                   ;; Parse appropriate element based on type
                   (case (string->symbol elem-type)
                     ((Project) 
                      (set! project (parse-project block-contents elem-name))
                      (loop rest-tokens))
                     ((Material)
                      (if project
                          (begin
                            (parse-material block-contents project elem-name)
                            (loop rest-tokens))
                          (error "Must define a Project before other elements")))
                     ((Structure)
                      (if project
                          (begin
                            (parse-structure block-contents project elem-name)
                            (loop rest-tokens))
                          (error "Must define a Project before other elements")))
                     ((Space)
                      (if project
                          (begin
                            (parse-space block-contents project elem-name)
                            (loop rest-tokens))
                          (error "Must define a Project before other elements")))
                     ((Connection)
                      (if project
                          (begin
                            (parse-connection block-contents project elem-name)
                            (loop rest-tokens))
                          (error "Must define a Project before other elements")))
                     ((Environment)
                      (if project
                          (begin
                            (parse-environment block-contents project elem-name)
                            (loop rest-tokens))
                          (error "Must define a Project before other elements")))
                     ((System)
                      (if project
                          (begin
                            (parse-system block-contents project elem-name)
                            (loop rest-tokens))
                          (error "Must define a Project before other elements")))
                     ((Evolution)
                      (if project
                          (begin
                            (parse-evolution block-contents project elem-name)
                            (loop rest-tokens))
                          (error "Must define a Project before other elements")))
                     (else (error "Unknown element type:" elem-type))))))
              
              ((command)
               (let* ((cmd-type (token-value token))
                      (name-token (cadr toks))
                      (cmd-name (token-value name-token))
                      (remaining (cddr toks)))
                 ;; Handle block after name
                 (let-values (((block-contents rest-tokens) 
                              (extract-block (if (eq? (token-type (car remaining)) 'block-start)
                                              (cdr remaining)
                                              remaining))))
                   ;; Parse appropriate command
                   (case (string->symbol cmd-type)
                     ((Simulate)
                      (if project
                          (begin
                            (parse-simulate block-contents project cmd-name)
                            (loop rest-tokens))
                          (error "Must define a Project before other elements")))
                     ((Visualize)
                      (if project
                          (begin
                            (parse-visualize block-contents project cmd-name)
                            (loop rest-tokens))
                          (error "Must define a Project before other elements")))
                     ((Query)
                      (if project
                          (begin
                            (parse-query block-contents project cmd-name)
                            (loop rest-tokens))
                          (error "Must define a Project before other elements")))
                     (else (error "Unknown command type:" cmd-type))))))

              (else (loop (cdr toks)))))))))

;; Load DSL from a file
(define (load-dsl-file filename)
  (let ((port (open-input-file filename))
        (content ""))
    (let loop ((line (read-line port)))
      (if (eof-object? line)
          (begin
            (close-port port)
            (parse-dsl content))
          (begin
            (set! content (string-append content line "\n"))
            (loop (read-line port)))))))) 