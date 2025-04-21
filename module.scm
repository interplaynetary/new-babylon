;; module.scm
;; Main module file for New Babylon DSL that re-exports all components

(define-module (new-babylon module)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib let-on)
  #:use-module (goblins actor-lib cell)
  #:use-module (new-babylon)
  #:use-module (new-babylon geometry)
  #:use-module (new-babylon simulation)
  #:use-module (new-babylon dsl)
  #:use-module (new-babylon scheme-dsl)
  #:re-export
  ;; Core API
  (make-project
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
   import-data
   export-data)
  
  ;; Geometry
  (make-point
   make-polygon
   make-box
   make-cylinder
   point-distance
   contains-point?
   intersect?)
  
  ;; Simulation
  (make-structural-simulator
   make-social-simulator
   make-environmental-simulator
   make-temporal-simulator)
  
  ;; JSON-style DSL parsing
  (parse-dsl
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
   load-dsl-file)
   
  ;; Scheme-native DSL
  (define-project
   define-babylon-material
   define-babylon-structure
   define-babylon-space
   define-babylon-connection
   define-babylon-environment
   define-babylon-system
   define-babylon-evolution
   babylon-simulate
   babylon-visualize
   babylon-query
   reference
   run-babylon-model
   load-babylon-file
   current-project))

;; Provide a convenience function to run a complete New Babylon model from DSL source
(define (run-babylon-dsl dsl-source)
  "Parse and execute a New Babylon DSL model from source text."
  (let ((project (parse-dsl dsl-source)))
    (if project
        (begin
          (display "New Babylon project loaded: ")
          (display ($ project 'name))
          (newline)
          project)
        (begin
          (display "Failed to parse New Babylon DSL")
          (newline)
          #f))))

;; Export the convenience function
(export run-babylon-dsl) 