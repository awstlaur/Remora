#lang racket

(require rackunit
         redex)

(define-language Arrays
  (expr (expr expr ...)
        fun
        arr
        base
        var)
  
  ; functions may be builtin or user-defined
  (fun (λ [(var num) ...] expr)
       op)
  
  ; syntax for writing arrays
  ; also include nested vector representation?
  (arr (A (num ...) (expr ...))) ; flat representation, shape and values
  
  (arr/v (A (num ...) (elt ...))) ; pseudo-value form -- no app forms inside
  (elt arr/v base)
  (arr/b (A (num ...) (base ...))) ; value form -- only base data inside
  
  ; builtin operators
  (op + - * /
      reduce/r)
  
  ; variables
  (var variable-not-otherwise-mentioned)
  
  ; base data types: numbers and booleans
  (base num bool)
  (num number)
  (bool #t #f)
  
  ; value forms
  (val (λ [(var num) ...] expr)
       op
       arr/b)
  
  ; evaluation contexts
  (E hole
     (E expr ...)
     (val expr ... E expr ...)
     ;(A (E expr ...) (expr ...))
     ;(A (num ... E expr ...) (expr ...))
     (A (num ...) (E expr ...))
     (A (num ...) (val ... E expr ...))))


(define ->Array
  (reduction-relation
   Arrays
   #:domain expr
   [--> (in-hole E ((reduce/r fun) arr/v))
        (in-hole E (chain-apply/r fun (arr/v_cell ...)))
        (where (arr/v_cell ...) (cells/rank -1 arr/v))
        reduce/r]
   [--> (in-hole E (op arr ...))
        (in-hole E (apply-op op (arr ...)))
        (side-condition (equal? (term (fun-rank op))
                                (term  ((rank arr) ...))))
        op]
   [--> (in-hole E ((λ [(var num) ...] expr) arr/b ...))
        (in-hole E (subst [(var arr/b) ...] expr))
        (side-condition (term (all ((at-rank? num arr/b) ...))))
        apply]
   [--> (in-hole E (fun arr/b ...))
        (in-hole E (array-map fun (arr/b ...)))
        (where (num_r ...) (fun-rank fun))
        ; arrays must all be overranked and by the same (nonzero) amount
        (side-condition (term (all ((overrank? num_r arr/b) ...))))
        (side-condition (term (same-overrank? [(num_r arr/b) ...])))
        (side-condition (< 0 (length (term (arr/b ...)))))
        map]
   [--> (in-hole E (fun arr/b ...))
        (in-hole E (fun arr/b_lifted ...))
        (where (natural_r ...) (fun-rank fun))
        (where (arr/b_lifted ...) (frame-lift ([natural_r arr/b] ...)))
        ; arrays must be overranked by different amounts
        (side-condition (not (term (same-overrank? [(natural_r arr/b) ...]))))
        (side-condition (< 0 (length (term (arr/b ...)))))
        lift]
   [--> (in-hole E (fun arr/b ...))
        (in-hole E (fun_natrank arr/b ...))
        (side-condition (not (andmap exact-nonnegative-integer?
                                     (term (fun-rank fun)))))
        (where fun_natrank (naturalize-rank fun arr/b ...))
        naturalize]
   [--> (in-hole E (A (num_f ...) (arr/v ...)))
        (in-hole E (A (num_f ... num_c ...) any_v))
        (where any_v ,(foldr append '() (term ((value arr/v) ...))))
        (where (num_c ...) (shape-of-all arr/v ...))
        (where ((A (num ...) (base ...)) ...) (arr/v ...))
        collapse]))




;; macro to convert number to rank-0 array
(define-metafunction Arrays
  scalar : num -> arr
  [(scalar num) (A () (num))])


;; apply a function to a chain of arrays, i.e.
;; arr_0 `fun` arr_1 `fun` arr_2 `fun` arr_3 `fun` ... `fun` arr_n
(define-metafunction Arrays
  chain-apply/r : fun (arr ...) -> expr
  [(chain-apply/r fun ()) (id-element/r fun)]
  [(chain-apply/r fun (arr)) arr]
  [(chain-apply/r fun (arr_0 arr_1 ...))
   (fun arr_0 (chain-apply/r fun (arr_1 ...)))])
#;(define-metafunction Arrays
  chain-apply/l : fun arr ... -> expr
  [(chain-apply/l fun ()) (id-element/l fun)]
  [(chain-apply/l fun (arr)) arr]
  [(chain-apply/l fun (arr_0 ... arr_1))
   (fun (chain-apply/r fun (arr_0 ...)) arr_1)])

;; get the neutral/identity element of a function
(define-metafunction Arrays
  id-element/r : fun -> arr
  [(id-element/r +) (scalar 0)]
  [(id-element/r -) (scalar 0)]
  [(id-element/r *) (scalar 1)]
  [(id-element/r /) (scalar 1)])
#;(define-metafunction Arrays
  id-element/l : fun -> arr
  [(id-element/l +) (scalar 0)]
  [(id-element/l *) (scalar 1)])

;; rewrite a function to eliminate negative/infinite arg ranks
(define-metafunction Arrays
  naturalize-rank : fun arr ... -> fun
  [(naturalize-rank (λ [(var num) ...] expr) arr ...)
   (λ [(var natural) ...] expr)
   (where (natural ...) ((natural-cell-rank num arr) ...))])
(define-metafunction Arrays
  natural-cell-rank : num arr -> num
  [(natural-cell-rank natural arr) natural]
  [(natural-cell-rank +inf.0 arr) (rank arr)]
  ; TODO: some of this code is duplicated elsewhere -- clean that up
  [(natural-cell-rank num_neg arr) ,(+ (term (rank arr)) (term num_neg))])

;; get the shape of all given arrays, or return #f if not all are the same
(define-metafunction Arrays
  shape-of-all : arr ... -> (num ...) ∨ #f
  [(shape-of-all arr)
   (shape arr)]
  [(shape-of-all arr_0 arr_1 ...)
   (shape arr_0)
   (side-condition (equal? (term (shape arr_0))
                          (term (shape-of-all arr_1 ...))))]
  [(shape-of-all arr ...) #f])

;; map a function over the cells of its arguments
;; only use after all arrays have been lifted into same frame
(define-metafunction Arrays
  array-map : fun (arr ...) -> arr
  ;[(array-map fun ())]
  [(array-map fun (arr_arg ...))
   ; break array into cells, construct array whose value is the computations
   ; from the function mapping
   
   (A (num_frame ...)
      (cell-apply fun ((arr_cell ...) ...))
      #;,(apply (λ (c) (map (λ (x) (cons (term fun) x))))
              (term ((fun arr_cell ...) ...))))
   
   (where (num_funrank ...) (fun-rank fun))
   (where ((num_cellshape ...) ...)
          ((take-right/m (shape arr_arg) num_funrank) ...))
   (where ((arr_cell ...) ...) ((cells/shape (num_cellshape ...) arr_arg) ...))
   (where (num_frame ...) ,(drop-right (first (term ((shape arr_arg) ...)))
                                       (first (term (num_funrank ...)))))])
  
;; set up function application for each cell group (1st cells, 2nd cells, etc.)
(define-metafunction Arrays
  cell-apply : fun ((arr ...) ...) -> ((fun arr ...) ...)
  [(cell-apply fun ()) ()]
  [(cell-apply fun (() ...)) ()]
  [(cell-apply fun ((arr ...) ...))
   ,(cons (cons (term fun) (map first (term ((arr ...) ...))))
         (term (cell-apply fun ,(map rest (term ((arr ...) ...))))))])

;; apply a builtin operator
(define-metafunction Arrays
  apply-op : op (arr ...) -> arr
  [(apply-op + ((A () (num_1)) (A () (num_2))))
   (A () (,(+ (term num_1) (term num_2))))])

;; extract or look up ranks of a function
(define-metafunction Arrays
  fun-rank : fun -> (num ...)
  [(fun-rank +) (0 0)]
  [(fun-rank -) (0 0)]
  [(fun-rank *) (0 0)]
  [(fun-rank /) (0 0)]
  [(fun-rank (λ [(var num) ...] expr)) (num ...)]
  [(fun-rank (reduce/r expr)) +inf.0])

;; capture-avoiding substitution
(define-metafunction Arrays
  subst : [(var expr) ...] expr -> expr
  [(subst [(var expr) ...] base) base]
  [(subst [(var expr) ...] op) op]
  [(subst [(var expr) ...] (A (num_sh ...) (expr_val ...)))
   (A (num_sh ...) ((subst [(var expr) ...] expr_val) ...))]
  [(subst [(var expr) ...] (op expr_arg ...))
   (op (subst [(var expr) ...] expr_arg) ...)]
  [(subst [(var expr) ...] (expr_fun expr_arg ...))
   ((subst [(var expr) ...] expr_fun) (subst [(var expr) ...] expr_arg) ...)]
  [(subst [(var_0 expr_0) ... (var_1 expr_1) (var_2 expr_2) ...] var_1) expr_1]
  [(subst [(var_0 expr_0) ...] var_1) var_1]
  [(subst [(var_sub expr_sub) ...] (λ [(var_arg num_arg) ...] expr_body))
   (λ [(var_arg num_arg) ...]
     (subst (shadow [(var_sub expr_sub) ...] (var_arg ...)) expr_body))])

;; eliminate some variables from a substitution list
(define-metafunction Arrays
  shadow : [(var expr) ...] (var ...) -> [(var expr) ...]
  ; empty sub list -> no change
  [(shadow [] ()) []]
  ; empty shadow list -> no change
  [(shadow [(var expr) ...] ()) [(var expr) ...]]
  ; first in shadow list is also in sub list -> remove from sub list, recur
  [(shadow [(var_sub0 expr_0) ... (var_sub1 expr_1) (var_sub2 expr_2) ...]
           (var_sub1 var_shadow ...))
   (shadow [(var_sub0 expr_0) ... (var_sub2 expr_2) ...]
           (var_sub1 var_shadow ...))]
  ; first in shadow list is not in sub list -> remove from shadow list, recur
  [(shadow [(var_sub expr) ...]
           (var_shadow0 var_shadow1 ...))
   (shadow [(var_sub expr) ...]
           (var_shadow1 ...))])
  

;; make sure array is well-shaped: product of shape must be length of value
;; only usable on array whose exact shape is known (shape piece normalized)
(define-metafunction Arrays
  well-shaped : arr -> bool
  [(well-shaped (A (num_s ...) (expr_v ...)))
   ,(= (foldr * 1 (term (num_s ...)))
       (length (term (expr_v ...))))])

;; make sure array is at desired rank
(define-metafunction Arrays
  at-rank? : num arr -> bool
  [(at-rank? num arr)
   #t
   (where num (rank arr))]
  [(at-rank? +inf.0 arr) #t]
  [(at-rank? num arr) #f])

;; make sure array is above desired rank
(define-metafunction Arrays
  overrank? : num arr -> bool
  [(overrank? +inf.0 arr) #f]
  [(overrank? num arr)
   #t
   (side-condition (< (term num) (term (rank arr))))]
  [(overrank? num arr) #f])

;; find how far above desired rank array is
(define-metafunction Arrays
  overrank : num arr -> num or #f
  [(overrank +inf.0 arr) 0]
  [(overrank num_neg arr)
   ,(- (term num_neg))
   (side-condition (negative? (term num_neg)))
   (side-condition (>= (- (term num_neg)) (term (rank arr))))]
  [(overrank num arr) ,(- (term num) (term (rank arr)))]
  [(overrank num arr) #f])

;; make sure all arrays are overranked by the same amount
(define-metafunction Arrays
  same-overrank? : [(num arr) ...] -> bool
  [(same-overrank? []) #t]
  [(same-overrank? [(num arr)]) #t]
  [(same-overrank? [(num_1 arr_1) (num_2 arr_2) (num_3 arr_3) ...])
   (same-overrank? [(num_2 arr_2) (num_3 arr_3) ...])
   (side-condition (= (term (overrank num_1 arr_1))
                      (term (overrank num_2 arr_2))))]
  [(same-overrank? [(num_1 arr_1) (num_2 arr_2) (num_3 arr_3) ...]) #f])
  

;; extract rank of array
(define-metafunction Arrays
  rank : arr -> num
  [(rank (A (num ...) (expr ...)))
   ,(length (term (num ...)))])

;; extract shape of array
(define-metafunction Arrays
  shape : arr -> (num ...)
  [(shape (A (num ...) (expr ...))) (num ...)])

;; extract value of array
(define-metafunction Arrays
  value : arr -> (expr ...)
  [(value (A (num ...) (expr ...))) (expr ...)])

;; grow argument arrays by duplication so they all have their desired ranks
;; frame-lift preprocesses input by converting infinite/negative cell ranks
;; to natural rank -- actual array growth is handled by frame-lift*
(define-metafunction Arrays
  ; [(cell-rank array) ...]
  frame-lift : [(num arr) ...] -> (arr ...)
  [(frame-lift [(natural arr) ...]) (frame-lift* [(natural arr) ...])]
  [(frame-lift [(natural_0 arr_0) ... (+inf.0 arr_1) (num_2 arr_2) ...])
   (frame-lift [(natural_0 arr_0) ... ((rank arr_1) arr_1) (num_2 arr_2) ...])]
  [(frame-lift [(natural_0 arr_0) ... (num_neg arr_1) (num_2 arr_2) ...])
   (frame-lift [(natural_0 arr_0) ... (num_nat arr_1) (num_2 arr_2) ...])
   (side-condition (and (exact-integer? (term num_neg))
                        (negative? (term num_neg))))
   (where num_nat (natural-cell-rank num_neg arr_1))])
(define-metafunction Arrays
  ; [(cell-rank array) ...]
  frame-lift* : [(num arr) ...] -> (arr ...)
  [(frame-lift* []) ()]
  ; make sure arrays can be lifted into same frame
  ; (need prefix relation for frame shapes)
  ; "principal frame" comes from least-overranked array
  [(frame-lift* [(num_cr arr) ...])
   ((cell-dup num_cr (num_pfr ...) arr) ...)
   ; extract frame shapes
   (where ((num_fr ...) ...)
          ((drop-right/m (shape arr) num_cr) ...))
   ; find the longest one -- that is the principal frame
   (where (num_pfr ...) (longest ((num_fr ...) ...)))
   ; all other frames must be prefixes of it
   (side-condition (term (all ((prefix? (num_fr ...) (num_pfr ...)) ...))))])

;; duplicate cells of given array to lift it into desired frame
(define-metafunction Arrays
  ; cell rank, frame shape, initial array
  cell-dup : num (num ...) arr -> arr
  #;[(cell-dup num_cr (num_fs ...) arr)
     arr
     (side-condition (= (term num_cr)
                        (term (rank arr))))]
  ; All elements of a single cell should appear consecutively in value segment
  ; Just split value into chunks, repeat chunks right number of times, and
  ; update the shape.
  [(cell-dup num_cr (num_fs ...) arr)
   ; new array's shape is frame-portion ++ growth-portion ++ cell-shape
   ; new array's value comes from repeating the cells (number of copies is
   ; product of the "growth" portion of the shape)
   (A ,(append (term (drop-right/m (shape arr) num_cr))
               (term (num_growth ...))
               (term (take-right/m (shape arr) num_cr)))
      ,(foldr append '()
              (term ((repeat ,(foldr * 1 (term (num_growth ...)))
                             (base_cell ...)) ...))))
   ; break the array's value segment into its cells
   (where ((base_cell ...) ...)
          (cell-values (take-right/m (shape arr) num_cr)
                       arr))
   ; identify the part of the result shape that comes from lifting
   ; drop frame portion of array from left side of frame
   (where (num_growth ...)
          (drop/m (num_fs ...)
                  ,(- (term (rank arr)) (term num_cr))))
   ; require that the array actually be liftable into the frame
   ; i.e. frame portion of array must be prefix of given frame
   (side-condition (term (prefix? (drop-right/m (shape arr) num_cr)
                                  (num_fs ...))))])

;; repeat the list the given number of times
(define-metafunction Arrays
  repeat : num (any ...) -> (any ...)
  [(repeat 0 (any ...)) ()]
  [(repeat num (any ...))
   ,(append (term (any ...))
            (term (repeat ,(- (term num) 1) (any ...))))])

;; extract the value segments of an array's cells
(define-metafunction Arrays
  ; cell shape, array
  cell-values : (num ...) arr -> ((base ...) ...)
  [(cell-values (num_cellshape ...) arr)
   ((base ...) ...)
   (where ((A (num ...) (base ...)) ...)
          (cells/shape (num_cellshape ...) arr))])

;; split an array into cells
(define-metafunction Arrays
  ; cell shape, array
  cells/shape : (num ...) arr -> (arr ...)
  [(cells/shape (num_cellshape ...) (A (num_arrshape ...) ())) ()]
  [(cells/shape (num_cellshape ...) (A (num_arrshape ...) (base ...)))
   ,(cons (term (A (num_cellshape ...) (take/m (base ...) num_cellsize)))
          ; drop one cell's elements from array, and split remaining elements
          (term (cells/shape (num_cellshape ...)
                       (A (num_arrshape ...)
                          (drop/m (base ...) num_cellsize)))))
   (where num_cellsize ,(foldr * 1 (term (num_cellshape ...))))])
(define-metafunction Arrays
  ; cell rank, array
  cells/rank : num arr -> (arr ...)
  [(cells/rank natural arr)
   (cells/shape (take-right/m (shape arr) natural) arr)]
  [(cells/rank +inf.0 arr) (arr)]
  [(cells/rank num_neg arr)
   (cells/shape (drop/m (shape arr) ,(- (term num_neg))) arr)
   (side-condition (and (exact-integer? (term num_neg))
                        (negative? (term num_neg))))])

;; make sure all arrays can be lifted into required frame
(define-metafunction Arrays
  frame-match : [(num arr) ...] -> bool
  [(frame-match [(num arr) ...])
   (all [(prefix? (num_shape ...) any_maxframe) ...])
   (where ((num_shape ...) ...) (frame-shapes [(num arr) ...]))
   (where any_maxframe (longest (frame-shapes [(num arr) ...])))])

;; find the frame component of the arrays' shapes
(define-metafunction Arrays
  frame-shapes : [(num arr) ...] -> [(num ...) ...]
  [(frame-shapes [(num_cellrank (A (num_shape ...) (base ...))) ...])
   ; can't just use
   ; [,(drop-right (term num_rank) (term (num_shape ...))) ...]
   ; because it doesn't associate num_rank with the last ...
   [(drop-right/m (num_shape ...) num_cellrank) ...]])

;; find longest list in a list of lists
(define-metafunction Arrays
  longest : [(any ...) ...] -> (any ...)
  [(longest [(any ...)]) (any ...)]
  [(longest [(any_1 ...) (any_2 ...) (any_3 ...) ...])
   (longer (any_1 ...) (longest [(any_2 ...) (any_3 ...) ...]))])
;; select longer of two lists
(define-metafunction Arrays
  longer : (any ...) (any ...) -> (any ...)
  [(longer (any_1 ...) (any_2 ...))
   (any_2 ...)
   (side-condition (> (length (term (any_2 ...)))
                      (length (term (any_1 ...)))))]
  [(longer (any_1 ...) (any_2 ...)) (any_1 ...)])

;; check that one input list is a prefix of the other
(define-metafunction Arrays
  prefix? : (any ...) (any ...) -> bool
  [(prefix? (any_1 ...) (any_1 ... any_2 ...)) #t]
  [(prefix? (any_1 ...) (any_2 ...)) #f])

;; check that one input list is a suffix of the other
(define-metafunction Arrays
  suffix? : (any ...) (any ...) -> bool
  [(suffix? (any_2 ...) (any_1 ... any_2 ...)) #t]
  [(suffix? (any_1 ...) (any_2 ...)) #f])

;; metafunction wrapper for existing take function
(define-metafunction Arrays
  take/m : (any ...) num -> (any ...)
  [(take/m (any ...) num) ,(take (term (any ...)) (term num))])

;; metafunction wrapper for existing drop function
(define-metafunction Arrays
  drop/m : (any ...) num -> (any ...)
  [(drop/m (any ...) num) ,(drop (term (any ...)) (term num))])

;; metafunction wrapper for existing drop-right function
(define-metafunction Arrays
  take-right/m : (any ...) num -> (any ...)
  [(take-right/m (any ...) num) ,(take-right (term (any ...)) (term num))])

;; metafunction wrapper for existing drop-right function
(define-metafunction Arrays
  drop-right/m : (any ...) num -> (any ...)
  [(drop-right/m (any ...) num) ,(drop-right (term (any ...)) (term num))])

;; metafunction wrapper for existing length function
(define-metafunction Arrays
  length/m : num num -> bool
  [(length/m num_1 num_2) ,(length (term num_1) (term num_2))])

;; metafunction wrapper for existing > function
(define-metafunction Arrays
  >/m : num num -> bool
  [(>/m num_1 num_2) ,(> (term num_1) (term num_2))])

;; make sure all list entries are true
(define-metafunction Arrays
  all : (bool ...) -> bool
  [(all (#t ...)) #t]
  [(all any) #f])



(module+
 test
 
 ; three simple examples for how rank affects array lifting
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term (+ (A (3 3) (1 2 3
                      4 5 6
                      7 8 9))
            (A (3) (10 20 30)))))
  (term ((A (3 3) (11 12 13
                   24 25 26
                   37 38 39)))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((λ ([x 1][y 1]) (+ x y)) (A (3 3) (1 2 3
                                             4 5 6
                                             7 8 9))
                                   (A (3) (10 20 30)))))
  (term ((A (3 3) (11 22 33
                   14 25 36
                   17 28 39)))))
 
 (check-equal?
  (apply-reduction-relation*
   ->Array
   (term ((λ ([x -1][y 1]) (+ x y)) (A (3 3) (1 2 3
                                              4 5 6
                                              7 8 9))
                                    (A (3) (10 20 30)))))
  (term ((A (3 3) (11 22 33
                   14 25 36
                   17 28 39))))))
