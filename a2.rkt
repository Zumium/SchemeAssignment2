;-----------------------------
; Assignment Group <number>

; <name1>, <student ID1>
; <name2>, <student ID2>
; <name3>, <student ID3>
;-----------------------------
(require racket/set) ;import needed standard library
;-------------------------------------------------------------------------------------
; If "graph" is acyclic, "topsort" returns an ordered list of its vertices obtained by
; topologically sorting it; otherwise "topsort" returns the symbol "cyclic".
;-------------------------------------------------------------------------------------

(define (topsort graph)
    (if (null? graph) '()              ; If "graph" is empty, () is a total ordering.
    (let ((small (noInEdges graph)))   ; Otherwise, get the list of vertices with no
      (if (null? small)                ; inedges.
          'cyclic                      ; If none, then "graph" is cyclic.
          (let ((rest (topsort                   ; Otherwise, sort the graph that results
                       (delete small graph))))   ; from deleting these vertices.
            (if (eq? rest 'cyclic)               ; If the reduced graph is cyclic, then
                'cyclic                          ; so is "graph".
                (append small rest)))))))        ; Otherwise, construct total ordering.

;-------------------------------------------------------------------------------------
; "noInEdges" returns a list of all vertices of a graph which have no incoming edges.
;-------------------------------------------------------------------------------------
;diff -- return a sub-list of list2 whose members are not in list1
(define (diff list1 list2)
	(filter   ;filter list2 with given results produced by the lambda-defined function below
		(lambda (x) 
			(not (member x list1));see if x is a member of list1
		) 
	list2)
)

;union -- get the union of the given lists,which are members of "lists"
(define (union lists)
	(foldl     ;apply the operation down through the members of 'lists'
		(lambda (a result) ;define the operation
			(set-union a result) ;the "set-union" is from racket's standard library,which returns the union of the given two lists
		)
	'() lists)
)

(define (noInEdges graph)
	(diff 
		(union (map cdr graph)) ;get a list of vertices who has incoming edges
		(union graph) ;get a list of all vertices
	)
)

;-------------------------------------------------------------------------------------
; "delete" returns the graph obtained by removing the vertices with no incoming edges
; from a graph
;-------------------------------------------------------------------------------------

(define (delete vertices graph)
	(filter  ;drop out edges from graph whose starting point is a to-be-deleted vertix
		(lambda (x)
			(not (member (car x) vertices)) ;see if the starting point is in the "vertices" list
		)
	graph)
)

;-------------------------------------------------------------------------------------
; Some graphs to try.
;-------------------------------------------------------------------------------------

(define cyclic1
   '((a b b e)
     (b c)
     (c)
     (d a b)
     (e d f)
     (f d)))

(define cyclic2
   '((a a)))

(define cyclic3
   '((a b)
     (b c)
     (c d)
     (d e)
     (e f)
     (f a)))

(define acyclic1
   '((a b c e)
     (b c)
     (c f)
     (d)
     (e d f)
     (f)))

(define acyclic2
   '())

(define acyclic3
   '((a b c d e f)
     (b c d e f)
     (c d e f)
     (d e f)
     (e f)
     (f)))

