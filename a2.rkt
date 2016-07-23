;-----------------------------
; Assignment Group <number>

; <name1>, <student ID1>
; <name2>, <student ID2>
; <name3>, <student ID3>
;-----------------------------
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
(define (noInEdges graph)
  (if (null? graph) '()
      (findallvex graph (getvex graph))))


(define (getvex graph)
  (cond ((null? (cdr graph)) (list (caar graph)))
        (else
         (let (( vexes (getvex (cdr graph))))
         (cons (caar graph) vexes)))))

(define (findallvex graph vexList)
  (cond ((null? vexList) '())
        (else
         (let ((vexes (findallvex graph (cdr vexList))))
           (let ((avex (findAvex graph (car VexList))))
             (if (not (boolean? avex))
                 (cons avex vexes)
                 vexes))))))
         
         

(define (findAvex graph vex)
  (cond ((null? graph) vex)
        ((ismember vex (cdar graph)) )
        (else
         (findAvex (cdr graph) vex))))
           
(define (ismember x list)
  (cond ((null? list) #f)
        ((eq? x (car list)) #t)
        (else
         (ismember x (cdr list)))))


;-------------------------------------------------------------------------------------
; "delete" returns the graph obtained by removing the vertices with no incoming edges
; from a graph
;-------------------------------------------------------------------------------------

(define (delete vertices graph)
	(cond ((null? vertices) graph)
              ((eq? (car vertices) (caar graph)) (delete (cdr vertices) (cdr graph)))
              (else
               (delete vertices (append (cdr graph) (list (car graph)))))))
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

