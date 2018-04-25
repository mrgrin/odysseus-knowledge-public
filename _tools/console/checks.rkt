 #lang racket

(require "../../../odysseus/lib/load/all.rkt")
(require "../../../odysseus/graphics/console.rkt")
(require "../../../odysseus/kb/tab-tree.rkt")
(require "../../../odysseus/kb/structure.rkt")

; (define divisor ",")

(define (special-word? x)
  (indexof? '(<unique> <?> <self>) (->symbol x)))

(define programming_languages (get-leaves (parse-tab-tree "../../computer-science/languages.tree")))
(define refs (ref-triplets (parse-tab-tree (pth "meta/refs.tree"))))

; check refs completeness by field-name (e.g. programming languages refs through 'from field)
; (check-registry "programming-languages" 'university "universities")
(define (check-registry base-registry-file field-name referred-registry-file (exclude-attributes '((id name) (id name))))
  (let* ((base-registry (get-leaves (parse-tab-tree base-registry-file) #:exclude (first exclude-attributes)))
        (referred-registry (if (equal? base-registry-file referred-registry-file)
                            base-registry
                            (get-leaves (parse-tab-tree referred-registry-file) #:exclude (second exclude-attributes))))
        (refs (uniques
                (cleanmap
                  (flatten
                    (map (λ (x)
                            (let* ((ref-group (hash-ref x field-name #f))
                                  (ref-group (if ref-group (split ref-group ",") #f)))
                                ref-group))
                          base-registry)))))
        (referred-registry-list (uniques
                                  (append
                                    (map (λ (x) ($ id x)) referred-registry) ; ids
                                    (cleanmap (map (λ (x) (->string ($ alt-id x))) referred-registry)))))) ; alt-ids
    (filter-not special-word? (minus refs referred-registry-list))))

(displayln (format "Total languages: ~a" (length programming_languages)))

(for
  ((r refs))
  (let* ((intersection
          (apply
            intersect
            (map (λ (x)
                    (check-registry (nth r 1) (->symbol (nth r 2)) x))
                  (nth r 3))))
        (intersection (filter-not special-word? intersection)))
    (when (not-empty? intersection)
        (set-text-color 'grey)
        (displayln "-------------------------------------------------------------------")
        (set-text-color 'green)
        (displayln (str (nth r 4) ":"))
        (set-text-color 'default)
        (--- intersection))))
        ; (for ((i intersection)) (displayln i)))))
