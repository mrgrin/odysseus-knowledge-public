#lang racket

(require "../../../odysseus/lib/load/all.rkt")
(require "../../../odysseus/kb/tab-tree.rkt")
(require "../../../odysseus/kb/neo4j/neo4j.rkt")
(require "../../../odysseus/kb/neo4j/util.rkt")
(require "../../../odysseus/kb/structure.rkt")

; the result should be run through: cypher-shell -u neo4j -p <pwd> < c:/denis/denis_core/odysseus-knowledge/_tools/neo4j/odysseus-knowledge.cypher

(define refs (parse-tab-tree (pth "meta/refs.tree")))

(define (get-all-leaves refs-hash-tree)
  (let* ((paths (get-paths refs-hash-tree))
        (paths (map but-last paths))
        (refs (filter (λ (element) ($ ref element)) (planarize refs-hash-tree)))
        (refs (map (λ (element) ($ ref element)) refs))
        (refs (apply append (map (λ (x) (split x ",")) refs)))
        (refs (map (λ (x) (split x "/")) refs))
        (all-leaves (uniques (append paths refs)))
        )
    (uniques
      (append
        (map
          (λ (x) (get-leaves x #:exclude '(name)))
          (map
            parse-tab-tree
            (map
              pth
              (map
                leaf->filename
                all-leaves))))))))

(define-catch (generate-cypher refs)
  (let* ((nodes (flatten (get-all-leaves refs)))
    ; (--- (neo4j/authenticate "localhost"))
        ; (ref-attributes (uniques
        ;                   (flatten
        ;                     (map
        ;                       (λ (x) (hash-keys
        ;                                 (hash-filter
        ;                                   (λ (k v) (--- k v (type k) (type v)) (symbol? v))
        ;                                   x)))
        ;                       (planarize refs)))))
        (nodes-cypher
          (for/fold
            ((cypher-str "//"))
            ((node nodes))
            (format "~a;~n~a"
                    cypher-str
                    (gdb/create-node (neo4jy-keys node) #:label-attr '_label))))
        (connections-cypher
            (for/fold
              ((cypher-str ""))
              ((node nodes))
              (format "~a~a"
                      cypher-str
                      (for/fold
                        ((connections-cypher-str ""))
                        ((k (hash-keys node)))
                        (let* (
                              (value (hash-ref node k))
                              (value (if value (split value ",") value))
                              (matched-elements (filter (λ (x) (let ((condition
                                                                (and
                                                                  (indexof? value ($ id x))
                                                                  (not-equal? ($ id x) ($ id node)))))
                                                            ; (when condition (--- ($ id node) ($ id x) (equal? ($ id node) ($ id x))))
                                                            ; (when (equal? ($ id node) "C") (--- ($ id node) ($ id x) ($ from node) (indexof? ($ from node) ($ id x)) (equal? ($ id node) ($ id x))))
                                                            condition))
                                                  nodes)))
                          ; (--- node k value)
                          (if (and
                                (not-empty? value)
                                (not-empty? matched-elements))
                              (for/fold
                                ((connections-per-one-node connections-cypher-str))
                                ((matched-element matched-elements))
                                (format "~a;~n~a"
                                  connections-per-one-node
                                  (gdb/create-rel ($ id node) ($ id matched-element) (neo4jy-attr k))))
                              connections-cypher-str)))))))
      (format "~a~a" nodes-cypher connections-cypher)
      ))

(write-file "odysseus-knowledge.cypher" (generate-cypher refs))
