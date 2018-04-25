#lang racket

(require "../../odysseus/lib/load/all.rkt")
(require "../../odysseus/kb/tab-tree.rkt")
(require "../../odysseus/reports/csv.rkt")

(define divisor ",")

(define programming_languages (get-leaves (parse-tab-tree "../programming-languages.tree")))

(define csv-nodes
  (let* (
        (headers '(id name year designer company university from))
        (headers-str (implode (map ->string headers) divisor)))
    (for/fold
      ((nodes headers-str))
      ((item programming_languages))
      (format "~a~n~a"
              nodes
              (hash->csv-line
                item
                headers)))))

(write-file "organism_description_language_nodes.csv" csv-nodes)

(define csv-relations
  (let* (
        (headers '(source target type kind))
        (headers-str (implode (map ->string headers) divisor)))
    (for/fold
      ((rels headers-str))
      ((item programming_languages))
      (let* ((from-ids (hash-ref item 'from #f))
            (from-ids (if from-ids (split from-ids ",") #f)))
        (if from-ids
            (for/fold
              ((rels-group rels))
              ((from-id from-ids))
              (format "~a~n~a"
                      rels-group
                      (hash->csv-line
                        (hash 'source from-id 'target ($ id item) 'type "directed" 'kind "influenced")
                        headers)))
            rels)))))

(write-file "organism_description_language_relations.csv" csv-relations)
