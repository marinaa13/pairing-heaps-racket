#lang racket
(require racket/match)
(require "etapa2.rkt")
(provide (all-defined-out))

;; Această etapă continuă seria aplicațiilor heap-urilor  
;; de împerechere, pe care le vom folosi pentru a calcula
;; în mod dinamic mediana recenziilor unui film, simulând
;; condițiile din realitate - în care apar în permanență
;; noi recenzii pentru diverse filme.
;;    
;; Pentru a modela această dinamică folosim un flux de
;; perechi (nume-film . rating), pe baza căruia calculăm
;; un flux de stadii evolutive astfel:
;;  - fiecare stadiu este reprezentat ca listă de perechi
;;    * o pereche pentru fiecare film cu minim o recenzie
;;    * fiecare pereche este de forma
;;      (nume-film . mediană-rating-uri-primite-până-acum)
;;  - fiecare nouă recenzie determină actualizarea unei
;;    mediane, adică trecerea într-un alt stadiu,
;;    generând un nou element în fluxul rezultat
;;
;; Algoritmul folosit este următorul:
;;  Fluxul de perechi este transformat într-un flux de
;;  liste de cvartete (nume-film delta max-ph min-ph)
;;   - fiecare element din flux conține câte un cvartet 
;;     pentru fiecare film care are minim o recenzie
;;   - dacă filmul are un număr par de recenzii:
;;     - max-ph și min-ph au aceeași dimensiune
;;     - delta = size(max-ph) - size(min-ph) = 0
;;     - max-ph = max-PH cu cele mai mici rating-uri
;;     - min-ph = min-PH cu cele mai mari rating-uri
;;     - mediana este media rădăcinilor celor 2 PH-uri
;;   - dacă filmul are un număr impar de recenzii:
;;     - max-ph are un element în plus față de min-ph
;;     - delta = size(max-ph) - size(min-ph) = 1
;;     - max-ph = max-PH cu cele mai mici rating-uri
;;     - min-ph = min-PH cu cele mai mari rating-uri
;;     - mediana este rădăcina lui max-ph
;;
;; Pentru completarea cu succes a etapei este necesar să
;; calculați medianele cu algoritmul descris în enunț.
;; În caz contrar, punctajul acordat de checker va fi retras.


; TODO 1 (45p)
; add-rating : (Symbol, Int, PH, PH) x Number
;              -> (Symbol, Int, PH, PH)
; in: cvartet (nume delta max-ph min-ph),
;     rating de adăugat
; out: cvartet actualizat prin adăugarea 
;      rating-ului, astfel:
;  - dacă rating <= root(max-ph)
;    inserează rating în max-ph, actualizând delta
;  - altfel
;    inserează rating în min-ph, actualizând delta
;  - dacă delta > 1
;    mută root(max-ph) în min-ph
;  - dacă delta < 0
;    mută root(min-ph) în max-ph

(define (full-length lst)
  (cond
    ((null? lst) 0)
    ((list? (car lst)) (+ (full-length (car lst)) (full-length (cdr lst))))
    (else (+ 1 (full-length (cdr lst))))))


(define (update-quad quad)
  (let ((max_ph (car (cdr (cdr quad)))) (min_ph (car (cdr (cdr (cdr quad))))) (delta (car (cdr quad))))
    (cond ((> delta 1) (list (car quad) (- (sub1 (full-length max_ph)) (add1 (full-length min_ph))) (ph-del-root merge-max max_ph) (ph-insert merge-min (ph-root max_ph) min_ph)))
          ((< delta 0) (list (car quad) (- (add1 (full-length max_ph)) (sub1 (full-length min_ph))) (ph-insert merge-max (ph-root min_ph) max_ph) (ph-del-root merge-min min_ph)))
          (else quad))))

(define (add-rating quad rating)
  (let ((max_ph (car (cdr (cdr quad)))) (min_ph (car (cdr (cdr (cdr quad))))))
      (update-quad (if (<= rating (ph-root max_ph))
          (list (car quad) (- (add1 (full-length max_ph)) (full-length min_ph)) (ph-insert merge-max rating max_ph) min_ph)
          (list (car quad) (- (full-length max_ph) (add1 (full-length min_ph))) max_ph (ph-insert merge-min rating min_ph))))))


; TODO 2 (45p)
; reviews->quads : Stream<(Symbol, Number)> ->
;                  Stream<[(Symbol, Int, PH, PH)]>
; in: stream de perechi (nume . rating)
; out: stream de liste de cvartete
;      (nume delta max-ph min-ph)
;  - elementul k din rezultat corespunde primelor
;    k recenzii din input (ex: dacă primele 10
;    recenzii sunt pentru 3 filme distincte, al
;    10-lea element din fluxul rezultat conține o
;    listă de 3 cvartete - unul pentru fiecare film)
; RESTRICȚII (20p):
;  - Lucrați cu operatorii pe fluxuri, fără a
;    converti liste în fluxuri sau fluxuri în liste.

; comparator = functia de comparat 2 elem
(define (compare-names x y)
  (if (equal? (car x) (car y))
      #t
      #f))

(define (get-quad pair)
  (let ((name (car pair)) (review (cdr pair)))
    (list name 1 (list review) '())))

(define (update-list elem L comparator update-func)
  (cond ((null? L) L)
        ((comparator (car L) elem)
         (cons (update-func (car L)) (cdr L)))
        ((cons (car L) (update-list elem (cdr L) comparator update-func)))))

(define (find-in-list elem L comparator)
  (cond ((null? L) #f)
        ((comparator (car L) elem) #t)
        (else (find-in-list elem (cdr L) comparator))))

(define (reviews->quads-helper reviews [acc '()])
  (if (stream-empty? reviews)
      empty-stream
      (let* ((rev (get-quad (stream-first reviews)))
            (rating (car (caddr rev)))
            (new-acc
             (if (find-in-list rev acc compare-names)
                 (update-list rev acc compare-names (λ (x) (add-rating x rating)))
                 (cons rev acc))))
       (stream-cons new-acc (reviews->quads-helper (stream-rest reviews) new-acc)))))

(define (reviews->quads reviews)
    (if (stream-empty? reviews) empty-stream
        (reviews->quads-helper reviews)))
   

; TODO 3 (30p)
; quads->medians : Stream<[(Symbol, Int, PH, PH)]> ->
;                  Stream<[(Symbol, Number)]>  
; in: stream de liste de cvartete (ca mai sus)
; out: stream de liste de perechi (nume-film . mediană)
;  - mediana se calculează pe baza PH-urilor din
;    fiecare cvartet, conform algoritmului de mai sus
; RESTRICȚII (20p):
;  - Nu folosiți recursivitate explicită. Folosiți cel
;    puțin o funcțională pe fluxuri.

(define (get-median quad)
  (let ((max_ph (car (cdr (cdr quad)))) (min_ph (car (cdr (cdr (cdr quad))))))
    (cons
     (car quad) 
     (if (even? (+ (full-length max_ph) (full-length min_ph)))
        (/ (+ (ph-root max_ph) (ph-root min_ph)) 2)
        (ph-root max_ph)))))

(define (parse-quads quads)
  (map get-median quads))
 
(define (quads->medians quads)
  (if (stream-empty? quads)
      quads
      (stream-map parse-quads quads)))
