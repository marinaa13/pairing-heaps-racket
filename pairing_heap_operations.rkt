#lang racket
(require "etapa2.rkt")
(require racket/match)
(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor heap-urilor de 
;; împerechere, pe care le vom folosi pentru:
;;  - a extrage cele mai bune filme dintr-o listă, conform
;;    cu un anumit criteriu
;;  - a extrage cele mai bune recenzii dintr-o colecție
;;    de recenzii pentru diverse filme (o recenzie bună
;;    corespunde unei note bune acordate filmului respectiv)
;;
;; Pentru completarea cu succes a etapei este necesar să
;; rezolvați sarcinile cu algoritmii dedicați PH-urilor 
;; (descriși în enunț). Punctajul acordat de checker va fi 
;; retras dacă sarcinile sunt rezolvate cu alți algoritmi.


; TODO 1 (40p)
; Definiți funcția best-k într-o formă care
; facilitează derivarea ulterioară a funcțiilor
; best-k-rating și best-k-duration.
; in: criteriu de comparație op (care compară 2 filme),
;     listă de filme movies, număr k
; out: lista sortată a celor mai "bune" filme
;      conform criteriului (cel mai "bun" primul)
; Algoritm:
;  1. construiește un PH de filme pe baza listei movies 
;     și a criteriului op
;  2. extrage în mod repetat root-ul acestui PH până
;     când rezultatul conține k filme (sau PH-ul devine vid)
; RESTRICȚII (20p):
;  - Folosiți named let pentru a efectua pasul 2 al
;    algoritmului.
(define (best-k op movies k)
  (let func ([L (list->ph op movies)] [acc '()])
    (if (or (null? L) (equal? (length acc) k))
        acc
        (func (ph-del-root op L) (append acc (list (ph-root L)))))))
        

; best-k-rating : [Movie] x Int -> [Movie]
; in: listă de filme movies, număr k
; out: cele mai bune k filme din movies (ca rating)
; RESTRICȚII (5p):
;  - Obțineți best-k-rating ca aplicație a lui best-k.
(define merge-rating
  (merge-f (λ (x y) (< (movie-rating x) (movie-rating y)))))

(define (best-k-rating movies k)
  (best-k merge-rating movies k))

; best-k-duration : [Movie] x Int -> [Movie]
; in: listă de filme movies, număr k
; out: cele mai scurte k filme din movies 
; RESTRICȚII (5p):
;  - Obțineți best-k-duration ca aplicație a lui best-k.
(define merge-duration
  (merge-f (λ (x y) (if (equal? (car (movie-duration x)) (car (movie-duration y)))
                        (> (cadr (movie-duration x)) (cadr (movie-duration y)))
                        (> (car (movie-duration x)) (car (movie-duration y)))))))

(define (best-k-duration movies k)
  (best-k merge-duration movies k))


; TODO 2 (30p)
; update-pairs : ((Symbol, PH) -> Bool) x [(Symbol, PH)]
;                -> [(Symbol, PH)]
; in: predicat p, listă de perechi (nume-film . PH)
;     (PH este un max-PH care conține notele acordate
;      filmului în diverse recenzii - așadar un PH
;      de numere)
; out: lista pairs actualizată astfel:
;      - pentru prima pereche care satisface predicatul 
;        p, PH-ului perechii i se șterge rădăcina
;      - dacă PH-ul perechii este vid sau dacă nicio pereche
;        nu satisface p, se întoarce lista pairs nemodificată
; RESTRICȚII (20p):
;  - Folosiți named let pentru a itera prin perechi.
(define (update-pairs p pairs)
  (let func([L pairs] [acc '()])
    (if (null? L)
        acc
        (if (and (not (null? (cdr(car L)))) (p (car L)))
            (append acc (list(cons (car (car L)) (ph-del-root merge-max (cdr (car L))))) (cdr L))
            (func (cdr L) (append acc (list(car L))))))))
                  

; TODO 3 (50p)
; best-k-ratings-overall : [(Symbol, PH)] x Int
;                          -> [(Symbol, Number)]
; in: listă de perechi (nume-film . PH)
;     (ca mai sus, PH este un max-PH de rating-uri)
;     număr k 
; out: lista sortată a celor mai bune k perechi
;      (nume-film . rating), corespunzând celor mai
;      bune rating-uri din toate PH-urile
; Algoritm:
;  1. Se inițializează un PH de perechi (nume . rating), 
;     corespunzând celui mai bun rating al fiecărui film
;     (adică se extrage rădăcina fiecărui PH de ratinguri,
;      în pereche cu numele filmului aferent)
;  2. Repetă de k ori:
;     - extrage rădăcina PH-ului de rădăcini
;       (name-root . rating-root)
;       (adică extrage cea mai bună pereche per total)
;     - adu în PH-ul de rădăcini următorul cel mai bun 
;       rating al filmului name-root (dacă există)
; RESTRICȚII (20p):
;  - Folosiți named let pentru a efectua pasul 2 al
;    algoritmului

; imi sterge root ul din fiecare ph de review uri
(define (remove-roots L)
  (if (null? L)
      '()
      (cons (cons (car (car L)) (ph-del-root merge-max (cdr (car L)))) (remove-roots (cdr L)))))

; face un ph din root urile de la fiecare ph de review uri
(define (make-roots-ph L)
  (if (null? L)
      '()
       (ph-insert merge-max-rating (cons (car (car L)) (ph-root (cdr (car L)))) (make-roots-ph (cdr L)))))

; daca filmul indeplineste cond p, imi returneaza root ul din ph ul de reviewuri
(define (get-root L p)
  (if (p (car L))
      (if (ph-root (cdr (car L)))
          (cons (car (car L)) (ph-root (cdr (car L))))
          null)
      (get-root (cdr L) p)))

; scoate root ul PH ului si adauga primul review al filmului corespunzator
(define (update-PH PH new_pairs)
  (let ([new_PH (ph-del-root merge-max-rating PH)])
    (if (null? PH) null
        (if (equal? (get-root new_pairs (λ(x) (equal? (car (ph-root PH)) (car x)))) null)
            new_PH
            (ph-insert merge-max-rating (get-root new_pairs (λ(x) (equal? (car (ph-root PH)) (car x)))) new_PH)
            ))))
      

(define (best-k-ratings-overall pairs k)
  (let func ([new_pairs (remove-roots pairs)] [PH (make-roots-ph pairs)] [acc null])
    (if (or (equal? (length acc) k) (null? PH))
        acc
        (func (update-pairs (λ(x) (equal? (car (ph-root PH)) (car x))) new_pairs) (update-PH PH new_pairs) (append acc (list (ph-root PH)))))))

