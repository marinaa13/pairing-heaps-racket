#lang racket
(require racket/match)
(provide (all-defined-out))

;; În această etapă abstractizăm operatorii tipului PH astfel
;; încât să putem deriva ușor operațiile pentru diverse variante
;; de PH, în funcție de relația de ordine pe care se bazează
;; proprietatea de heap.
;;  - funcție afectată direct: merge
;;  - funcții afectate indirect: funcțiile care apelează merge,
;;     care vor avea nevoie să primească tipul de merge ca parametru
;;
;; Apoi, folosim tipul PH pentru a prelucra filme, unde un film
;; este reprezentat ca o structură cu 5 câmpuri: nume, rating, gen, 
;; durată, altele.
;; În Racket, există un mod simplu de a defini și manipula structuri,
;; descris în fișierul "tutorial.rkt".
;;
;; Fluxul de lucru recomandat pentru etapa 2 este:
;; - Copiați din etapa 1 funcțiile care rămân neschimbate
;; - Abstractizați după relația de ordine:
;;  * definiți operatorul mai general merge-f care primește, în plus
;;    față de merge, un comparator după care trebuie ordonate elementele
;;  * derivați din acest operator variantele cerute de merge
;;  * modificați acele funcții din etapa 1 care apelează merge, astfel
;;    încât funcția merge să fie parametru al funcției, nu un identificator
;;    legat la o valoare externă
;; - Citiți tutorialul despre structuri în Racket (fișierul "tutorial.rkt")
;; - Implementați funcțiile care prelucrează filme 


; TODO 0 (0p)
; Copiați din etapa 1 implementările funcțiilor
; de la TODO 1.

; empty-ph : PH
; out: PH-ul vid
(define empty-ph
  '())

; val->ph : T -> PH
; in: o valoare de un tip oarecare T
; out: PH-ul care conține doar această valoare
(define (val->ph val)
  (cons val null))

; ph-empty? : PH -> Bool
; in: pairing heap ph
; out: true, dacă ph este vid
;      false, altfel
(define (ph-empty? ph)
  (if (null? ph)
      #t
      #f))

; ph-root : PH -> T | Bool
; in: pairing heap ph
; out: false, dacă ph e vid
;      root(ph), altfel
(define (ph-root ph)
  (if(ph-empty? ph)
     #f
     (car ph)))

; ph-subtrees : PH -> [PH] | Bool
; in: pairing heap ph
; out: false, dacă ph e vid
;      copii(ph), altfel
(define (ph-subtrees ph)
  (if (ph-empty? ph)
      #f
      (cdr ph)))

; merge: PH x PH -> PH
; in: pairing heaps ph1, ph2
; out: union(ph1, ph2) astfel:
;  - union(vid, orice) = orice
;  - altfel, PH-ul cu root mai mic devine
;    primul fiu al celui cu root mai mare
;    (prin convenție, dacă rădăcinile sunt
;    egale, ph2 devine fiul lui ph1)
(define (merge ph1 ph2 comp)
  (cond ((ph-empty? ph1) ph2)
        ((ph-empty? ph2) ph1)
        (else (if (comp (car ph1) (car ph2))
                  (append (list(car ph2)) (list ph1) (cdr ph2))
                  (append (list(car ph1)) (list ph2) (cdr ph1))))))
      

; ph-insert : T x PH -> PH
; in: valoare val, pairing heap ph
; out: ph' rezultat după inserția lui val în ph
;  - inserția este un merge între ph și 
;    PH-ul creat doar din valoarea val
;    (în această ordine)
(define (ph-insert merge-fct val ph)
  (merge-fct ph (val->ph val)))


; list->ph : [T] -> PH
; in: listă de valori lst
; out: ph' rezultat din inserții repetate
;  - se inserează ultimul element din lst în PH-ul vid
;  - ...
;  - se inserează primul element din lst în PH-ul de până acum
(define (list->ph merge-fct lst)
  (if (null? lst)
      empty-ph
      (ph-insert merge-fct (car lst) (list->ph merge-fct (cdr lst)))))


; two-pass-merge-LR : [PH] -> PH
; in: listă de PH-uri phs
; out: ph' rezultat din merge stânga-dreapta:
;  - merge de primele două PH-uri
;  - merge de rezultat cu merge de următoarele două
;  ...
;  - merge de rezultat cu:
;    - merge de ultimele două PH-uri, dacă nr_par(phs)
;    - ultimul PH, dacă nr_impar(phs)
(define (two-pass-merge-LR merge-fct phs [acc empty-ph])
  (if (null? phs)
      acc
      (if (null? (cdr phs))
          (merge-fct acc (car phs))
          (two-pass-merge-LR merge-fct (cdr (cdr phs)) (merge-fct acc (merge-fct (car phs) (car (cdr phs))))))))


; two-pass-merge-RL : [PH] -> PH
; in: listă de PH-uri phs
; out: ph' rezultat din merge dreapta-stânga
; (ca mai sus, dar se începe cu ultimele două PH-uri:
;  - merge de penultimul cu ultimul
;  - merge de rezultat cu merge de anterioarele două etc.)
(define (two-pass-merge-RL merge-fct phs)
  (if (null? phs)
      phs
      (if (null? (cdr phs))
          (car phs)
          (if (even? (length phs))
              (merge-fct (two-pass-merge-RL merge-fct (cdr (cdr phs))) (merge-fct (car phs) (car (cdr phs))))
              (merge-fct (two-pass-merge-RL merge-fct (cdr phs)) (car phs))))))
              

; tournament-merge : [PH] -> PH
; in: listă de PH-uri phs
; out: ph' rezultat din merge tip "knock-out"
;  - listele de ph-uri sunt parcurse stânga-dreapta
;  - merge două câte două (pentru număr impar, ultimul rămâne ca atare)
;  - merge două câte două între PH-urile rezultate anterior
;  ...
;  - până rămâne un singur PH
(define (tournament-merge phs)
  (cond
    ((null? phs) empty-ph)
    ((null? (cdr phs)) (car phs))
    (else
     (tournament-merge
      (merge-pairs phs)))))


(define (merge-pairs merge-fct phs)
  (cond
    ((null? phs) '())
    ((null? (cdr phs)) (list (car phs)))
    (else
     (cons (merge-fct (car phs) (cadr phs))
           (merge-pairs (cddr phs) merge-fct)))))

; ph-del-root : PH -> PH | Bool
; in: pairing heap ph
; out: false, dacă ph e vid
;      ph' rezultat în urma ștergerii root(ph), altfel
;      - fiii root(ph) sunt uniți prin two-pass-merge-LR
(define (ph-del-root merge-fct ph)
  (if (ph-empty? ph)
      #f
      (two-pass-merge-LR merge-fct (ph-subtrees ph)))) 


; TODO 1 (15p)
; Definiți funcția merge-f în formă curry, 
; astfel încât ulterior să definiți point-free
; funcțiile merge-min, merge-max și 
; merge-max-rating, ca aplicații parțiale
; ale lui merge-f.
;  - definiție point-free = o definiție care 
;    nu explicitează argumentul funcției
;   * ex: (define f add1) este o definiție point-free
;   * ex: (define (f x) (add1 x)) sau, echivalent,
;     (define f (λ (x) (add1 x))) nu sunt point-free
; merge-f = merge cu criteriul de comparație comp
; in: pairing heaps ph1, ph2, comparator comp
;     (ordinea și gruparea parametrilor
;     trebuie decisă de voi)
; out: union(ph1, ph2) astfel:
;   - union(vid, orice) = orice
;   - altfel, PH-ul cu root "mai puțin comp" 
;     devine primul fiu al celuilalt
;     (la egalitate, ph2 devine fiul lui ph1)
(define (merge-f comp)
  (λ (ph1 ph2)
    (merge ph1 ph2 comp)))

  ; merge-max : PH x PH -> PH
  ; in: pairing heaps ph1, ph2
  ; precondiții: ph1, ph2 sunt max-PH-uri
  ; out: max-PH rezultat din union(ph1, ph2)
  ; RESTRICȚII (5p):
  ;  - Definiția trebuie să fie point-free.
  (define merge-max
    (merge-f <))

  ; merge-min : PH x PH -> PH
  ; in: pairing heaps ph1, ph2
  ; precondiții: ph1, ph2 sunt min-PH-uri
  ; out: min-PH rezultat din union(ph1, ph2)
  ; RESTRICȚII (5p):
  ;  - Definiția trebuie să fie point-free.
  (define merge-min
    (merge-f >))

  ; merge-max-rating : PH x PH -> PH
  ; in: pairing heaps ph1, ph2
  ; precondiții: ph1, ph2 conțin perechi cu punct
  ; (nume . rating) și sunt max-PH-uri ordonate
  ; după rating
  ; out: max-PH rezultat din union(ph1, ph2)
  ; RESTRICȚII (5p):
  ;  - Definiția trebuie să fie point-free.
  (define merge-max-rating
    (merge-f (λ (x y) (< (cdr x) (cdr y)))))


(define (merge-genre genres)
    (merge-f (λ (x y) (not(before? (movie-genre x) (movie-genre y) genres)))))


  ; TODO 2 (10p)
  ; Redefiniți următoarele funcții din etapa 1 care
  ; apelează (direct sau indirect) merge, astfel
  ; încât funcția merge să fie dată ca parametru
  ; (pe prima poziție, ca în apelurile din checker):
  ;  - ph-insert
  ;  - list->ph
  ;  - two-pass-merge-LR
  ;  - ph-del-root


  ;; PARTEA A DOUA (cea în care prelucrăm filme)

  ;; Definim un film (movie) ca pe o structură cu 5 câmpuri:   
  ;; nume, rating, gen, durată, altele.
  (define-struct movie (name rating genre duration others) #:transparent)


  ; TODO 3 (10p)
  ; lst->movie : [Symbol, Number, Symbol, [Int], [Symbol]] -> Movie
  ; in: listă lst cu 5 valori, în această ordine:
  ;     - numele reprezentat ca simbol (ex: 'the-lives-of-others)
  ;     - ratingul reprezentat ca număr (ex: 8.4)
  ;     - genul reprezentat ca simbol (ex: 'drama)
  ;     - durata reprezentată ca listă de ore și minute (ex: '(2 17))
  ;     - altele reprezentate ca listă de simboluri (ex: '(german))
  ; out: obiect de tip movie instanțiat cu cele 5 valori
  ; RESTRICȚII (10p):
  ;  - Nu identificați elementele listei, ci folosiți o funcțională.
  (define (lst->movie lst)
    (apply movie lst))


  ; TODO 4 (10p)
  ; mark-as-seen : Movie -> Movie
  ; in: film m
  ; out: m actualizat astfel încât symbolul 'seen este
  ;      adăugat la începutul câmpului (listei) others
  (define (mark-as-seen m)
    (struct-copy movie m [others (cons'seen (movie-others m))]))


  ; TODO 5 (10p)
  ; mark-as-seen-from-list : [Movie] x [Symbol] -> [Movie]
  ; in: listă de filme movies, listă de nume seen
  ; out: lista movies actualizată astfel încât filmele
  ;      cu numele în lista seen sunt marcate ca văzute
  ; RESTRICȚII (10p):
  ;  - Nu folosiți recursivitate explicită.
  ;  - Folosiți cel puțin o funcțională.
  (define (mark-as-seen-from-list movies seen)
    (map (λ(x) (if (member (movie-name x) seen)
                  (mark-as-seen x)
                  x)) movies))
                  

 
  ; TODO 6 (10p)
  ; extract-seen : [Movie] -> [Symbol]
  ; in: listă de filme movies
  ; out: lista numelor filmelor văzute din lista movies
  ;      (văzut = lista others conține 'seen)
  ; RESTRICȚII (10p):
  ;  - Nu folosiți recursivitate explicită.
  ;  - Nu folosiți funcționale de tip fold.
  ;  - Folosiți cel puțin o funcțională.
  (define (extract-seen movies)
   (map (λ(x) (movie-name x)) (filter (λ(x) (member 'seen (movie-others x))) movies)))


  ; TODO 7 (15p)
  ; rating-stats : [Movie] -> (Number, Number)
  ; in: listă de filme movies
  ; out: pereche (rating-mediu-seen . rating-mediu-unseen)
  ;  - rating-mediu-seen = media rating-urilor filmelor văzute
  ;  - analog pentru unseen și filmele nevăzute
  ; (dacă nu există filme de un anumit fel, media este 0)
  ; RESTRICȚII
  ;  - Nu folosiți recursivitate explicită.
  ;  - Folosiți cel puțin o funcțională.
  ;  - Nu parcurgeți filmele din listă (sau părți ale listei)
  ;    mai mult decât o dată.
  (define (rating-stats movies)
    (compute (foldr (λ (x acc) 
         (if (member 'seen (movie-others x))
             (list  (cons (+ (movie-rating x) (car (car acc))) 
                         (add1 (cdr (car  acc))))
                    (car (cdr acc)))
             (list (car acc)
                   (cons (+ (movie-rating x) (car (car (cdr acc)))) 
                         (add1 (cdr (car (cdr acc))))))))
       (list (cons 0 0) (cons 0 0))
       movies)))

(define (compute acc)
  (cond ((and (equal? (car (car acc)) 0) (equal? (car (car (cdr acc))) 0)) (cons 0 0))
        ((equal? (car (car acc)) 0) (cons 0 (/ (car (car (cdr acc))) (cdr (car (cdr acc))))))
        ((equal? (car (car (cdr acc))) 0) (cons (/ (car (car acc)) (cdr (car acc))) 0))
        (else (cons (/ (car (car acc)) (cdr (car acc))) (/ (car (car (cdr acc))) (cdr (car (cdr acc))))))))
      
  ; TODO 8 (10p)
  ; extract-name-rating : [Movie] -> [(Symbol, Number)]
  ; in: listă de filme movies
  ; out: listă de perechi (nume . rating) 
  ;      (o pereche pentru fiecare film din movies)
  ; RESTRICȚII (10p):
  ;  - Nu folosiți recursivitate explicită.
  ;  - Folosiți cel puțin o funcțională.
  (define (extract-name-rating movies)
    (map (λ(x) (cons (movie-name x) (movie-rating x))) movies))


  ; TODO 9 (10p)
  ; make-rating-ph : [Movie] -> PH
  ; in: listă de filme movies
  ; out: max-PH care conține perechile (nume . rating)
  ;      corespunzătoare filmelor din movies
  ;      (cu ordonare după rating)
  ;  - se inserează ultima pereche în PH-ul vid
  ;  - ...
  ;  - se inserează prima pereche în PH-ul de până acum
  (define (make-rating-ph movies)
    (list->ph merge-max-rating (extract-name-rating movies)))


  ; TODO 10 (10p)
  ; before? : T1 x T2 x List
  ;           (List este o listă eterogenă)
  ; in: valori oarecare a, b, listă oarecare List
  ; out: true, dacă a = b sau a apare înaintea lui b în List
  ;      false, altfel
  ; RESTRICȚII (10p):
  ;  - Nu folosiți recursivitate explicită.
  ;  - Identificați în Help Desk funcționala findf
  ;    și folosiți-o.
  (define (before? a b L)
    (cond ((equal? a b) #t)
      ((and (not (findf (λ (x) (equal? b x)) L)) (not (findf (λ (x) (equal? a x)) L))) #f)
      ((not (findf (λ (x) (equal? b x)) L)) #t)
      ((not (findf (λ (x) (equal? a x)) L)) #f)
      (else (> (length (member a L)) (length (member b L))))))


  ; TODO 11 (10p)
  ; make-genre-ph : [Movie] x [Symbol] -> PH
  ; in: listă de filme movies, listă de genuri genres
  ; out: PH care conține filme, astfel încât genul
  ;      unui nod părinte să apară în lista genres
  ;      înaintea genului fiilor săi      
  ;      (conform definiției din funcția before?)
  ;  - se inserează ultimul film în PH-ul vid
  ;  - ...
  ;  - se inserează primul film în PH-ul de până acum
  ; observație: când se inserează un film de același
  ; gen cu root-ul curent, noul film devine fiul
  ; root-ului 
  (define (make-genre-ph movies genres)
    (list->ph (merge-genre genres) movies))

  