(require "./partcont")
(import kahua.partcont)

(define (dmap2 ls)
  (let/pc k (map k ls)))

(reset/pc (+ 1 (dmap2 '(1 2 3))))
;==> (2 3 4)
(reset/pc (+ 1 (dmap2 (dmap2 '((1 2) (2 3) (3 4))))))
;==> ((2 3) (3 4) (4 5))


(define (dmap . lss)
  (let/pc k (apply (map$ (lambda args (k args))) lss)))

(reset/pc ((apply$ +) (dmap '(1 2 3) '(3 4 5))))
;==> (4 6 8)
(reset/pc ((apply$ (pa$ + 1)) (dmap '(1 2 3) '(3 4 5))))
;==> (5 7 9)
