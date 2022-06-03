;variable declarations
(declare-const A1 Bool)
(declare-const B1 Bool)
(declare-const C1 Bool)
(declare-const D1 Bool)

(declare-const A2 Bool)
(declare-const B2 Bool)
(declare-const C2 Bool)
(declare-const D2 Bool)

(declare-const A3 Bool)
(declare-const B3 Bool)
(declare-const C3 Bool)
(declare-const D3 Bool)

(declare-const A4 Bool)
(declare-const B4 Bool)
(declare-const C4 Bool)
(declare-const D4 Bool)

(declare-const A5 Bool)
(declare-const B5 Bool)
(declare-const C5 Bool)
(declare-const D5 Bool)

(assert(or A1 B1 C1 D1))

(assert(or A5 B5 C5 D5))

(assert(=> A1 (and (not A2)(not A3)(not A4))))

;for time 1
(assert(=> A1 B2))
(assert(or (=> B1 D2) (=> B1 C2)))
(assert(=> C1 A2))
(assert(or(=> D1 C2)(=> D1 B2)))

;for time 2
(assert(=> A2 B3))
(assert(or (=> B2 D3) (=> B2 C3)))
(assert(=> C2 A3))
(assert(or(=> D2 C3)(=> D2 B3)))

;for time 3
(assert(=> A3 B4))
(assert(or (=> B3 D4) (=> B3 C4)))
(assert(=> C3 A4))
(assert(or(=> D3 C4)(=> D3 B4)))

;for time 4
(assert(=> A4 B5))
(assert(or (=> B4 D5) (=> B4 C5)))
(assert(=> C4 A5))
(assert(or(=> D4 C5)(=> D4 B5)))

;for time 5
(assert(=> A5 B1))
(assert(or (=> B5 D1) (=> B5 C1)))
(assert(=> C5 A1))
(assert(or(=> D5 C1)(=> D5 B1)))

(check-sat)
(get-model)

(exit)
