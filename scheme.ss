;;;Scheme Samples
;;;Steven Warner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Converts inches into centimeters
(define (in->cm cm)
  (* 2.54 cm))
  
;;;Converts centimeters into inches
(define (cm->in in)
  (/ in 2.54))

;;;Converts kilograms into pounds
(define (kg->lb lb)
  (/ lb 0.45359237))

;;;Converts pounds into kilograms
(define (lb->kg kg)
  (* 0.45359237 kg))

;;;Evalutes your BMI based on hight in cm and wieght in kg
(define (BMI-Metric cm kg)
  (let ((x (/ kg (expt (* .01 cm) 2)) ))
  (cond ((< x 18.5) (list x 'underweight))
        ((< x 25.0) (list x 'normal))
        ((< x 30.0) (list x 'overweight))
        ((>= x 30.0) (list x 'obese))
)))


;;;Evalutes your BMI based on hight in inches and wieght in pounds
(define (BMI-American in lb)
  (let ((x (/ (lb->kg lb) (expt (* .01 (in->cm in)) 2))))
  (cond ((< x 18.5) (list x 'underweight))
        ((< x 25.0) (list x 'normal))
        ((< x 30.0) (list x 'overweight))
        ((>= x 30.0) (list x 'obese))
)))

;;;Converts a list of roman numeral to numbers
(define ROMAN '((M 1000)
                 (D 500)
                 (C 100)
                 (L 50)
                 (X 10)
                 (V 5)
                 (I 1)))

(define (value numeral)
  (cadr (assoc numeral ROMAN)))

(define (roman->num roman)
  (if (null? roman)
      0
      (+ (value (car roman)) (roman->num (cdr roman)))))

(define (roman->num2 num)
  (apply + (map value num)))

;;;Coverts a number to a list of roman numerals
(define NUM '((1000 M)
                 (500 D)
                 (100 C)
                 (50 L)
                 (10 X)
                 (5 V)
                 (1 I)))

(define (value2 numeral)
  (cadr (assoc numeral NUM)))

(define (num->roman num)
  (if (null? num)
      0
      (cond ((>= num 10)
          (num->roman (- num 1000)
                      (string-append s (car ROMAN))
                      value ROMAN))
          (num->roman num s
                      (cdr value) (cdr ROMAN)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Association list of condons and the abbreviation for the corresponding amino acid
(define ACIDS
    '( ((U U U) F) ((U U C) F) ((U U A) L) ((U U G) L)
       ((U C U) S) ((U C C) S) ((U C A) S) ((U C G) S)
       ((U A U) Y) ((U A C) Y) ((U A A) *) ((U A G) *)
       ((U G U) C) ((U G C) C) ((U G A) *) ((U G G) W)
       ((C U U) L) ((C U C) L) ((C U A) L) ((C U G) L)
       ((C C U) P) ((C C C) P) ((C C A) P) ((C C G) P)
       ((C A U) H) ((C A C) H) ((C A A) Q) ((C A G) Q)
       ((C G U) R) ((C G C) R) ((C G A) R) ((C G G) R)
       ((A U U) I) ((A U C) I) ((A U A) I) ((A U G) M)
       ((A C U) T) ((A C C) T) ((A C A) T) ((A C G) T)
       ((A A U) N) ((A A C) N) ((A A A) K) ((A A G) K)
       ((A G U) S) ((A G C) S) ((A G A) R) ((A G G) R)
       ((G U U) V) ((G U C) V) ((G U A) V) ((G U G) V)
       ((G C U) A) ((G C C) A) ((G C A) A) ((G C G) A)
       ((G A U) N) ((G A C) N) ((G A A) E) ((G A G) E)
       ((G G U) G) ((G G C) G) ((G G A) G) ((G G G) G) ))

;;;Takes in a codon and returns the abbreviation for the corresponding amino acid
(define (translate codon)
  (cadr (assoc codon ACIDS)))

;;;Takes in list of sequence bases and returns list of amino acid abbreviations
(define (translate-sequence codon)
  (if (> 3 (length codon))
      '()
     (append (list (translate (list (car codon) (cadr codon) (caddr codon))) )
                    (translate-sequence (cdddr codon)))))

;;;Returns all six possible translations of a sequence
(define (translate-all-possible codon)
      (list (translate-sequence codon) 
            (translate-sequence (cdr codon))
            (translate-sequence (cddr codon))
            (translate-sequence (reverse codon))
            (translate-sequence (cdr (reverse codon)))
            (translate-sequence (cddr (reverse codon)))
            ))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ANIMALS 
    '(dog
      (bird (horse () ()) (cat () ()))
      (possum (dog () ()) ())))

(define (empty-tree? tree)
  (null? tree))

(define (root tree)
  (if (empty-tree? tree)
      'ERROR
      (car tree)))

(define (left-subtree tree)
  (if (empty-tree? tree)
      'ERROR
      (cadr tree)))

(define (right-subtree tree)
  (if (empty-tree? tree)
      'ERROR
      (caddr tree)))

;;;Reads in a list representing a binary tree and calculates the height
(define (height tree)
  (if (empty-tree? tree)
      0
      (max (length(right-subtree tree))
           (length(left-subtree tree)))))

;;;Calculates if a specified tree contains a specifc atom
(define (contains? tree atom)
  (cond ((empty-tree? tree) #f)
        ((equal? (root tree) atom) #t)
        ((contains? (left-subtree tree) atom))
        (else (contains? (right-subtree tree) atom))))

;;;Returns a count of the number of times a specified atom occurs in a tree
(define (num-occur tree atom)
  (let ((number 0))
    (define (num-helper tree atom)
      (cond ((empty-tree? tree) 0)
            ((equal? (root tree) atom) (set! number (+ number 1))))
      (cond ((empty-tree? tree) 0)
            ((num-helper (left-subtree tree) atom)))
      (cond ((empty-tree? tree) 0)
            ((num-helper (right-subtree tree) atom)))number)
    (num-helper tree atom)
    number))
