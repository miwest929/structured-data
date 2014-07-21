(ns structured-data)

(defn do-a-thing [x]
  (let [twox (+ x x)]
    (Math/pow twox twox)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[one two three] v]
    (+ one three)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn abs [n] (max n (- n)))

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (abs (- x2 x1))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (abs (- y1 y2))))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (= (- x2 x1) (- y2 y1))))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [xp yp] point]
    (cond
      (or (> yp y2) (< yp y1)) false
      (or (> xp x2) (< xp x1)) false
      :else true)))

(defn contains-rectangle? [outer inner]
  (let [[[outerLeft outerBottom] [outerRight outerTop]] outer [[innerLeft innerBottom] [innerRight innerTop]] inner]
    (cond
      (> outerLeft innerLeft) false
      (< outerRight innerRight) false
      (> outerBottom innerBottom) false
      (< outerTop innerTop) false
      :else true)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (count (get book :authors)) 1) true false))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [v] (get v 1)) collection))

(defn titles [books]
  (map (fn [b] (get b :title)) books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map (fn [b] (set (get b :authors))) books)))

(defn all-author-names [books]
  (let [author-names (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

(defn author->string [author]
  (let [years (fn [a] (if (contains? a :birth-year) (conj [] " (" (get a :birth-year) " - " (get a :death-year) ")") ""))]
  (apply str (conj [] (get author :name) (apply str (years author))))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (interpose ", written by " (conj [] (get book :title) (authors->string (get book :authors))))))

(defn books->string [books]
  (let [book-cnt (fn [b] (if (= 0 (count b)) "No books." (if (= 1 (count b)) "1 book. " (apply str (conj [] (str (count b)) " books. ")))))
        book-sentence (fn [b] (str (book->string b) \.))
  ]
  (apply str (conj [] (book-cnt books) (apply str (interpose ". " (map book-sentence books))))
  )))

(defn books-by-author [author books]
 (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= name (get a :name))) authors)))

(defn living-authors [authors]
  (filter (fn [a] (alive? a)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
