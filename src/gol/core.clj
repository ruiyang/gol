(ns gol.core)

(defn get-all-cells-to-check [cells]
  (->> (map (fn [[x y]]
              (for [step_x [-1 0 1]
                    step_y [-1 0 1]]
                [(+ x step_x) (+ y step_y)]))
            cells)
       (apply concat)
       distinct))

(defn- get-neighbours [[x y]]
  (filter #(not (nil? %))
          (for [step_x [-1 0 1]
                step_y [-1 0 1]]
            (if (= step_x step_y 0)
              nil
              [(+ x step_x) (+ y step_y)]))))

(defn get-live-neighbour-size [[x y] state-matrix]
  (let [neighbours (set (get-neighbours [x y]))
        state-matrix (set state-matrix)
        live-neighbours (clojure.set/intersection neighbours state-matrix)]
    (count live-neighbours)))


(defn calculate-cell-next-state [live? live-neighbour-size]
  (if live?
    (cond
      (< live-neighbour-size 2) :die
      (= live-neighbour-size 2) :live
      (= live-neighbour-size 3) :live
      (> live-neighbour-size 3) :die)
    (cond
      (= live-neighbour-size 3) :live
      :else :die)))

(defn is-cell-alive? [cell matrix]
  (not= (some #(= cell %) matrix)
        nil))

(defn gol-next-state [matrix]
  (let [cells-next-state (map (fn [cell]
                                (let [live? (is-cell-alive? cell matrix)
                                      next-state (calculate-cell-next-state live? (get-live-neighbour-size cell matrix))]
                                  (if (= next-state :live)
                                    cell
                                    nil)))
                              (get-all-cells-to-check matrix))]
    (filter #(not= nil %) cells-next-state)))


(def initial-matrix [[5, 5], [6, 5], [7, 5], [5, 6], [6, 6], [7, 6]])

(sort (gol-next-state initial-matrix))
