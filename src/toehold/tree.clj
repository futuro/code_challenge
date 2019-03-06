(ns toehold.tree
  (:require [clojure.zip :as z]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [toehold.core :as c :refer :all]))


(defrecord node [content children])

;; Contentful-node zipper largely borrowed from
;; http://grokbase.com/p/gg/clojure/12ag6cjnch/how-to-represent-trees-for-use-with-zippers
(defn content-zipper
  "vector-zip and seq-zip assume that branch nodes don't have content. This
  version is like vector-zip, but assumes all nodes can have content."
  [root]
  (z/zipper (comp coll? :children)
            :children
            (fn [nd children]
              (assoc nd :children children))
            root))

(defn content [loc] (:content (z/node loc)))

(defn node-str
  "Return the attractively formatted contents of a node"
  [loc]
  (when-let [node (z/node loc)]
    (str "> " (:content node))))

(defn print-tree
  [loc]
  (when-not (z/end? loc)
    (do (when (z/branch? loc)
          (pprint (str (string/join "" (repeat (count (z/path loc)) " "))
                       (node-str loc))))
        (recur (z/next loc)))))

(defn node-path
  "Return the simple path of nodes up to and including this location, including
  the location"
  [loc]
  (conj (z/path loc) (z/node loc)))

;; Tree Generation

(defn init-game-state
  "Generate a node representing a board before any more have been made."
  []
  (node.
   {:played []
    :open   #{[0 0] [0 1] [0 2]
              [1 0] [1 1] [1 2]
              [2 0] [2 1] [2 2]}}
   []))

(defn append-move
  "Given a zipper location, a position to play, and a player to place there,
  create and append a new child node representing that player playing in that
  position.

  N.B. this doesn't do any move validation, so it's important that you only pass
  valid moves."
  [loc mv player]
  (z/append-child loc
                  (node.
                   (-> loc
                       content
                       (update :open disj mv)
                       (update :played conj (conj mv player)))
                   [])))

;; TODO is this necessary anymore?
;;
;; I originally wrote this when I was thinking about end-game board-states, but
;; since moving to a recursive DFS-esque algorithm it's not necessary anymore. I
;; may still want it for an end-game board-states implementation though...
(defn permute-moves
  [open-positions]
  (reduce (fn [mvs pos]
            (conj mvs
                  (conj pos :x)
                  (conj pos :o)))
          []
          open-positions))

(defn loc-depth
  "Return the depth of the tree at the current location. This is directly related
  to the number of moves played."
  [loc]
  (-> loc content :played count))

(defn permute-move-states
  "Generate all possible ordered movement sets as a tree, up to a given depth.

  This differs from all possible board-states, because a single board-state can
  be represented by many movement combinations, and thus takes greater time as
  well as space complexity.

  `depth` is defined as the number of moves made to reach a particular level,
  and an empty game board has a depth of zero."
  ([depth] (permute-move-states (content-zipper (init-game-state)) depth))
  ([location depth]
   ;; Using z/next would be depth-first generation, so I need to not generate
   ;; another move if the board is full or if the moves so far represent a win.
   ;; If I want to generate board states, then `full-or-win?` will complicate
   ;; things because it takes lists of moves.
   ;;
   ;; On the other hand, if I'm generating moves, then we could track open
   ;; positions and played moves, which would avoid needing to generate open
   ;; positions every time we make another node, We start with an empty vec of
   ;; played positions and the 9 coords for every position available, then
   ;; iterate through them to generate new children, and finally recurse with
   ;; `z/next` to handle the next depth.
   ;;
   ;;I need to return the root of the
   (loop [location location]
     (cond

       ;; Have we looked at every node that exists in the tree via z/next?
       ;; Because of the way that zippers are constructed, we can add nodes to
       ;; the tree and `z/next` will happily walk into them. So, if `z/next`
       ;; says we've reached the "end" then that means there are no more levels
       ;; to generate and we've made as much of the tree as was asked for. So
       ;; return the current location.
       (z/end? location)
       location

       ;; If we haven't generated up to the request depth of levels, keep adding
       ;; nodes to the tree.
       ;;
       ;; A depth is defined by the number of moves necessary to get
       ;; there, with the root having a depth of 0.
       ;;
       ;; TODO add a check to not generate more moves if the game is won.
       (and (not (> (loc-depth location) depth)))
       (recur (z/next
               (reduce (fn [loc mv]
                         (-> loc
                             (append-move mv :x)
                             (append-move mv :o)))
                       location
                       (-> location content :open))))

       ;; If we're not at the end of the nodes, but we *are* at the `depth`
       ;; limit, then we just want to go look at the next node in the tree to
       ;; see if it needs more levels generated.
       :else
       (recur (z/next location))))))

;; CHALLENGE 2: Write a function to build a tree of all possible games. Explain
;; why or why not it uses content-zipper (above).

;; CHALLENGE 3: Is it possible to rewrite build-tree so that it's significantly
;; more efficient in time and/or space? If so, what strategies do you see for
;; that? Implement one.

;; CHALLENGE 4: write code to answer some of the following questions:
;; 1. What percentage of 100000 random games have no win?
;; 2. Given a partial game in a particular state, which player if any has
;;   a guaranteed win if they play optimally?
;; 3. Under what conditions is player 2 (O) guaranteed a win?
;; 4. Can X get a win if they blow 1st move?
