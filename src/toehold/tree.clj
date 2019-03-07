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

;; TODO this game state structure has implications for the `permute-move-states`
;; function's structure, so it seems weird for it to be removed from the
;; function. should I move this into the functions definition and simply remove
;; the 2-arity form?
(defn init-game-state
  "Generate a node representing a game state before any move have been made.

  We store both the played moves as well as the open positions to allow
  recursive calls on each node to decide how to move next, instead of having to
  calculate the available positions on each call."
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

;; CHALLENGE 2: Write a function to build a tree of all possible games. Explain
;; why or why not it uses content-zipper (above).
;;
;; CHALLENGE 2 ANSWER:
;;
;; This uses content-zipper primarily because content zipper is there and I
;; wanted to see what it would be like to use it. It also gave me an opportunity
;; to explore the version of the problem space where you're permuting all
;; possible move combinations. There's possibly a time savings from the way
;; zippers reconstruct the parent nodes only once you move back up to them, but
;; I'm not certain that applies here.
;;
;; We stop generating moves (and thus child nodes) for a particular branch once
;; a win state has been accomplished, as more moves after that would be
;; nonsensical.
;;
;; TODO: how would I test this without hand-coding something like 200K move
;; sets? What are the base cases you'd expect, and what are the extreme cases to
;; watch out for?
(defn permute-move-states
  "Generate all possible ordered movement sets as a tree, up to a given depth.

  This differs from all possible board-states, because a single board-state can
  be represented by many movement combinations, and thus takes greater time as
  well as space complexity.

  `depth` is defined as the number of moves made to reach a particular level,
  and an empty game board has a depth of zero."
  ([depth] (permute-move-states (content-zipper (init-game-state)) depth))
  ([location depth]
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
       (and (not (> (loc-depth location) depth))
            ;; You need at least 5 total moves for a winner, so only run that
            ;; calculation if it's even possible to have won
            ;;
            ;; Interestingly, without this check a 6-depth tree takes ~4.6
            ;; seconds to make (though it would be riddled with invalid game
            ;; states), and a depth-boxed check takes ~7.4 seconds, while a
            ;; non-depth-boxed check takes ~15 seconds.
            (or (< (loc-depth location) 4)
                (not (win? (-> location content :played)))))
       (recur (z/next
               (let [previous-player (-> location content :played last last)]
                 (reduce (fn [loc mv]
                           (cond-> loc
                             ;; If the last player was :o, or if no one has
                             ;; played yet, create a game state where :x has
                             ;; played the next move
                             (or (= previous-player :o)
                                 (= previous-player nil))
                             (append-move mv :x)

                             ;; Same deal here for :x as the previous player
                             (or (= previous-player :x)
                                 (= previous-player nil))
                             (append-move mv :o)))
                         location
                         (-> location content :open)))))

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
