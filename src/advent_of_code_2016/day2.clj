(ns advent-of-code-2016.day2
  (:import (java.io BufferedReader StringReader)))


(def grid [[1 4 7] [2 5 8] [3 6 9]])

(def sample-puzzle "ULL
RRDDD
LURDL
UUUUD")

(def puzzle "UULLULLUULLLURDLDUURRDRRLDURDULLRURDUDULLLUULURURLRDRRRRULDRUULLLLUUDURDULDRRDRUDLRRLDLUDLDDRURURUURRRDDDLLRUDURDULUULLRRULLRULDUDRDRLDLURURUDDUDLURUDUDURLURURRURLUDDRURRDLUURLLRURRDUDLULULUDULDLLRRRDLRDLDUDRDDDRRUURRRRRUURRDRRDLURDRRURDLLUULULLRURDLDDDRRLLRRUURULURUUDDLRRUDDRURUUDLRLRDLRURRRDULLDLRUDDUULRDULURUURDULUDLLRRLDDLRDLRUDRLDDRLRRRDURDULLRRRDRRLUURURDRRDRRLDLUDURURLDUURDRUDRDDRLDRRLDLURURULLUURUDUUDLRLL
LLLULLULDDULRLLURLLLRUUDDLRUULRLULLDLLRRDRLRLRLLDRUUURULDRDDLUDLLDUDULLLRLULLLRULDRDRUDLLRLRLLUDULRRRLDRUULDDULLDULULLUDUDLDRDURDLDLLDUDRRRDLUURRUURULLURLDURLRRLLDDUUULDRLUUDUDLURLULUDURRDRLLDDDDDRRULLRLDULULDDRUURRDLUDDDUDURDDRDRULULLLLUURDURUUUULUDLRURRULRDDRURURLLRLUUDUUURDLLDDLUDRLLLUDLLLLULRLURDRRRDUUDLLDLDDDURRDDRURUURDDRURRLDDDURDLLUURUUULRLUURRUDRLLDLURDUDRLULDLRLULULUDDLRDUDRUDLUULUULDURDRRRRLRULLUDRDDRDLDUDRDRRLDLLLLUDDLRULDLLDDUULDDRRULRRUURUDRDURLLLDDUUDRUUDLULLDR
UDUUULLDDDDLUDLDULRLRDLULLDDRULDURRLURRUDLRRUDURRDUDRRRUULRLLRLUDLDRRDUURDDRDRDUUUDUDLDLLRRLUURLUUUDDDUURLULURRLURRRDRDURURUDRLRUURUDRUDDDRDRDLDRDURDLDRRDUUDLLURLDDURRRLULDRDRLLRLLLRURLDURDRLDRUURRLDLDRLDDDRLDLRLDURURLLLLDDRDUDLRULULLRDDLLUDRDRRLUUULDRLDURURDUDURLLDRRDUULDUUDLLDDRUUULRRULDDUDRDRLRULUUDUURULLDLLURLRRLDDDLLDRRDDRLDDLURRUDURULUDLLLDUDDLDLDLRUDUDRDUDDLDDLDULURDDUDRRUUURLDUURULLRLULUURLLLLDUUDURUUDUULULDRULRLRDULDLLURDLRUUUDDURLLLLDUDRLUUDUDRRURURRDRDDRULDLRLURDLLRRDRUUUURLDRURDUUDLDURUDDLRDDDDURRLRLUDRRDDURDDRLDDLLRR
ULDRUDURUDULLUDUDURLDLLRRULRRULRUDLULLLDRULLDURUULDDURDUUDLRDRUDUDDLDRDLUULRRDLRUULULUUUDUUDDRDRLLULLRRDLRRLUDRLULLUUUUURRDURLLRURRULLLRLURRULRDUURRLDDRRDRLULDDRRDRLULLRDLRRURUDURULRLUDRUDLUDDDUDUDDUDLLRDLLDRURULUDRLRRULRDDDDDRLDLRRLUUDLUURRDURRDLDLDUDRLULLULRLDRDUDLRULLULLRLDDRURLLLRLDDDLLLRURDDDLLUDLDLRLUULLLRULDRRDUDLRRDDULRLLDUURLLLLLDRULDRLLLUURDURRULURLDDLRRUDULUURRLULRDRDDLULULRRURLDLRRRUDURURDURDULURULLRLDD
DURLRRRDRULDLULUDULUURURRLULUDLURURDDURULLRRUUDLRURLDLRUDULDLLRRULLLLRRLRUULDLDLLRDUDLLRLULRLLUUULULRDLDLRRURLUDDRRLUUDDRRUDDRRURLRRULLDDULLLURRULUDLRRRURRULRLLLRULLRRURDRLURULLDULRLLLULLRLRLLLDRRRRDDDDDDULUUDUDULRURDRUDRLUULURDURLURRDRRRRDRRLLLLUDLRRDURURLLULUDDLRLRLRRUURLLURLDUULLRRDURRULRULURLLLRLUURRULLLURDDDRURDUDDULLRULUUUDDRURUUDUURURRDRURDUDRLLRRULURUDLDURLDLRRRRLLUURRLULDDDUUUURUULDLDRLDUDULDRRULDRDULURRUURDU")

(defn parse-input [s]
  (line-seq (BufferedReader. (StringReader. s))))

(defn move [[v x y] m]
  (letfn [(new-state [mx my]
            (if-let [nv (get-in grid [(mx x) (my y)])]
              [(conj v nv) (mx x) (my y)]
              [v x y]))]
    (case m
      \U (new-state identity dec)
      \D (new-state identity inc)
      \L (new-state dec identity)
      \R (new-state inc identity))))

(defn one-digit [[ds _ _ :as s] ms]
  (let [r (reduce move s (seq ms))
        newdigits (conj ds (last (get r 0)))]
    (update-in r [0] (constantly newdigits))))


(defn day2 [s]
  (reduce one-digit [[] 1 1]  (parse-input s)))

(comment (day2 puzzle))

