<?php

class DiffModule {
    function lcs($R, $C) {
        // see en.wikipedia.org/wiki/Longest_common_subsequence for algorithm
        
        // Each element in the table is a 2-array: [length, direction].
        $LENGTH = 0;
        $DIRECTION = 1;
        
        // Direction is:
        $UPLEFT = 0;  //  (keep)
        $LEFT   = 1;  //  (dec col)
        $UP     = 2;  //  (dec row)
        $BOTH   = 3;

        // We do not need to fill in direction for the -1 entries
        // because direction is only used during reconstruction,
        // which will terminate at index 0.
        for ($i = 0; $i < count($R); $i++) {
            $table[$i][-1][$LENGTH] = 0;
        }
        for ($j = 0; $j < count($C); $j++) {
            $table[-1][$j][$LENGTH] = 0;
        }
        $table[-1][-1] = array(0,0);

        // Fill in the table.
        for ($i = 0; $i < count($R); $i++) {
            for ($j = 0; $j < count($C); $j++) {
                if ($this->compare($R[$i], $C[$j])) {
                    $table[$i][$j] = array(1 + $table[$i-1][$j-1][$LENGTH], $UPLEFT);
                }
                else {
                    $lenleft = $table[$i][$j-1][$LENGTH];
                    $lenup   = $table[$i-1][$j][$LENGTH];
                    if ($lenleft > $lenup) {
                        $table[$i][$j] = array($lenleft, $LEFT);
                    }
                    else if ($lenup > $lenleft) {
                        $table[$i][$j] = array($lenup, $UP);
                    }
                    else {
                        $table[$i][$j] = array($lenleft, $UPLEFT);
                    }
                }
            }
        }

        // Reconstruct.
        $i = count($R)-1;
        $j = count($C)-1;
        $rix = $table[$i-1][$j-1][$LENGTH];
        while ($i != -1 && $j != -1) {
            $elem = $table[$i][$j];
            $dir = $elem[$DIRECTION];
            if ($dir == $UPLEFT) {
                $result[$rix] = $R[$i]; // == $C[$j]
                $i--; $j--; $rix--;
            }
            else if ($dir == $LEFT) {
                $j--;
            }
            else if ($dir == $UP) {
                $i--;
            }
            else if ($dir == $BOTH) {
                // uhhh... pick one I guess.  We could also do some callback bullshit
                // to return all results, but we'll se if we need that.
                $i--;
            }
        }

        return $result;
    }

    function compare($x, $y) {
        return $x == $y;
    }
}

$mod = new DiffModule();
var_dump($mod->lcs(array("S", "p", "o", "o", "k"), array("S", "o", "o", "l", "k")));

?>
