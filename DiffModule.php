<?php

class DiffModule {
    function diff($R, $C) {
        // see en.wikipedia.org/wiki/Longest_common_subsequence for algorithm
        
        // Each element in the table is a 2-array: [length, direction].
        $LENGTH = 0;
        $DIRECTION = 1;
        
        // Direction is:
        $UPLEFT = 0;  //  (keep)
        $LEFT   = 1;  //  (dec col)
        $UP     = 2;  //  (dec row)
        $BOTH   = 3;

        for ($i = 0; $i < count($R); $i++) {
            $table[$i][-1] = array(0, $UP);
        }
        for ($j = 0; $j < count($C); $j++) {
            $table[-1][$j] = array(0, $LEFT);
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
                        $table[$i][$j] = array($lenleft, $BOTH);
                    }
                }
            }
        }

        // Reconstruct.
        $i = count($R)-1;
        $j = count($C)-1;
        $rix = 0;
        while ($i != -1 || $j != -1) {
            $elem = $table[$i][$j];
            $dir = $elem[$DIRECTION];
            if ($dir == $UPLEFT) {
                $result[$rix++] = array(" ", $R[$i]); // == $C[$j]
                $i--; $j--;
            }
            else if ($dir == $LEFT) {
                $result[$rix++] = array("+", $C[$j]);
                $j--;
            }
            else if ($dir == $UP || $dir == $BOTH) {  // if both we just pick one I guess...
                $result[$rix++] = array("-", $R[$i]);
                $i--; 
            }
        }

        return $this->reverse($result);
    }

    function reverse($a) {
        for ($i = count($a)-1, $j=0; $i >= 0; $i--, $j++) {
            $b[$j] = $a[$i];
        }
        return $b;
    }

    // Override this function for different line compare modes; eg. whitespace-insensitive.
    function compare($x, $y) {
        return $x == $y;
    }

    function merge3($orig, $left, $right) {
        $ldiff = $this->diff($orig, $left);
        $rdiff = $this->diff($orig, $right);

        var_dump($ldiff);
        var_dump($rdiff);

        $oi = $li = $ri = $zi = 0;
        while ($oi < count($orig) || $li < count($ldiff) || $ri < count($rdiff)) {
            if ($li < count($ldiff)) {
                $lstat = $ldiff[$li][0];
                $ltext = $ldiff[$li][1];
            }
            else {
                $lstat = " ";
                $ltext = null;
            }

            if ($ri < count($rdiff)) {
                $rstat = $rdiff[$ri][0];
                $rtext = $rdiff[$ri][1];
            }
            else {
                $rstat = " ";
                $rtext = null;
            }

            echo("$lstat$ltext\n");
            echo("$rstat$rtext\n");
            echo("----\n");
            
            switch ($lstat.$rstat) {
                case "  ": 
                    $result[$zi] = array("orig", $orig[$oi]);
                    $zi++; $oi++; $li++; $ri++;
                    break;
                case " -":
                case "- ":
                case "--":
                    $oi++; $li++; $ri++;
                    break;
                case "+ ":
                    $result[$zi] = array("left", $ltext);
                    $zi++; $li++;
                    break;
                case " +":
                    $result[$zi] = array("right", $rtext);
                    $zi++; $ri++;
                    break;
                case "++":
                    if ($this->compare($ltext, $rtext)) {
                        $result[$zi] = array("both", $ltext);
                    }
                    else {
                        $result[$zi] = array("conflict", $ltext, $rtext);
                    }
                    $zi++; $li++; $ri++;
                    break;
                default:
                    throw new Exception("Missed something: [$lstat][$rstat]");
            }
        }
        return $result;
    }
}

?>
