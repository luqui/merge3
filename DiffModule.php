<?php

class DiffModule {
    function dump_table($table, $R, $C) {
        echo('<table><tr>');
        echo('<td></td>');
        for ($j = 0; $j < count($C); $j++) {
            echo ('<td>'.$C[$j].'</td>');
        }
        echo ('</tr>');
        
        for ($i = 0; $i < count($R); $i++) {
            echo('<tr>');
            echo('<td>'.$R[$i].'</td>');
            for ($j = 0; $j < count($C); $j++) {
                $len = $table[$i][$j][0];
                $dlen = $table[$i][$j][1];
                $dir = $table[$i][$j][2];
                if ($dir == 0) { $dirch = '&#8598;'; }
                else if ($dir == 1) { $dirch = '&#8592;'; }
                else if ($dir == 2) { $dirch = '&#8593;'; }
                echo("<td>($len, $dlen, $dirch)</td>");
            }
            echo ('</tr>');
        }
        echo('</table>');
    }

    function diff($R, $C) {
        // see en.wikipedia.org/wiki/Longest_common_subsequence for algorithm
        
        // Each element in the table is a 3-array: [length, difflength, direction].
        $LENGTH = 0;
        $DIFFLENGTH = 1;
        $DIRECTION = 2;
        
        // Direction is:
        $UPLEFT = 0;  //  (keep)
        $LEFT   = 1;  //  (dec col)
        $UP     = 2;  //  (dec row)

        for ($i = 0; $i < count($R); $i++) {
            $table[$i][-1] = array(0, $i+1, $UP);
        }
        for ($j = 0; $j < count($C); $j++) {
            $table[-1][$j] = array(0, $i+1, $LEFT);
        }
        $table[-1][-1] = array(0,0,0);

        // Fill in the table.
        for ($i = 0; $i < count($R); $i++) {
            for ($j = 0; $j < count($C); $j++) {
                if ($this->compare($R[$i], $C[$j])) {
                    $table[$i][$j] = array(1 + $table[$i-1][$j-1][$LENGTH], $table[$i-1][$j-1][$DIFFLENGTH], $UPLEFT);
                }
                else {
                    $left = $table[$i][$j-1];
                    $up   = $table[$i-1][$j];
                    if ($left[$LENGTH] > $up[$LENGTH]) {
                        $table[$i][$j] = array($left[$LENGTH], $left[$DIFFLENGTH]+1, $LEFT);
                    }
                    else if ($up[$LENGTH] > $left[$LENGTH]) {
                        $table[$i][$j] = array($up[$LENGTH], $up[$DIFFLENGTH]+1, $UP);
                    }
                    else if ($left[$DIFFLENGTH] < $up[$DIFFLENGTH]) {
                        $table[$i][$j] = array($left[$LENGTH], $left[$DIFFLENGTH]+1, $LEFT);
                    }
                    else if ($up[$DIFFLENGTH] < $left[$DIFFLENGTH]) {
                        $table[$i][$j] = array($up[$LENGTH], $up[$DIFFLENGTH]+1, $UP);
                    }
                    else {
                        // just pick up, I guess
                        $table[$i][$j] = array($up[$LENGTH], $up[$DIFFLENGTH]+1, $UP);
                    }
                }
            }
        }

        // $this->dump_table($table, $R, $C);

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
            else if ($dir == $UP) {
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

            switch ($lstat.$rstat) {
                case "  ": 
                    $result[$zi] = array("orig", $orig[$oi]);
                    $zi++; $oi++; $li++; $ri++;
                    break;
                case " -":
                    $result[$zi] = array("delright", $rtext);
                    $zi++; $oi++; $li++; $ri++;
                    break;
                case "- ":
                    $result[$zi] = array("delleft", $ltext);
                    $zi++; $oi++; $li++; $ri++;
                    break;
                case "--":
                    $result[$zi] = array("delboth", $ltext);
                    $zi++; $oi++; $li++; $ri++;
                    break;
                case "+-":
                    $result[$zi] = array("delright", $rtext);
                    $zi++; $oi++; $ri++;
                    break;
                case "-+":
                    $result[$zi] = array("delleft", $ltext);
                    $zi++; $oi++; $li++;
                    break;
                case "+ ":
                    $result[$zi] = array("addleft", $ltext);
                    $zi++; $li++;
                    break;
                case " +":
                    $result[$zi] = array("addright", $rtext);
                    $zi++; $ri++;
                    break;
                case "++":
                    if ($this->compare($ltext, $rtext)) {
                        $result[$zi] = array("addboth", $ltext);
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
