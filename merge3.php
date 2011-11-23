<html>
<head>
<title>Merged output</title>
<style>
pre { margin: 0 }

pre.left.addleft  { background: #bbffbb }
pre.left.addright { color: white }
pre.left.addboth  { background: #bbffbb }
pre.left.delleft  { background: #ffbbbb }
pre.left.delboth  { background: #ffbbbb }

pre.right.addleft  { color: white }
pre.right.addright { background: #bbffbb }
pre.right.addboth  { background: #bbffbb }
pre.right.delright { background: #ffbbbb }
pre.right.delboth  { background: #ffbbbb }

pre.orig.delleft  { color: white }
pre.orig.delright { color: white }
pre.orig.delboth  { color: white }

pre.conflict      { background: #ff8888 }
</style>
</head>
<body>

<?php
include_once('DiffModule.php');
$diff = new DiffModule();
$merged = $diff->merge3(preg_split('/\r?\n/', $_POST['orig']), 
                        preg_split('/\r?\n/', $_POST['left']),
                        preg_split('/\r?\n/', $_POST['right']));
?>

<table><tr><td><?php

for ($i = 0; $i < count($merged); $i++) {
    $type = $merged[$i][0];
    echo("<pre class=\"left $type\">".$merged[$i][1]."</pre>");
}

?></td>
<td><?php

for ($i = 0; $i < count($merged); $i++) {
    $type = $merged[$i][0];
    echo("<pre class=\"orig $type\">".$merged[$i][1]."</pre>");
}

?></td>
<td><?php

for ($i = 0; $i < count($merged); $i++) {
    $type = $merged[$i][0];
    if ($type == 'conflict') {
        echo("<pre class=\"right $type\">".$merged[$i][2]."</pre>");
    }
    else {
        echo("<pre class=\"right $type\">".$merged[$i][1]."</pre>");
    }
}

?></td></tr>
</table>
 
</body>
</html>
