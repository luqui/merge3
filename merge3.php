<html>
<head>
<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.7.0/jquery.min.js"></script>
<title>Merged output</title>
<style>
pre { margin: 0; width: 50ex }

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
pre.orig.conflict { background: #ff8888; color: #ff8888 }

pre.conflict      { background: #ff8888 }
pre.orig.resolved { background: #ff8888; color: black }

td { padding: 0; vertical-align: top }

.hidden { display: none }
</style>

<script>
$(function() {
    $('.conflictrow').each(function(ix,row) {
        row = $(row);
        var middle = row.find('pre.orig');
        var resolve = function() { middle.removeClass('conflict'); middle.addClass('resolved'); };
        
        row.find('input.leftoption').click(function() {
            resolve();
            middle.text(row.find('pre.left').text());
        });
        row.find('input.bothoption').click(function() {
            resolve();
            middle.text(row.find('pre.left').text() + "\n" + row.find('pre.right').text());
        });
        row.find('input.rightoption').click(function() {
            resolve();
            middle.text(row.find('pre.right').text());
        });
    });
});
</script>

</head>
<body>

<?php
include_once('DiffModule.php');
$diff = new DiffModule();
$merged = $diff->merge3(preg_split('/\r?\n/', $_POST['orig']), 
                        preg_split('/\r?\n/', $_POST['left']),
                        preg_split('/\r?\n/', $_POST['right']));
?>

<form action="resolve.php" method="post">
<table><?php

for ($i = 0; $i < count($merged); $i++) {
    $type = $merged[$i][0];
    if ($type == "conflict") {
        echo("<tr class=\"conflictrow\">");
    }
    else {
        echo("<tr>");
    }
    
    echo("<td><pre class=\"left $type\">".htmlspecialchars($merged[$i][1])."</pre></td>");
    
    echo("<td><pre class=\"orig $type\">".htmlspecialchars($merged[$i][1])."</pre>");
    if ($type == 'conflict') {
        echo("<input class=\"leftoption\" type=\"radio\" name=\"line$i\" value=\"left\" />&larr; ");
        echo("<input class=\"bothoption\"  type=\"radio\" name=\"line$i\" value=\"both\" />&harr; ");
        echo("<input class=\"rightoption\" type=\"radio\" name=\"line$i\" value=\"right\" />&rarr; ");
    }
    echo("</td>");

    $rline = ($type == 'conflict') ? $merged[$i][2] : $merged[$i][1];
    
    echo("<td><pre class=\"right $type\">".htmlspecialchars($rline)."</pre></td>");
    echo("</tr>");
}
?></table>
<textarea class="hidden" name="left"><?php echo htmlspecialchars($_POST['left']) ?></textarea>
<textarea class="hidden" name="orig"><?php echo htmlspecialchars($_POST['orig']) ?></textarea>
<textarea class="hidden" name="right"><?php echo htmlspecialchars($_POST['right']) ?></textarea>
<input type="submit" />
</form>
 
</body>
</html>
