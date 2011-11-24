<html>
<head>
<title>Final output</title>
</head>
<body>

<?php
include_once('DiffModule.php');
$diff = new DiffModule();
$merged = $diff->merge3(preg_split('/\r?\n/', $_POST['orig']), 
                        preg_split('/\r?\n/', $_POST['left']),
                        preg_split('/\r?\n/', $_POST['right']));
?>

<textarea rows="30" cols="50"><?php

for ($i = 0; $i < count($merged); $i++) {
    $type = $merged[$i][0];

    if ($type == "conflict") {
        switch ($_POST["line$i"]) {
            case 'left': 
                echo($merged[$i][1]."\n");
                break;
            case 'right':
                echo($merged[$i][2]."\n");
                break;
            case 'both';
                echo($merged[$i][1]."\n".$merged[$i][2]."\n");
                break;
            default:
                throw new Exception("Don't know how to resolve conflict with '".$_POST["line$i"]."'");
        }
    }
    else if ($type == 'orig' || $type == 'addleft' || $type == 'addright' || $type == 'addboth') {
        echo($merged[$i][1]."\n");
    }
}
?></textarea>
 
</body>
</html>
