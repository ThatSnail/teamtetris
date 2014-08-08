<?php
    $playerName = $_POST["playerName"];

    // Find available game
    mysql_connect("localhost", "admin", "pass") or die(mysql_error());
    
?>
