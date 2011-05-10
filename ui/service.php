<?php

$url = $_GET["url"];
$handle = fopen($url, "rb");
$content = stream_get_contents($handle);
fclose($handle);

echo $content;

?>
