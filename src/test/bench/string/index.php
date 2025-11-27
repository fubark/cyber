<?php

$str = str_repeat('abcdefghijklmnopqrstuvwxyz123456', 1000000) . 'waldo';

$start = microtime(true);

for ($i = 0; $i < 50; $i++) {
    $idx = strpos($str, 'waldo');
}

$dur = (microtime(true) - $start) * 1000;
echo "idx: {$idx} ms: {$dur}\n";
?>
