<?php
$list = array();

for ($i = 0; $i < 1000000; $i++) {
    array_push($list, $i);
}

$sum = 0;
foreach ($list as $i) {
    $sum += $i;
}

echo $sum;
?>
