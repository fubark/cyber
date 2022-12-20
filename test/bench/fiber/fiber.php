<?php
$count = 0;

$fibers = array();

for ($i = 0; $i < 100000; $i++) {
    $f = new Fiber(function () use (&$count) {
        $count += 1;
        Fiber::suspend();
        $count += 1;
    });
    $f->start();
    array_push($fibers, $f);
}

foreach ($fibers as $f) {
    $f->resume();
}
echo $count;
?>