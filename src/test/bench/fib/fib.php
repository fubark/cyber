<?php  
// jit: `php -d opcache.enable_cli=1 test/bench/fib/fib.php`
$start = hrtime(true);
function fib($n){
    if ($n < 2) {
        return $n;    
    }
    return fib($n-1) + fib($n-2);
}
$res = fib(30);
$time = (hrtime(true) - $start) / 1000000;
echo "time: $time";
echo $res;
?>
