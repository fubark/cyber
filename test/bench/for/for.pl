@list = ();

for (my $i = 0; $i < 1000000; $i++) {
    push(@list, $i);
}

$sum = 0;
for my $i (@list) {
    $sum += $i;
}

print($sum);
