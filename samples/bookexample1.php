<?php
$n = rand();
$i = 0;
$sum = 0;
$product = 1;

while($i <= $n)
{
    $sum = $sum + $i;
    $product = $product * $i;
    $i++;
}

echo $sum;
echo $product;
?>
