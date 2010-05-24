<?php

function sum_up_to($number) {
    $result = 0;
    $i = 1;
    while ($i <= $number) {
        $i++;
        $result += $i;
    }
    return $result;
}

function sum_up_recursive($number) {
    return ($number + sum_up_recursive($number - 1));
}

sum_up_to(42);
sum_up_recursive(42);
?>
