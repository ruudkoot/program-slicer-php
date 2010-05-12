<?php
function sum_up_to($number) {
    $result = 0;
    for ($i = 1; $i <= $number; $i++) {
        $result += $i;
    }
    return $result;
}

function sum_up_recursive($number) {
    return ($number) ? ($number + sum_up_recursive($number - 1)) : 0;
}

echo '<p>Regular version = ', sum_up_to(42), "</p>\n";
echo '<p>Recursive version = ', sum_up_recursive(42), "</p>\n";
?>
