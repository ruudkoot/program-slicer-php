<?php
$n = rand();
$i = 1;
while($i <= $n)
{
    if($i % 2 == 0)
        $x = 17;
    else
        $x = 18;
    $i = $i + 1;
}
out($x);
?>
