<?
$x=1;
$y=1;
$z=1;
$u=1;

function f(&$a,$b,$c)
{
    $a=$a+$b;
}

f($x,$y,$z);

trace($u,$x);
?>
