<?
$x=1;
$y=1;
$z=1;
$u=1;
$v=1;
$w=1;

function f(&$a,$b,$c)
{
    $a=$a+$b;
}

//$d=1;

f($x,$y,$z);

//$e=1;

f($v,$w,$w);

//$f=1;

trace($u,$v,$x);
?>
