<?
$i=1;
$j=1;
$k=1;
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

function g(&$a,$b,$c)
{
    //$a=$a+$b;
    $a++;
}


//$d=1;

f($x,$y,$z);

//$e=1;

f($v,$w,$w);

//$f=1;

g($i,$j,$k);

trace($i,$u,$v,$x);
?>