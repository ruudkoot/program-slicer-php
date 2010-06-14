<?

/*
function abc($a,$b,$c)
{
    $d = $b*$b-4.0*$a*$c;
    $dsq = sqrt($d);
    if($d == 0.0)
        $x0 = $x1 = -$b / 2*$a;
    else if($d < 0.0)
        $x0 = $x1 = 0.0;
    else
    {
        $x0 = (-$b - $dsq)/2*$a;
        $x1 = (-$b + $dsq)/2*$a;            
    }
    
    echo $x0, $x1;
}*/

//abc(1.0,2.0,4.0);

function powerRec($base,$exp)
{
    if($exp == 0)
        return 1;
    else 
        return powerRec($base,$exp-1);
}

/*
function powerFor($base,$exp)
{
    $ret = 1;
    for($i=0; $i < $exp; $i++)
        $ret *= $exp;
    return $ret;
}

$a = powerFor(4,5);*/
$v1=4;
$v2=5;
$b = powerRec($v1,$v2);

/*
function retTest($a)
{
    if($a==0)
        return 5;
    else
        return 4;
}

retTest($a);
*/

trace($b);
?>
