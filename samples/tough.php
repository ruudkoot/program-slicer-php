<?

function abc($a,$b,$c)
{
    $d = $b*$b-4.0*$a*$c;
    $dsq = sqrt($d);
    if($d == 0.0){
        $x0 = $x1 = -$b / 2*$a;
    }
    else
    {
        if($d < 0.0)
            $x0 = $x1 = 0.0;
        else
        {
            $x0 = (-$b - $dsq)/2*$a;
            $x1 = (-$b + $dsq)/2*$a;            
        }
    }
    out($x0,$x1);
}

abc(1.0,2.0,4.0);

/*
function powerRec($base,$exp)
{
    return ($exp==0)?1:powerRec($base,$exp-1);
}

function powerFor($base,$exp)
{
    $ret = 1;
    for($i=0; $i < $exp; $i++)
        $ret *= $exp;
    return $ret;
}

powerFor(4,5);
powerRec(4,5);*/
?>
