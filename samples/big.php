<?

function mysql_connect($to)
{
    return $to;
}

function mysql_query($con,$q)
{
    return $con+$q;
}

function mysql_fetch_row($q)
{
    return $q;
}


function isValidUser($name,$message)
{
    if($name == null)
    {
        echo $message+$name;
        return 0;
    }
    else
        return 1;
}

function formatUserInfo($name)
{
    if(!isValidUser(getUser($i),"Invalid user: "))
        return "";
        
    $userGroup  = "\nGroup: " + userGroup($name);
    $userAge    = "\nAge: " + userAge($name);
    $userName   = "\nName: " + $name;
    return $userGroup + $userAge + $userName;
}

function displayUsersByGroup($group)
{
    $con = mysql_connect("localhost");
    $query = "SELECT name FROM user WHERE group =" + $group;
    $q = mysql_query($con,$query);
    
    while($r=mysql_fetch_row($q))
        $result += formatUserInfo($r);
    
    return $result;
}

$group = "ST Master Students";
$info = displayUsersByGroup($group);

echo $info;
 
trace($info);

?>
