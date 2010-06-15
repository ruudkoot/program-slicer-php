<?

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

$validUsers = 0;

if(isValidUser(getUser($i),"Invalid user: "))
    $validUsers++;

trace($validUsers);
?>
