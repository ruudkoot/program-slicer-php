<?
function isValidUser($name,$message)
{
    if($name == null)
    {
        echo $message+$name;
        return false;
    }
    else
        return true;
}

$validUsers = 0;

for($i=0;$i<$numUsers;$i++)
{
    if(isValidUser(getUser($i),"Invalid user: "))
        $validUsers++;
}

trace($validUsers);
?>
