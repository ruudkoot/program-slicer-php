<?php
     print("Een test!");
     function cnt_backwards($from) {
          $from--;
          if($from <= 1) {
               print($from);
               return;
          }
          print($from);
          $ret = cnt_backwards($from);
     }
     $ret = cnt_backwards(5);
?>
