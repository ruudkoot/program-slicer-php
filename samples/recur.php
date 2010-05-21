<?php
     out("Een test!");
     function cnt_backwards($from) {
          $from--;
          if($from <= 1) {
               out($from);
               return;
          }
          out($from);
          $ret = cnt_backwards($from);
     }
     $ret = cnt_backwards(5);
?>
