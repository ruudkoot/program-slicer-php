<?php
     function cnt_backwards($from) {
          $from--;
          if($from <= 1) {
               print($from);
               return;
          }
          print($from);
          cnt_backwards($from);
     }
     cnt_backwards(5);
?>