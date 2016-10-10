<?php

// usage: php -f convert_monads_to_ppx.php

/*
$x = "aa";
$y = $x;
$y[1] = "c";
//$x[0] = "b";
//$x[1] = "d";
printf("%s %s\n", $x, $y);
*/


$input = "JsInterpreterSrc.ml";
$output = "JsInterpreterOut.ml";

$sinput = file_get_contents($input);
/*

$sinput = <<<EOF


and run_expr_assign s c opo e1 e2 =
  if_success (run_expr s c e1) (fun s1 rv1 ->
    let_binding (fun s0 rv_2 ->
      match rv_2 with
      | Coq_resvalue_empty ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
          s0
          ("Non-value result in [run_expr_assign].")
      | Coq_resvalue_value v ->
        if_void (ref_put_value s0 c rv1 v) (fun s_2 ->
          result_out (Coq_out_ter (s_2, (res_val v))))
      | Coq_resvalue_ref r ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
          s0
          ("Non-value result in [run_expr_assign]."))
      (fun follow ->
      match opo with
      | Some op ->
        if_spec (ref_get_value s1 c rv1) (fun s2 v1 ->
          if_spec (run_expr_get_value s2 c e2) (fun s3 v2 ->
            if_success (run_binary_op s3 c op v1 v2) (fun s4 v -> follow s4 v)))
      | None ->
        if_spec (run_expr_get_value s1 c e2) (fun x x0 ->
          follow x (Coq_resvalue_value x0))))
EOF;
*/
$ninput = strlen($sinput);

/*
$matches = array();
$n = preg_match('/\G\s*run_([^ ]*)/', $sinput, $matches, PREG_OFFSET_CAPTURE, 3);
print_r($matches);
exit;
*/
//printf("%d\n", $n);


//printf("%s\n", $sinput);

/*
let if_some op k =
let if_some_or_default o d k =
let if_result_some w k =
let if_out_some w k =
let if_ter w k =
let if_success_state rv w k =
let if_success w k =
let if_void w k =
let if_not_throw w k =
let if_any_or_throw w k1 k2 =
let if_success_or_return w k1 k2 =
let if_break w k =
let if_value w k =
let if_bool w k =
let if_object w k =
let if_string w k =
let if_number w k =
let if_prim w k =
let if_abort o k =
let if_spec w k =
*/

function fail($s, $i) {
   global $sinput;
   printf("error: %s\n", $s);
   printf("at: %s\n", substr($sinput, $i, 200));
   exit(1);
}


// return index of matching parent
function matching_parenthesis($iOpenParent) {
   global $sinput, $ninput;
   if ($sinput[$iOpenParent] != "(") {
      fail("matching not open", $iOpenParent);
   }
   $d = 1;
   for ($i = $iOpenParent+1; $i < $ninput; $i++) {
      $c = $sinput[$i];
      if ($c == '(') {
         $d++;
      } else if ($c == ')') {
         $d--;
         if ($d == 0) {
            return $i;
         }
      }
   }
   return -1;
}


$soutput = '';

function process($start, $stop) {
   //printf("process %d to %d\n", $start, $stop);
   global $sinput, $soutput;
   for ($i = $start; $i <= $stop;) {
      $matches = array();
      $n = preg_match('/if_([^ \n]*)/', $sinput, $matches, PREG_OFFSET_CAPTURE, $i);
      if ($n != 1) {
         break;
      }
      $occ = $matches[0][1];
      if ($occ > $stop) {
         break;
      }
      $npat = strlen($matches[0][0]); 
      $key = $matches[1][0];
      if ($key == "some_or_default" || $key == "empty_label" || $key == "any_or_throw" || $key == "empty") {
         $i = $occ+1;
         continue;
      }
      //printf("in %d to %d, offset %d, found %s at %s\n", $start, $stop, $i, $key, $occ);
      //print_r($matches);
      $matches1 = $matches;
      $add = substr($sinput, $i, ($occ-1) - $i + 1);
      $soutput .= $add;

      $n = preg_match('/\G\s*(\()/', $sinput, $matches, PREG_OFFSET_CAPTURE, $occ + $npat);
      if ($n != 1) {
         fail("no open parent for first arg of monad", $occ);
      }
      $open = $matches[1][1];
      if ($open > $stop) {
         fail("no open parent for first arg of monad", $occ);
      }
      $close = matching_parenthesis($open);
      //printf("-- first open from %d to %d\n", $open, $close);
      if ($close == -1 || $close > $stop) {
         fail("unclosed first parent", $open);
      }

      $matches = array();
      $n = preg_match('/\G\s*(\(fun(.*?)->[ ]*)/', $sinput, $matches, PREG_OFFSET_CAPTURE, $close+1);
      if ($n != 1) {
         printf("key = '%s'\n", $key);
         printf("when: %s\n", substr($sinput, $occ, 100));
         fail("no bopen parent for second arg of monad", $close+1);
      }
      $bopen = $matches[1][1];
      if ($bopen > $stop) {
         fail("no bopen parent for second arg of monad", $occ);
      }
      //print_r($matches);
      $bsize = strlen($matches[1][0]);
      $args = $matches[2][0];
      // printf("args=%s\n", $args);
      $args_pieces = preg_split('/\s+/', $args, NULL, PREG_SPLIT_NO_EMPTY);
      // print_r($args_pieces);
      if (count($args_pieces) == 1) {
         $arg = $args_pieces[0];
      } else if (count($args_pieces) == 2) {
         $arg = "(" . $args_pieces[0] . "," . $args_pieces[1] . ")";
      } else {
        printf("when: %s\n", substr($sinput, $i, 100));
        printf("added: %s\n", $add);
        print_r($matches1);
        print_r($matches);
        print_r($args_pieces);
         fail("invalid count for pieces: " . count($args_pieces), $close);
      }

      $bclose = matching_parenthesis($bopen);
      if ($bclose == -1 || $bclose > $stop) {
         printf("bclose=%d, stop=%d\n", $bclose, $stop);
         printf("bopen: %s\n", substr($sinput, $bopen, 100));
         printf("when: %s\n", substr($sinput, $occ, 100));
         fail("unclosed second parent", $bopen);
      }
      $sinput[$bclose] = " ";
      // &("-- second from %d to %d\n", $bopen, $bclose);
      $len = ($close - 1) - ($open + 1) + 1;
      $def = substr($sinput, $open + 1, $len);
      $add = 'let%' . $key . " " . $arg . ' = ' . $def . " in\n";
      // printf("-- add: %s\n", $add);
      $soutput .= $add;

      $sub = $bopen + $bsize;
      $n = preg_match('/\G[ ]*(\n)/', $sinput, $matches, PREG_OFFSET_CAPTURE, $sub);
      if ($n == 1) {
         $sub = $matches[1][1] + 1;
      }
      process($sub, $bclose - 1);
      $i = $bclose + 1;
      // printf("-- continue at offset: %d\n", $i);
   }
   if ($i <= $stop) {
      $add = substr($sinput, $i, $stop - $i);
      $soutput .= $add;
   }
   // printf("-- add rest: %s\n", $add);
   // printf("return %d to %d\n", $start, $stop);
}


process(0, $ninput-1);
 file_put_contents($output, $soutput);
// printf("%s\n", $soutput);

?>