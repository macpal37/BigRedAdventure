open OUnit2

let suite =
  "test suite for Final Project"
  >::: List.flatten
         [ Itemtest.tests; Inventorytest.tests; Playertest.tests ]

let _ = ignore Testutil.cmp_set_like_lists
let _ = run_test_tt_main suite
