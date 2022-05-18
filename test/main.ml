open OUnit2

(* TESTING STRATEGY: We decided to test the backing data types with
   OUnit. Since they have relatively little dependency on the rest of
   the system, they're particularly amenable to automated testing.
   Furthermore, we want assurance that are backing data types are sound
   so we can more easily pinpoint the source of bugs. OUnit tests the
   modules Item, Inventory, and Player. Test cases were primarily
   developed via black-box testing. In practice, since the different
   code paths in the implementation of the modules closely mirror the
   different possibiltiies via specification, we also achieve good code
   coverage. The rest of the modules were tested manually via play
   testing. In particular, the other gameplay elements have significant
   randomness and game state dependency, so we found it more reasonable
   to test them in the context of a running game. Of course, graphical
   modules, such as most of the modules in Controller, can only be
   tested by running them. We ran these modules and tested the options
   available to the player ourselves to make sure the choices the player
   can make behave correctly. By running automated tests on our data
   types and manually verifying the behavior of the game all put to
   together, we demonstrate correctness of our system.*)

let suite =
  "test suite for Final Project"
  >::: List.flatten
         [ Itemtest.tests; Inventorytest.tests; Playertest.tests ]

let _ = ignore Testutil.cmp_set_like_lists
let _ = run_test_tt_main suite
