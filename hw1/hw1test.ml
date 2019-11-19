(* Copyright 2006-2011, 2013-2016, 2019 Paul Eggert.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  *)

let my_subset_test0 = subset [1;2;3] [1;2;3]
let my_subset_test1 = subset [-3;-2;-1] [1;2;3;-3;-2;-1]
let my_subset_test2 = not (subset [-3;-2;-1] [1;2;3])
let my_subset_test3 = subset [-1000;100000] [-1000;100000;20000]
let my_subset_test4 = not (subset [-1000;3;7] [4;1;3])
let my_subset_test5 = subset [] []
let my_subset_test6 = subset [] [1;2;3]
let my_subset_test7 = not (subset [1;2;3] [])
let my_subset_test8 = subset [1;2;3] [1;1;3;2;2;3;1;4;7]
let my_subset_test9 = not (subset [1;2;3;8] [1;1;3;2;2;3;1;4;7])
let my_subset_test10 = subset [1;2;3;3;2;1;1;2] [1;2;3;4]

let my_equal_sets_test0 = equal_sets [1;2;3] [1;2;3]
let my_equal_sets_test1 = equal_sets [1;3;9;-10] [-10;9;9;3;1;-10]
let my_equal_sets_test2 = equal_sets [] []
let my_equal_sets_test3 = not (equal_sets [1;3;4] [3;1;3])
let my_equal_sets_test4 = not (equal_sets [] [3;1;3])
let my_equal_sets_test5 = not (equal_sets [1;3;4] [])
let my_equal_sets_test6 = not (equal_sets [1;3;4;4] [1;2;3])
let my_equal_sets_test7 = equal_sets [1;3;1;4;4] [1;3;4]

let my_set_union_test0 = equal_sets (set_union [7;8;9] [9;10;15]) [7;8;9;10;15]
let my_set_union_test1 = equal_sets (set_union [9;9;7;4;3;7] [10;15;20;15;9]) [3;4;7;9;10;15;20]
let my_set_union_test2 = equal_sets (set_union [-10;-9;-8] [1;2;3]) [1;2;3;-10;-9;-8]
let my_set_union_test3 = equal_sets (set_union [-10;-9;-8;-8;-9;-10] [1;2;3;3;2;1]) [1;2;3;-10;-9;-8]
let my_set_union_test4 = equal_sets (set_union [1;2;3;4;5] [1;2;3;4;5]) [1;2;3;4;5]
let my_set_union_test5 = equal_sets (set_union [1;2;3;4;4;3] [1;3;4;2]) [1;2;3;4]
let my_set_union_test6 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let my_set_union_test7 = equal_sets (set_union [] [3;3;2;1]) [1;2;3]

let my_set_intersection_test0 =
  equal_sets (set_intersection [] []) []
let my_set_intersection_test1 =
  equal_sets (set_intersection [1;4;5;1;5;4] [1;9;10;10;9]) [1]
let my_set_intersection_test2 =
  equal_sets (set_intersection [1;4;5] [2;9;10;4;1;5]) [1;4;5]
let my_set_intersection_test3 =
  equal_sets (set_intersection [100;1000;10000] [9;99;999]) []
let my_set_intersection_test4 =
  equal_sets (set_intersection [3;9;10;-100] [3;9;10;-100]) [3;9;10;-100]
let my_set_intersection_test5 =
  equal_sets (set_intersection [100;80;0;0;80] [80;0;0;100]) [0;80;100]
let my_set_intersection_test6 =
  equal_sets (set_intersection [] [9;10]) []
let my_set_intersection_test7 =
  equal_sets (set_intersection [1;2;100] []) []

let my_set_diff_test0 = equal_sets (set_diff [] [1;2;3]) []
let my_set_diff_test1 = equal_sets (set_diff [1;3;2;2;3] []) [1;2;3]
let my_set_diff_test2 = equal_sets (set_diff [9;10;11;1;2;3] [1;2;3;4;5;6]) [9;10;11]
let my_set_diff_test3 = equal_sets (set_diff [] []) []
let my_set_diff_test4 = equal_sets (set_diff [1;2;3] [7;8;9]) [1;2;3]

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 2) 1024 = 0
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> sqrt(x)) 1. = 1.
  
type awksub_nonterminals =
  | Expr | A | B | C | D | E
let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N A; N B];
    Expr, [N C; N D];
    A, [T "Mary"];
    B, [T "Mark"];
    C, [T "Mango"];
    D, [N E];
    E, [T "1"]]

let awksub_grammar = Expr, awksub_rules

let awksub_test0 =
  filter_reachable awksub_grammar = awksub_grammar

let awksub_test1 =
  filter_reachable (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules)


