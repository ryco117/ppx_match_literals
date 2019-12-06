open Ppxlib

let match_ext_name = "literals"
let case_ext_name = "lit"
let sanitized_prefix = "ppx_match_literals_lit_"

exception Bad_match_var_syntax of string;;

(*Map string to an identifier AST node*)
let create_identifier loc var_name = 
  {pexp_desc = Pexp_ident {txt = Lident var_name; loc = loc}; pexp_loc = loc; pexp_attributes = []}

(*Map string to an identifier pattern (ie. a wildcard node) *)
let create_identifier_pattern loc var_name = 
  {ppat_desc = Ppat_var {txt = var_name; loc = loc}; ppat_loc = loc; ppat_attributes = []}

(*Append an equivalency check to an existing guard based on the given var_name and a sanitized wildcard variable*)
let create_guard loc old_guard var_name sanitized_wildcard =
  let new_guard = [%expr [%e create_identifier loc sanitized_wildcard] = [%e create_identifier loc var_name]]
  in match old_guard with 
  | None -> new_guard
  | Some guard ->  [%expr [%e guard] && [%e new_guard]] 

(*Given an initial guard and a list of guards, produce a guard equivalent to the "&&" operator applied between them*)
let fold_guards loc initial_guard guards_list =
  List.fold_left (fun acc_guard new_guard ->
      match (acc_guard, new_guard) with
      | (Some a, Some n) -> Some [%expr [%e a] && [%e n]]
      | (Some a, _) -> Some a
      | (_, Some n) -> Some n
      | _ -> None) initial_guard guards_list

(*Given an initial list and a list of lists, append all lists together*)
let fold_var_lists init_list var_lists =
  List.fold_left (fun acc_list new_list ->
    List.append new_list acc_list) init_list var_lists

(*Append var_name to list if not already contained*)
let rec append_name_to_list var_name lst = 
  match lst with
  | [] -> [var_name]
  | hd::tl when hd = var_name -> hd::tl
  | hd::tl -> hd::(append_name_to_list var_name tl)

(*Takes a pattern node and a list of variable names and returns a pair (node to replace with, updated var list)
 *Pattern types from https://caml.inria.fr/pub/docs/manual-ocaml/compilerlibref/Parsetree.html#TYPEpattern_desc*)
let rec replace_literals loc var_list pattern_node =
  match pattern_node with
  (*Check if pattern is an extension [%lit ...] node*)
  | {ppat_desc = Ppat_extension ({txt = "lit"; _}, pstr); _} ->
    (match pstr with 
    (*Check if used our extension correctly*)
     | PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_ident {txt = Lident var_name; _}; _}, _); _}] ->
       (*Replace given pattern with a wildcard and append new guard*)
       let sanitized_var_name = sanitized_prefix ^ var_name (*string_of_int (Random.bits ())*) in
       (create_identifier_pattern loc sanitized_var_name, append_name_to_list var_name var_list)

     (*Used our extension incorrectly*)
     | _ -> raise (Bad_match_var_syntax ("Invalid use of extension:" ^ match_ext_name ^ ":" ^ case_ext_name)))

  (*Check if node is an alias*)
  | {ppat_desc = Ppat_alias (alias_pattern, {txt = alias; loc = alias_loc}); _} ->
    (match replace_literals loc var_list alias_pattern with
     | (n, v_lst) -> ({ppat_desc = Ppat_alias (n, {txt = alias; loc = alias_loc}); 
                       ppat_loc = loc; ppat_attributes = []}, v_lst))

  (*Check if pattern is a tuple*)
  | {ppat_desc = Ppat_tuple tuple_list; _} ->
    (*Retrieve ordered list of nodes to replace and vars needed by folding nodes list*)
    (match List.fold_left (fun (n_lst, v_lst) node ->
         (match replace_literals loc v_lst node with
          | (n, new_v_lst) -> (List.append n_lst [n], new_v_lst))) ([], var_list) tuple_list with
     (*Retrieved ordered list of nodes to replace and vars needed*)
     | (nodes_list, new_var_list) ->
       ({ppat_desc = Ppat_tuple nodes_list; ppat_loc = loc; ppat_attributes = []}, new_var_list))

  (*Check if node is a construct*)
  | {ppat_desc = Ppat_construct ({txt = Lident cons_type; loc = cons_loc}, Some cons_pattern); _} ->
    (match replace_literals loc var_list cons_pattern with
     | (n, v_lst) -> ({ppat_desc = Ppat_construct ({txt = Lident cons_type; loc = cons_loc}, Some n);
                       ppat_loc = loc; ppat_attributes = []}, v_lst))

  (*Check if node is a variant pattern*)
  | {ppat_desc = Ppat_variant (label, Some pattern); _} -> (match replace_literals loc var_list pattern with
    | (n, g) -> ({ppat_desc = Ppat_variant (label, Some n);
                  ppat_loc = loc; ppat_attributes = []}, g))

  (*Check if node is a record pattern*)
  | {ppat_desc = Ppat_record (fields_list, closed_status); _} ->
    (match List.fold_left (fun (field_node_pairs, vars) (f, n) ->
         match replace_literals loc vars n with (new_n, v_lst) ->
           (List.append field_node_pairs [(f, new_n)], v_lst)) ([], var_list) fields_list with
     | (new_fields_list, new_var_list) ->
       ({ppat_desc = Ppat_record (new_fields_list, closed_status); ppat_loc = loc; ppat_attributes = []},
        new_var_list))

  (*Check if node is an array -- TODO: DOESN'T WORK!!! 
   * I think arrays are being populated before matching is able to maniplulate AST*)
  (*| {ppat_desc = Ppat_array array_list; _} ->
    let patterns_list = List.map (replace_literals loc None) array_list in
    let _nodes_list = List.map (function (n, _) -> n) patterns_list in
    let guards_list = List.map (function (_, g) -> g) patterns_list in
    ({ppat_desc = Ppat_array nodes_list; ppat_loc = loc; ppat_attributes = []},
     fold_guards loc guard guards_list)*)

  (*Check if node is an "or" pattern
   * TODO fix https://caml.inria.fr/pub/docs/manual-ocaml/comp.html#sec292 WARNING 9.5.3 *)
  | {ppat_desc = Ppat_or (pattern1, pattern2); ppat_loc = or_loc; _} ->
    (match replace_literals loc var_list pattern1 with
    | (n1, v_lst1) -> (match replace_literals loc v_lst1 pattern2 with
      | (n2, v_lst2) -> ({ppat_desc = Ppat_or (n1, n2); ppat_loc = or_loc; ppat_attributes = []}, v_lst2)))

  (*Check if node is a type constraint*)
  | {ppat_desc = Ppat_constraint (pattern, node_core_type); _} -> (match replace_literals loc var_list pattern with
      | (n, g) -> ({ppat_desc = Ppat_constraint (n, node_core_type);
                    ppat_loc = loc; ppat_attributes = []}, g))

  (*Check if node is a lazy pattern*)
  | {ppat_desc = Ppat_lazy pattern; _} -> (match replace_literals loc var_list pattern with
      | (n, g) -> ({ppat_desc = Ppat_lazy n; ppat_loc = loc; ppat_attributes = []}, g))

  (*Check if node is an exception case*)
  | {ppat_desc = Ppat_exception pattern; _} -> (match replace_literals loc var_list pattern with
      | (n, g) -> ({ppat_desc = Ppat_exception n; ppat_loc = loc; ppat_attributes = []}, g))

  (*Check if is a module open pattern*)
  | {ppat_desc = Ppat_open (identity, pattern); _} -> (match replace_literals loc var_list pattern with
      | (n, g) -> ({ppat_desc = Ppat_open (identity, n); ppat_loc = loc; ppat_attributes = []}, g))

  (*Some other node, don't modify or recurse*)
  | _ -> (pattern_node, var_list)

(*Create function to map case statements according to type of AST node*)
let case_map loc case = 
  match case with
  | {pc_lhs = case_pattern; 
     pc_guard = guard; 
     pc_rhs = expression} ->
    match replace_literals loc [] case_pattern with
    | (new_case, var_list) ->
      (*Replace old pattern with recursively built new pattern-tree*)
      {pc_lhs = new_case;
       (*Replace old guard with accumulation of all guards needed for literals and original*)
       pc_guard = List.fold_left (fun acc_guard var_name ->
           Some (create_guard loc acc_guard var_name (sanitized_prefix ^ var_name))) guard var_list;
       pc_rhs = expression}

let expand ~loc ~path:_ expr =
  match expr with 
  (*Check if used the extension correctly*)
  | {pexp_desc = Pexp_match ({pexp_desc = match_description; pexp_loc = match_loc;
                              pexp_attributes = match_attributes},
                             case_list); pexp_attributes = ext_attributes; _} ->
    (*Replace extension with a match expression, where every case statement is mapped*)
    {pexp_desc = Pexp_match ({pexp_desc = match_description; pexp_loc = match_loc;
                              pexp_attributes = match_attributes},
                             List.map (case_map loc) case_list); 
     pexp_loc = loc; pexp_attributes = ext_attributes}

    (*Incorrect syntax for match extension*)
  | _ -> raise (Bad_match_var_syntax ("Invalid use of extension:" ^ match_ext_name))

(*Create extension using ppxlib*)
let extension = 
  Extension.declare
    match_ext_name 
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand

(*Define and register transformation rule for our custom match extension*)
let rule = Context_free.Rule.extension extension
let () =
    Driver.register_transformation ~rules:[rule] (match_ext_name ^ "_transformation")
