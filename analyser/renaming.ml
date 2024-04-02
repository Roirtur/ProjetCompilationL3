open Util
open Ast

(* Codez ici la passe de renommage de termes.

   Cette passe permet de simplifier grandement la gestion des passes suivantes, et de l’interprétation, en assurant qu’un nom de variable 
   n’est jamais réutilisé dans la même portée de déclaration.

   Pour cela, on va modifier le programme en, pour chaque nom, gardant un nombre correspondant son nombre de redéfinition, et en renommant 
   l’occurence de chaque nom par un identifiant unique (son nom, suivi de son nombre d’occurence, avec un séparateur interdit dans le 
   langage (pour empêcher les redéfinitions)).

   Comme seule la portée des variables est importante, dans deux blocs disjoints, il est possible de réutiliser un même nom.

   Pour obtenir ce résultat, il sera nécessaire de copier les environnement avant d’évaluer des sous-blocs (puisqu’en sortant d’un bloc, 
   il est possible de continuer à utiliser un nom défini plus haut).

   Attention, l’interpréteur ne fonctionnera pas correctement en cas de redéfinition si vous n’effectuez pas correctement cette passe.*)

let rec renaming_arg_list (arg_list, counter_env) =
  match arg_list with
  | [] -> []
  | Argument ( name, typ, anno ) :: tl_arg_list -> 
   (match (Environment.get counter_env name) with
      | None -> Environment.add counter_env name 0; Argument(name, typ, anno) :: (renaming_arg_list (tl_arg_list, counter_env))
      | Some value -> Environment.modify counter_env name (value + 1); Argument((name ^ "#" ^ (string_of_int (value + 1))), typ, anno) :: (renaming_arg_list (tl_arg_list, counter_env))
   )

let rec renaming_expression (expression, counter_env) = 
   match expression with
   | Coord (x, y, anno) -> Coord (renaming_expression (x, counter_env), renaming_expression (y, counter_env), anno)
   | Color (r, g, b, anno) -> Color (renaming_expression (r, counter_env), renaming_expression (g, counter_env), renaming_expression (b, counter_env), anno)
   | Pixel (x, y, anno) -> Pixel (renaming_expression (x, counter_env), renaming_expression (y, counter_env), anno)
   | Variable (name, anno) -> (match (Environment.get counter_env name) with
      | None -> Environment.add counter_env name 0; Variable (name, anno)
      | Some value -> Environment.modify counter_env name (value + 1); Variable ((name ^ "#" ^ (string_of_int (value + 1))), anno)
   )
   | Binary_operator (op, e1, e2, anno) -> Binary_operator (op, renaming_expression (e1, counter_env), renaming_expression (e2, counter_env), anno)
   | Unary_operator (op, e, anno) -> Unary_operator (op, renaming_expression (e, counter_env), anno)
   | Field_accessor (field, e, anno) -> Field_accessor (field, renaming_expression (e, counter_env), anno)
   | List (l, anno) -> List (List.map (fun x -> renaming_expression (x, counter_env)) l, anno)
   | _ -> expression

let rec renaming_statement (statement, counter_env) =
  match statement with
   | Affectation (name, expression, anno) -> Affectation (name, renaming_expression (expression, counter_env), anno)
   | Declaration(id, typ, anno) -> (match (Environment.get counter_env id) with
      | None -> Environment.add counter_env id 0; Declaration(id, typ, anno)
      | Some value -> Environment.modify counter_env id (value + 1); Declaration((id ^ "#" ^ (string_of_int (value + 1))), typ, anno)
   )
   | Block (block, anno) -> Block (List.map (fun x -> renaming_statement (x, Environment.copy counter_env)) block, anno)
   | IfThenElse (condition, then_block, else_block, anno) -> IfThenElse (condition, renaming_statement (then_block, Environment.copy counter_env), renaming_statement (else_block, Environment.copy counter_env), anno)
   | For (id, start, stop, step, block, anno) -> (match (Environment.get counter_env id) with
      | None -> Environment.add counter_env id 0; For(id, start, stop, step, renaming_statement (block, Environment.copy counter_env), anno)
      | Some value -> Environment.modify counter_env id (value + 1); For((id ^ "#" ^ (string_of_int (value + 1))), start, stop, step, renaming_statement (block, Environment.copy counter_env), anno)
   )
   | Foreach (id, expression, block, anno) -> (match (Environment.get counter_env id) with
      | None -> Environment.add counter_env id 0; Foreach(id, renaming_expression (expression, counter_env), renaming_statement (block, Environment.copy counter_env), anno)
      | Some value -> Environment.modify counter_env id (value + 1); Foreach((id ^ "#" ^ (string_of_int (value + 1))), renaming_expression (expression, counter_env), renaming_statement (block, Environment.copy counter_env), anno)
   )
   | Draw_pixel (expression, anno) -> Draw_pixel (renaming_expression (expression, counter_env), anno)
   | Nop -> Nop
   | Print (expression, anno) -> Print (renaming_expression (expression, counter_env), anno)


let renaming (program) =
   let name_counter = Environment.new_environment () in
   match program with
    | Program (arg_list, statement) -> Program (renaming_arg_list (arg_list, name_counter), renaming_statement (statement, name_counter))
 