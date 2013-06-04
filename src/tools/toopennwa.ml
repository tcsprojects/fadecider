open Arg ;;
open Tcsbasedata;;
open Tcslist;;
open Tcsset;;
open Tcsargs;;
open Tcsautomata;;
open Tcsautotransform;;
open Tcsautohelper;;
open Tcsautomataparser;;

(* stack has to be equal to state *)

let _ =
  let source = parse_automaton2 stdin in
  let h = function NMVPA.Internal i -> i | NMVPA.Push i -> i | NMVPA.Pop i -> i in
  match source with
	NbvpaType (auto, state_iter, alpha_iter, stack_iter) -> (
		print_string (ListUtils.custom_format string_of_int "Q: {\n" "}\n" ",\n" (Iterators.to_list state_iter));
		print_string ("Q0: {\n" ^ string_of_int (NMVPA.initial auto) ^ "}\n");
		print_string (ListUtils.custom_format string_of_int "Qf: {\n" "}\n" ",\n" (Iterators.to_list (Iterators.filter (NBVPA.accept auto) state_iter)));
		print_string (ListUtils.custom_format (fun x -> string_of_int (h x)) "Sigma: {\n" "}\n" ",\n" (Iterators.to_list alpha_iter));
		print_string (ListUtils.custom_format (fun ((a,b),(c,_)) -> "(" ^ string_of_int a ^ "," ^ string_of_int (h b) ^ "," ^ string_of_int c ^ ")") "Delta_c: {\n" "}\n" ",\n" (Iterators.to_list (
			Iterators.depend_product
				(Iterators.product state_iter (Iterators.filter (function NMVPA.Push _ -> true | _ -> false) alpha_iter))
				(fun (x,y) -> NMVPA.delta_push auto x y)
		)));
		print_string (ListUtils.custom_format (fun ((a,b),c) -> "(" ^ string_of_int a ^ "," ^ string_of_int (h b) ^ "," ^ string_of_int c ^ ")") "Delta_i: {\n" "}\n" ",\n" (Iterators.to_list (
			Iterators.depend_product
				(Iterators.product state_iter (Iterators.filter (function NMVPA.Internal _ -> true | _ -> false) alpha_iter))
				(fun (x,y) -> NMVPA.delta_internal auto x y)
		)));
		print_string (ListUtils.custom_format (fun (((a,b),c),d) -> "(" ^ string_of_int a ^ "," ^ string_of_int c ^ "," ^ string_of_int (h b) ^  "," ^ string_of_int d ^ ")") "Delta_r: {\n" "}\n" ",\n" (Iterators.to_list (
			Iterators.depend_product
				(Iterators.product (Iterators.product state_iter (Iterators.filter (function NMVPA.Pop _ -> true | _ -> false) alpha_iter)) stack_iter)
				(fun ((x,y),z) -> NMVPA.delta_pop auto x y (Some z))
		)));
	)
  | _ -> failwith "wrong automaton type"
