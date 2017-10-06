type var = int
type t = var

module Hash =
struct
	type t = var
	let equal : t -> t -> bool = (=)
	let hash (x : t) : int = x
end
module Tbl = Hashtbl.Make(Hash)

type var_info = {
	name : string;
}

let initial_size = 50
let vars : var_info Tbl.t = Tbl.create initial_size
let names : (string, var) Hashtbl.t = Hashtbl.create initial_size

let fresh (basename : string) : t =
	let var = Tbl.length vars in
	let name = "$" ^ basename ^ string_of_int var in
	let info = {name} in
	Tbl.add vars var info;
	Hashtbl.add names name var;
	var

let of_string (name : string) : t =
	if String.length name = 0
	|| name.[0] = '$' then
		invalid_arg ("Var.of_string: " ^ name);
	try
		Hashtbl.find names name
	with Not_found ->
		let var = Tbl.length vars in
		let info = {name} in
		Tbl.add vars var info;
		Hashtbl.add names name var;
		var

let to_string (var : t) : string = (Tbl.find vars var).name

module Order =
struct
	type t = var
	let compare : t -> t -> int = (-)
end
module Map = Map.Make(Order)
module Set = Set.Make(Order)
