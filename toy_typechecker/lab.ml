type lab = string
type t = lab

let of_string (lab : t) : string = lab
let to_string (s : string) : t = s

module Order =
struct
	type t = lab
	let compare = compare
end
module Map = Map.Make(Order)
module Set = Set.Make(Order)
