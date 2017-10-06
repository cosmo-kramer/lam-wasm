type var = Var.t
type lab = Lab.t

type ty =
| Tint | Tunit
| Tfun of ty * ty
| Tref of ty
| Tun

type exp =
| Int of int
| Unit
| Var of var
| Abs of var * exp
| App of exp * exp
| Let of var * exp * exp	(* Not a derived form due to typing. *)
| Ref of exp
| Deref of exp
| Assign of exp * exp
| Asc of exp * ty

(* labels name declarations for external reference *)
(* declarations bind variables for internal reference *)
type dec = ValDec of lab * var * exp
type decs = dec list	(* a compilation unit *)

type spec = ValSpec of lab * ty
type specs = spec list	(* a compilation unit interface *)
