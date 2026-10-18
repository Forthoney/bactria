module Make (C : Negamax_intf.Config) : Negamax_intf.S

(* tries columns from the center outward *)
module Center_first : Negamax_intf.S
