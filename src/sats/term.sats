#staload "./variable.sats"
#staload "libats/SATS/hashtbl_chain.sats"

abstype abs

datatype ty =
  | Prim of string
  | Arrow of (ty, ty)

datatype tm =
  | Var of variable
  | Lam of (ty, abs)
  | App of (tm, tm)

fun bind : (variable, tm) -> abs
fun unbind : abs -> (variable, tm)
fun subst : (variable, tm, tm) -> tm
fun msubst : (!vmap(tm), tm) -> tm

fun fprint_tm : (FILEref, tm) -> void
fun print_tm : tm -> void
fun prerr_tm : tm -> void

overload fprint with fprint_tm
overload print with print_tm
overload prerr with prerr_tm