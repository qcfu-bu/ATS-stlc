#staload "./types.sats"

fun bind : (variable, tm) -> abs
fun unbind : abs -> (variable, tm)
fun subst : (variable, tm, tm) -> tm
fun msubst : (!vmap tm, tm) -> tm
fun eval : tm -> tm
fun equal_ty : (ty, ty) -> bool
fun infer : (vmap ty, tm) -> Option_vt ty

fun fprint_ty : (FILEref, ty) -> void
fun print_ty : ty -> void
fun prerr_ty : ty -> void

fun fprint_tm : (FILEref, tm) -> void
fun print_tm : tm -> void
fun prerr_tm : tm -> void

overload = with equal_ty

overload fprint with fprint_ty
overload print with print_ty
overload prerr with prerr_ty

overload fprint with fprint_tm
overload print with print_tm
overload prerr with prerr_tm