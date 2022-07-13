#staload "./types.sats"

fun mk : string -> variable
fun bound : Int -> variable
fun equal_variable : (variable, variable) -<> INV(bool)
fun fresh : variable -> variable
fun is_bound : 
  {sz, k:nat}(variable, int sz, int k) -> 
  Option_vt ([i:nat | k <= i; i < k + sz] int i)
fun id_of : variable -<> Int

fun fprint_variable : (FILEref, variable) -> void
fun print_variable : variable -> void
fun prerr_variable : variable -> void

overload = with equal_variable

overload fprint with fprint_variable
overload print with print_variable
overload prerr with prerr_variable
