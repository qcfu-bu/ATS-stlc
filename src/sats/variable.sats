abstype variable

fun mk : string -> variable
fun bound : Int -> variable
fun equal : (variable, variable) -<> INV(bool)
fun fresh : variable -> variable
fun is_bound : 
  {sz, k:nat}(variable, int(sz), int(k)) -> 
  Option ([i:nat | k <= i; i < k + sz] int(i))

fun fprint_variable : (FILEref, variable) -> void
fun print_variable : variable -> void
fun prerr_variable : variable -> void

overload fprint with fprint_variable
overload print with print_variable
overload prerr with prerr_variable