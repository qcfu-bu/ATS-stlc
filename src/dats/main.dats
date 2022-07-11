#include "share/atspre_staload.hats"
#staload "./../sats/variable.sats"
#staload "./variable.dats"
#dynload "./variable.dats"
#staload "./../sats/term.sats"
#staload "./term.dats"

val x = mk "x"
val y = mk "y"

val foo = Lam (Prim "A", bind (x, Var x))

implement main0() = println!(foo)