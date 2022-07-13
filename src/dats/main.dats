#include "share/atspre_staload.hats"

#staload "./../sats/types.sats"
#staload "./../sats/variable.sats"
#staload "./../sats/term.sats"
#staload "./../sats/vmap.sats"

#staload _ = "./term.dats"
#staload _ = "./vmap.dats"

#dynload "./variable.dats"

val x = mk "x"
val y = mk "y"

val bar_ty =
  Arrow (Prim "A", Prim "A")

val foo = 
  Lam (bar_ty, bind (x, 
  Lam (Prim "A", bind (y, 
    App (Var x, Var y)))))

val bar = 
  Lam (Prim "A", bind (y, Var y))

val test = App (foo, bar)

implement main0() = let
  val _ = println! (eval test)
  val opt = infer (empty (), foo)
in
  case opt of
  | ~None_vt () => println!("infer failed")
  | ~Some_vt ty => println!("infer success(", ty, ")")
end