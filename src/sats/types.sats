abstype variable_boxed
typedef variable = variable_boxed

abstype vmap_boxed (a : type)
typedef vmap a = vmap_boxed a

abstype abs_boxed
typedef abs = abs_boxed

datatype ty =
  | Prim of string
  | Arrow of (ty, ty)

datatype tm =
  | Var of variable
  | Lam of (ty, abs)
  | App of (tm, tm)