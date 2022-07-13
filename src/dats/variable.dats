#include "share/atspre_staload.hats"
#staload "./../sats/types.sats"
#staload "./../sats/variable.sats"

local
  datatype variable_t =
    | Bound of Int
    | Free of (string, Int)

  assume variable_boxed = variable_t

  val stamp = ref<Int> 1
in
  implement mk (s : string) = let
    val id = stamp[]
    val _ = stamp[] := id + 1
  in
    Free (s, id)
  end

  implement bound k = Bound k

  implement equal_variable (x, y) =
    case (x, y) of
    | (Free (_, x), Free (_, y)) => x = y
    | (Bound x, Bound y) => x = y
    | (_, _) => false

  implement fresh x =
    case x of
    | Bound _ => x
    | Free (x, _) => let
        val id = stamp[]
        val _ = stamp[] := id + 1
      in
        Free (x, id)
      end

  implement is_bound (x, sz, k) =
    case x of
    | Bound i =>
      if (k <= i) * (i < k + sz) then
        Some_vt i
      else
        None_vt
    | Free _ => None_vt

  implement id_of x =
    case x of
    | Bound _ => 0
    | Free (_, id) => id

  implement fprint_val<variable> = fprint_variable

  implement fprint_variable (out, x) =
    case x of
    | Bound k => fprint!(out, "_", k)
    | Free (x, id) => fprint!(out, x, "_", id)

  implement print_variable x = fprint_variable (stdout_ref, x)
  implement prerr_variable x = fprint_variable (stderr_ref, x)
end