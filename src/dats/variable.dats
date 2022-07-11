#include "share/atspre_staload.hats"
#staload "./../sats/variable.sats"
#staload "libats/SATS/hashtbl_linprb.sats"
#staload UN = "prelude/SATS/unsafe.sats"

local
  datatype variable_t =
    | Bound of Int
    | Free of (string, Int)

  assume variable = variable_t

  val stamp = ref<Int>(0)
in
  implement mk (s : string) = let
    val id = stamp[]
    val _ = stamp[] := id + 1
  in
    Free (s, id)
  end

  implement bound (k) = Bound k

  implement equal (x, y) =
    case (x, y) of
    | (Free (_, x), Free (_, y)) => x = y
    | (Bound x, Bound y) => x = y
    | (_, _) => false

  implement fresh (x) =
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
        Some i
      else
        None
    | Free _ => None

  implement hash_key<variable>(x) = $UN.cast2ulint (x)
  implement equal_key_key<variable>(x, y) = equal (x, y)

  implement fprint_val<variable> = fprint_variable

  implement fprint_variable (out, x) =
    case x of
    | Bound k => fprint!(out, "_", k)
    | Free (x, id) => fprint!(out, x, "_", id)

  implement print_variable (x) = fprint_variable (stdout_ref, x)
  implement prerr_variable (x) = fprint_variable (stderr_ref, x)
end