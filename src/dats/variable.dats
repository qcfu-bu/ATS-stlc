#include "share/atspre_staload.hats"
#staload "./../sats/variable.sats"
#staload "libats/SATS/hashfun.sats"
#staload "libats/SATS/hashtbl_chain.sats"
#staload _ = "libats/DATS/hashtbl_chain.dats"
#staload UN = "prelude/SATS/unsafe.sats"

local
  datatype variable_t =
    | Bound of Int
    | Free of (string, Int)

  assume variable = variable_t
  assume vmap (a) = hashtbl(int, a)

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

  implement fprint_val<variable> = fprint_variable

  implement fprint_variable (out, x) =
    case x of
    | Bound k => fprint!(out, "_", k)
    | Free (x, id) => fprint!(out, x, "_", id)

  implement print_variable (x) = fprint_variable (stdout_ref, x)
  implement prerr_variable (x) = fprint_variable (stderr_ref, x)

  implement{key} equal_key_key = gequal_val_val<key>

  implement hash_key<int>(id) = let
    val key = $UN.cast{uint32}(id)
  in
    $UN.cast{ulint}(inthash_jenkins<>(key))
  end

  implement{a} empty () =
    hashtbl_make_nil<int,a>(i2sz(200))

  implement{a} find (x, tbl) =
    case x of
    | Bound _ => None_vt
    | Free (_, id) =>
      hashtbl_search_opt<int,a>(tbl, id)
end