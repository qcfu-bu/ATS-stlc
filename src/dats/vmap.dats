#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

#staload "./../sats/types.sats"
#staload "./../sats/variable.sats"
#staload "./../sats/vmap.sats"

#staload "libats/ML/SATS/funmap.sats"
#staload UN = "prelude/SATS/unsafe.sats"

#staload _ = "./variable.dats"
#staload _ = "libats/ML/DATS/funmap.dats"
#staload _ = "libats/DATS/funmap_avltree.dats"

local
  assume vmap_boxed a = map (variable, a)
in
  implement gcompare_val_val<variable>(x, y) =
    id_of x - id_of y

  implement empty {a} () = funmap_nil<>()

  implement one {a} (x, a) = add (x, a, empty ())

  implement add {a} (x, a, vmap) = let
    var vmap = vmap
    val opt = funmap_insert<variable,a>(vmap, x, a)
  in
    case opt of
    | ~None_vt _ => vmap
    | ~Some_vt _ => vmap
  end

  implement remove {a} (x, vmap) = let
    var vmap = vmap
    val _ = funmap_remove<variable,a>(vmap, x)
  in
    vmap
  end

  implement find {a} (x, vmap) =
    funmap_search<variable,a>(vmap, x)
end