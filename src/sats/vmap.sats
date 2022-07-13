#staload "./types.sats"

exception vmap_find of string

fun empty : {a:type} () -> vmap a
fun one : {a:type} (variable, a) -> vmap a
fun add : {a:type} (variable, a, vmap a) -> vmap a
fun find : {a:type} (variable, vmap a) -> Option_vt a
fun remove : {a:type} (variable, vmap a) -> vmap a