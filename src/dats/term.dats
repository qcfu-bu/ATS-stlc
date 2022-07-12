#include "share/atspre_staload.hats"
#staload "./../sats/variable.sats"
#staload _ = "./variable.dats"
#staload "./../sats/term.sats"
#define ATS_DYNLOADFLAG 0

local
  datatype abs_t = Abs of (variable, tm)
  assume abs = abs_t
in
  fun {a:type}findi_opt(f : a -<cloref1> bool, ls : List a) : Option_vt@([n:nat]int(n), a) = let
    fun aux{k:nat}(k : int(k), ls : List a) : Option_vt@([n:nat]int(n), a) =
      case ls of
      | nil() => None_vt
      | cons(h, t) =>
        if f(h) then
          Some_vt@(k, h)
        else
          aux(k + 1, t)
  in
    aux (0, ls)
  end
    
  fun bindn{k:nat}(k : int(k), xs : List0 variable, m : tm) = let
    fun aux{k:nat}(k : int(k), m : tm) =
      case m of
      | Var y => let
          val opt = findi_opt<variable>(lam x => equal(x, y), xs)
        in
          case opt of
          | ~Some_vt@(i, _) => Var (bound (i + k))
          | ~None_vt() => Var y
        end
      | Lam(a, Abs (x, b)) => let
          val b = aux (k + 1, b)
        in
          Lam (a, Abs(x, b))
        end
      | App(m, n) => let
          val m = aux (k, m)
          val n = aux (k, n)
        in
          App (m, n)
        end
  in
    aux (k, m)
  end

  fun unbindn{i:nat}(k : int(i), xs : List0 variable, m : tm) = let
    val sz = list_length xs
    fun aux{i:nat}(k : int(i), m : tm) =
      case m of
      | Var y => let
          val opt = is_bound (y, sz, k)
        in
          case opt of
          | Some i => Var (list_nth<variable>(xs, (i - k)))
          | _ => Var y
        end
      | Lam (a, Abs (x, b)) => let
          val b = aux (k + 1, b)
        in
          Lam (a, Abs (x, b))
        end
      | App (m, n) => let
          val m = aux (k, m)
          val n = aux (k, n)
        in
          App (m, n)
        end
  in
    aux (k, m)
  end

  implement bind(x, m) = Abs (x, bindn (0, $list (x), m))

  implement unbind(Abs (x, m)) = let
    val x = fresh (x)
  in
    (x, unbindn (0, $list (x), m))
  end

  implement msubst (vmap, m) : tm =
    case m of
    | Var y => (
        case find<tm>(y, vmap) of
        | ~Some_vt n => n
        | ~None_vt () => Var y)
    | Lam (a, abs) => let
        val (x, m) = unbind abs
        val m = msubst (vmap, m)
      in
        Lam (a, bind (x, m))
      end
    | App (m, n) => let
        val m = msubst (vmap, m)
        val n = msubst (vmap, n)
      in
        App (m, n)
      end

  implement subst (x, m, n) =
    case m of
    | Var y =>
      if equal (x, y) then
        n
      else
        m
    | Lam (a, abs) => let
        val (y, m) = unbind abs
        val m = subst (x, m, n)
      in
        Lam (a, bind (y, m))
      end
    | App (m1, m2) => let
        val m1 = subst (x, m1, n)
        val m2 = subst (x, m2, n)
      in
        App (m1, m2)
      end

  implement fprint_val<tm> = fprint_tm

  implement fprint_tm(out, m) =
    case m of
    | Var x => print!("Var(", x, ")")
    | Lam (a, abs) => let
        val (x, m) = unbind abs
      in
        print!("Lam(", x, ", ", m, ")")
      end
    | App (m, n) =>
      print!("App(", m, ", ", n, ")")

  implement print_tm m = fprint_tm(stdout_ref, m)
  implement prerr_tm m = fprint_tm(stderr_ref, m)
end