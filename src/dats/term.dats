#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0

#staload "./../sats/types.sats"
#staload "./../sats/variable.sats"
#staload "./../sats/vmap.sats"
#staload "./../sats/term.sats"

#staload _ = "./variable.dats"
#staload _ = "./vmap.dats"

local
  datatype abs_t = Abs of (variable, tm)
  assume abs_boxed = abs_t
in
  fun {a:type}findi_opt(f : a -<cloref1> bool, ls : List a) : Option_vt@([n:nat]int(n), a) = let
    fun aux{k:nat}(k : int k, ls : List a) : Option_vt@([n:nat]int n, a) =
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
    
  fun bindn{k:nat}(k : int k, xs : List0 variable, m : tm) = let
    fun aux{k:nat}(k : int k, m : tm) =
      case m of
      | Var y => let
          val opt = findi_opt<variable>(lam x => x = y, xs)
        in
          case opt of
          | ~Some_vt@(i, _) => Var (bound (i + k))
          | ~None_vt() => Var y
        end
      | Lam(a, Abs (x, b)) => let
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

  fun unbindn{i:nat}(k : int i, xs : List0 variable, m : tm) = let
    val sz = list_length xs
    fun aux{i:nat}(k : int i, m : tm) =
      case m of
      | Var y => let
          val opt = is_bound (y, sz, k)
        in
          case opt of
          | ~Some_vt i => Var (list_nth<variable>(xs, i - k))
          | ~None_vt () => Var y
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
        case find (y, vmap) of
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
    msubst (one (x, n), m)
end

implement eval m =
  case m of
  | Var _ => m
  | Lam (a, abs) => let 
      val (x, m) = unbind abs
      val m = eval m
    in
      Lam (a, bind (x, m))
    end
  | App (m, n) => let
      val m = eval m
      val n = eval n
    in
      case m of 
      | Lam (_, abs) => let
          val (x, m) = unbind abs
        in
          eval (subst (x, m, n))
        end
      | _ => App (m, n)
    end

implement equal_ty (a, b) =
  case (a, b) of
  | (Prim a, Prim b) => a = b
  | (Arrow (a1, b1), Arrow (a2, b2)) =>
    equal_ty (a1, a2) && equal_ty (b1, b2)
  | (_, _) => false

implement infer (ctx, m) =
  case m of
  | Var x => find (x, ctx)
  | Lam (a, abs) => let
      val (x, m) = unbind abs 
      val ctx = add (x, a, ctx)
      val opt = infer (ctx, m)
    in
      case opt of
      | ~None_vt () => None_vt ()
      | ~Some_vt b => Some_vt (Arrow (a, b))
    end
  | App (m, n) => let
      val m_ty = infer (ctx, m)
      val n_ty = infer (ctx, n)
    in
      case (m_ty, n_ty) of
      | (~Some_vt a, ~Some_vt a') => (
        case a of
        | Arrow (a, b) =>
          if equal_ty (a, a') then
            Some_vt b
          else
            None_vt
        | _ => None_vt)
      | (_, _) => let 
          val _ = option_vt_free m_ty
          val _ = option_vt_free n_ty
        in
          None_vt
        end
    end
  
implement fprint_val<ty> = fprint_ty

implement fprint_ty (out, a) =
  case a of
  | Prim a => print!("Prim(", a, ")")
  | Arrow (a, b) => print!("Arrow(", a, ", ", b, ")")

implement print_ty a = fprint_ty (stdout_ref, a)
implement prerr_ty a = fprint_ty (stderr_ref, a)

implement fprint_val<tm> = fprint_tm

implement fprint_tm (out, m) =
  case m of
  | Var x => print!("Var(", x, ")")
  | Lam (a, abs) => let
      val (x, m) = unbind abs
    in
      print!("Lam(", x, ", ", m, ")")
    end
  | App (m, n) =>
    print!("App(", m, ", ", n, ")")

implement print_tm m = fprint_tm (stdout_ref, m)
implement prerr_tm m = fprint_tm (stderr_ref, m)