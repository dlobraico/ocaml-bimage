type 'a t =
  | Kernel : Kernel.t -> float t
  | Input : Input.index * int t * int t * int t -> float t
  | X : int t
  | Y : int t
  | C : int t
  | Int : int -> int t
  | Float : float -> float t
  | Bool : bool -> bool t
  | Float_of_int : int t -> float t
  | Int_of_float : float t -> int t
  | Fadd : float t * float t -> float t
  | Fsub : float t * float t -> float t
  | Fmul : float t * float t -> float t
  | Fdiv : float t * float t -> float t
  | Fpow : float t * float t -> float t
  | Fsqrt : float t -> float t
  | Fsin : float t -> float t
  | Fcos : float t -> float t
  | Ftan : float t -> float t
  | Fmod : float t * float t -> float t
  | Iadd : int t * int t -> int t
  | Isub : int t * int t -> int t
  | Imul : int t * int t -> int t
  | Idiv : int t * int t -> int t
  | Imod : int t * int t -> int t
  | Gt : 'a t * 'a t -> bool t
  | Eq : 'a t * 'a t -> bool t
  | Lt : 'a t * 'a t -> bool t
  | And : bool t * bool t -> bool t
  | Or : bool t * bool t -> bool t
  | Not : bool t -> bool t
  | Cond : bool t * 'a t * 'a t -> 'a t
  | Func : 'b t * (int -> int -> int -> 'b -> 'a) -> 'a t
  | Pixel : Input.index * int t * int t -> Pixel.t t
  | Pixel_norm : Input.index * int t * int t -> Pixel.t t
  | Value : 'a -> 'a t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | Kind_min : Input.index -> float t
  | Kind_max : Input.index -> float t
  | Channels : Input.index -> int t

let rec prepare :
    type a. int ref -> int ref -> int ref -> a t -> ('b, 'c, 'd) Input.t -> a =
 fun x y c expr inputs ->
   match expr with
   | Kernel k -> Kernel.op k inputs !x !y !c
   | Input (input, x', y', c') ->
       let x' = prepare x y c x' inputs in
       let y' = prepare x y c y' inputs in
       let c' = prepare c y c c' inputs in
       let f : float = Image.get_f (Input.get inputs input) x' y' c' in
       f
   | X -> !x
   | Y -> !y
   | C -> !c
   | Bool b -> b
   | Int i -> i
   | Float f -> f
   | Float_of_int i ->
       let a = prepare x y c i inputs in
       float_of_int a
   | Int_of_float f ->
       let a = prepare x y c f inputs in
       int_of_float a
   | Fadd (Kernel a, Kernel b) -> Kernel.join ( +. ) a b inputs !x !y !c
   | Fadd (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a +. b
   | Fsub (Kernel a, Kernel b) -> Kernel.join ( -. ) a b inputs !x !y !c
   | Fsub (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a -. b
   | Fmul (Kernel a, Kernel b) -> Kernel.join ( *. ) a b inputs !x !y !c
   | Fmul (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a *. b
   | Fdiv (Kernel a, Kernel b) -> Kernel.join ( /. ) a b inputs !x !y !c
   | Fdiv (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a /. b
   | Fpow (Kernel a, Kernel b) -> Kernel.join ( ** ) a b inputs !x !y !c
   | Fpow (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a ** b
   | Fsqrt a ->
       let a = prepare x y c a inputs in
       sqrt a
   | Fsin a ->
       let a = prepare x y c a inputs in
       sin a
   | Fcos a ->
       let a = prepare x y c a inputs in
       cos a
   | Ftan a ->
       let a = prepare x y c a inputs in
       tan a
   | Fmod (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       mod_float a b
   | Iadd (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a + b
   | Isub (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a - b
   | Imul (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a * b
   | Idiv (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a / b
   | Imod (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a mod b
   | Gt (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a > b
   | Eq (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a = b
   | Lt (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a < b
   | And (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a && b
   | Or (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       a || b
   | Not a ->
       let a = prepare x y c a inputs in
       not a
   | Cond (cond, a, b) ->
       let cond = prepare x y c cond inputs in
       if cond then prepare x y c a inputs else prepare x y c b inputs
   | Func (f, func) ->
       let x' = prepare x y c X inputs in
       let y' = prepare x y c Y inputs in
       let c' = prepare x y c C inputs in
       let f = prepare x y c f inputs in
       func x' y' c' f
   | Pixel (index, x', y') ->
       let x' = prepare x y c x' inputs in
       let y' = prepare x y c y' inputs in
       Image.get_pixel inputs.(index) x' y'
   | Pixel_norm (index, x', y') ->
       let x' = prepare x y c x' inputs in
       let y' = prepare x y c y' inputs in
       Image.get_pixel_norm inputs.(index) x' y'
   | Value x -> x
   | Pair (a, b) ->
       let a = prepare x y c a inputs in
       let b = prepare x y c b inputs in
       (a, b)
   | Kind_min index -> Image.kind inputs.(index) |> Type.Kind.min_f
   | Kind_max index -> Image.kind inputs.(index) |> Type.Kind.max_f
   | Channels index -> Image.channels inputs.(index)

let op ?(x = ref 0) ?(y = ref 0) ?(c = ref 0) body =
  let f = prepare x y c body in
  fun inputs x' y' c' ->
    x := x';
    y := y';
    c := c';
    f inputs

let int i = Int i

let int_of_float x = Int_of_float x

let float_of_int x = Float_of_int x

let float f = Float f

let x = X

let y = Y

let c = C

let kernel k = Kernel k

let func i f = Func (i, f)

let pair a b = Pair (a, b)

let pixel input x y = Pixel (input, x, y)

let pixel_norm input x y = Pixel_norm (input, x, y)

let kind_min input = Kind_min input

let kind_max input = Kind_max input

let channels input = Channels input

let value x = Value x

let input i x y c = Input (i, x, y, c)

let fadd a b = Fadd (a, b)

let fsub a b = Fsub (a, b)

let fdiv a b = Fdiv (a, b)

let fmul a b = Fmul (a, b)

let iadd a b = Iadd (a, b)

let isub a b = Isub (a, b)

let idiv a b = Idiv (a, b)

let imul a b = Imul (a, b)

let pow a b = Fpow (a, b)

let sqrt a = Fsqrt a

let sin a = Fsin a

let cos a = Fcos a

let tan a = Ftan a

let pi () = float Util.pi

let and_ a b = And (a, b)

let or_ a b = Or (a, b)

let not_ a = Not a

let cond v a b = Cond (v, a, b)

let blend a b : float t =
  func
    (pair (input a X Y C) (input b X Y C))
    (fun _ _ _ (a, b) -> (a +. b) /. 2.0)

let min a b : float t =
  func
    (pair (input a X Y C) (input b X Y C))
    (fun _ _ _ (a, b) -> if a < b then a else b)

let max a b : float t =
  func
    (pair (input a X Y C) (input b X Y C))
    (fun _ _ _ (a, b) -> if a > b then a else b)

let brightness i scale : float t =
  func (pair scale (input i X Y C)) (fun _ _ _ (scale, x) -> x *. scale)

let grayscale input : float t =
  func (pixel input X Y) (fun _ _ _ px ->
      let px = Pixel.data px in
      (px.{0} *. 0.21) +. (px.{1} *. 0.72) +. (px.{2} *. 0.07))

let color input : float t =
  func (pixel input X Y) (fun _ _ _ px ->
      let px = Pixel.data px in
      px.{0})

module Infix = struct
  let ( + ) a b = Iadd (a, b)

  let ( - ) a b = Isub (a, b)

  let ( * ) a b = Imul (a, b)

  let ( / ) a b = Idiv (a, b)

  let ( +. ) a b = Fadd (a, b)

  let ( -. ) a b = Fsub (a, b)

  let ( *. ) a b = Fmul (a, b)

  let ( /. ) a b = Fdiv (a, b)

  let ( ** ) a b = Fpow (a, b)
end

let kernel_3x3 ?(input = 0) kernel =
  let k00 = Kernel.get kernel 0 0 |> float in
  let k10 = Kernel.get kernel 1 0 |> float in
  let k20 = Kernel.get kernel 2 0 |> float in
  let k01 = Kernel.get kernel 0 1 |> float in
  let k11 = Kernel.get kernel 1 1 |> float in
  let k21 = Kernel.get kernel 2 1 |> float in
  let k02 = Kernel.get kernel 0 2 |> float in
  let k12 = Kernel.get kernel 1 2 |> float in
  let k22 = Kernel.get kernel 2 2 |> float in
  let open Infix in
  let get a b = Input (input, X + int a, Y + int b, C) in
  (get (-1) (-1) *. k00)
  +. (get (-1) 0 *. k10)
  +. (get (-1) 1 *. k20)
  +. (get 0 (-1) *. k01)
  +. (get 0 0 *. k11)
  +. (get 0 1 *. k21)
  +. (get 1 (-1) *. k02)
  +. (get 1 0 *. k12)
  +. (get 1 1 *. k22)
