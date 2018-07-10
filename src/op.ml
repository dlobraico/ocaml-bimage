open Type
open Image

type ('a, 'b, 'c) t = int -> int -> int -> ('a, 'b, 'c) Image.t array -> float

let blend: ('a, 'b, 'c) t = fun x y c inputs ->
  let a = inputs.(0) in
  let b = inputs.(1) in
  get a x y c +. get b x y c

let max: ('a, 'b, 'c) t = fun x y c inputs ->
  let a = inputs.(0) in
  let b = inputs.(1) in
  max (get a x y c) (get b x y c)

let min: ('a, 'b, 'c) t = fun x y c inputs ->
  let a = inputs.(0) in
  let b = inputs.(1) in
  min (get a x y c) (get b x y c)

let grayscale: ('a, 'b, [< `Rgb | `Rgba]) t = fun x y _c inputs ->
  let a = inputs.(0) in
  (get a x y 0 *. 0.21)
  +. (get a x y 1 *. 0.72)
  +. (get a x y 2 *. 0.07)

let color: ('a, 'b, [`Gray]) t = fun x y _c inputs ->
  let a = inputs.(0) in
  get a x y 0

let eval op output (inputs: ('a, 'b, 'c) Image.t array) =
  let channels = channels output in
  let kind = kind output in
  let of_float = Kind.of_float kind in
  let clamp = Kind.clamp kind in
  let x = ref 0 in (* x index *)
  let y = ref 0 in (* y index *)
  let c = ref 0 in (* channel index *)
  for i = 0 to length output - 1 do
    let f = clamp (op !x !y !c inputs) in
    Bigarray.Array1.unsafe_set output.data i (of_float f);

    (* Increment channel index *)
    incr c;

    (* If channel index is greater than the number of channels
     * then reset channel index to 0 and increment x index *)
    let () = if !c = channels then
      let () = c := 0 in
      incr x
    in

    (* If x index is greater than the width then reset x index to 0
     * and increment y index *)
    if !x = output.width then
      let () = x := 0 in
      incr y
  done

let join f a b =
  fun x y c inputs ->
    f (a x y c inputs) (b x y c inputs)

let map a f =
  fun x y c inputs ->
    f (a x y c inputs)

let ( $ ) a f = map a f
let ( &+ ) a b = join (+.) a b
let ( &- ) a b = join (-.) a b
let ( &* ) a b = join ( *. ) a b
let ( &/ ) a b = join ( /.) a b

let scalar: float -> ('a, 'b, 'c) t = fun f _x _y _c _inputs -> f

let invert_f kind f =
  Kind.max_f kind -. f

let invert: ('a, 'b, 'c) t = fun x y c inputs ->
  let a = inputs.(0) in
  let kind = kind a in
  if c = 4 then get a x y c
  else Kind.max_f kind -. get a x y c

let filter_3x3: Kernel.t -> ('a, 'b, 'c) t = fun kernel ->
  let k00 = Kernel.get kernel 0 0 in
  let k10 = Kernel.get kernel 1 0 in
  let k20 = Kernel.get kernel 2 0 in
  let k01 = Kernel.get kernel 0 1 in
  let k11 = Kernel.get kernel 1 1 in
  let k21 = Kernel.get kernel 2 1 in
  let k02 = Kernel.get kernel 0 2 in
  let k12 = Kernel.get kernel 1 2 in
  let k22 = Kernel.get kernel 2 2 in
  fun x y c inputs ->
    let a = inputs.(0) in
    Kind.clamp (kind a)
      (get a (x - 1) (y - 1) c *. k00
       +. get a (x - 1) y c *. k10
       +. get a (x - 1) (y + 1) c *. k20
       +. get a x (y - 1) c *. k01
       +. get a x y c *. k11
       +. get a x (y + 1) c *. k21
       +. get a (x + 1) (y - 1) c *. k02
       +. get a (x + 1) y c *. k12
       +. get a (x + 1) (y + 1) c *. k22)

let filter: Kernel.t -> ('a, 'b, 'c) t = fun kernel ->
  let rows = Kernel.rows kernel in
  let cols = Kernel.cols kernel in
  let r2 = rows / 2 in
  let c2 = cols / 2 in
  if rows = 3 && cols = 3 then
    filter_3x3 kernel
  else
    fun x y c inputs ->
      let a = inputs.(0) in
      let f = ref 0.0 in
      for ky = -r2 to r2 do
        let kr = kernel.(ky + r2) in
        for kx = -c2 to c2 do
          f := !f +. (get a (x + kx) (y + ky) c *. kr.(kx + c2))
        done
      done;
      !f

let join_filter: (float -> float -> float ) -> Kernel.t -> Kernel.t -> ('a, 'b, 'c) t = fun fn kernel kernel2 ->
  let rows = Kernel.rows kernel in
  let cols = Kernel.cols kernel in
  let r2 = rows / 2 in
  let c2 = cols / 2 in
  fun x y c inputs ->
    let a = inputs.(0) in
    let f = ref 0.0 in
    for ky = -r2 to r2 do
      let kr = kernel.(ky + r2) in
      let kr2 = kernel2.(ky + r2) in
      for kx = -c2 to c2 do
        let v = get a (x + kx) (y + ky) c in
        f := !f +. fn (v *. kr.(kx + c2)) (v *. kr2.(kx + c2))
      done
    done;
    !f

let sobel_x: ('a, 'b, 'c) t = fun x y c inputs ->
  filter_3x3 Kernel.sobel_x x y c inputs

let sobel_y: ('a, 'b, 'c) t = fun x y c inputs ->
  filter_3x3 Kernel.sobel_y  x y c inputs

let sobel x y c inputs = join_filter ( +. ) Kernel.sobel_x Kernel.sobel_y x y c inputs [@@inline]

let gaussian ?std n x y z inputs = filter (Kernel.gaussian ?std n) x y z inputs

let transform t =
  fun x y c inputs ->
    let x = float_of_int x in
    let y = float_of_int y in
    let x', y' = Transform.transform t x y in
    let x', y' = int_of_float x', int_of_float y' in
    if x' >= 0 && y' >= 0 && x' < inputs.(0).width && y' < inputs.(0).height then
      get inputs.(0) x' y' c
    else 0.

let rotate ?center angle =
  let r = Transform.rotate ?center angle in
  transform r

let rotate' angle dw dh =
  let a = Util.Angle.to_radians angle in
  fun x y c inputs ->
    let input = inputs.(0) in
    let x = x + ((dw - dh) / 2) in
    let y = y + ((dh - dw) / 2) in
    let mid_x = float_of_int input.width /. 2. in
    let mid_y = float_of_int input.height /. 2. in
    let dx = float_of_int x +. 0.5 -. mid_x in
    let dy = float_of_int y +. 0.5 -. mid_y in
    let x' = int_of_float @@ mid_x +. dx *. cos a -. dy *. sin a in
    let y' = int_of_float @@ mid_y +. dx *. sin a +. dy *. cos a in
    if x' >= 0 && y' >= 0 && x' < input.width && y' < input.height then
      get inputs.(0) x' y' c
    else 0.

