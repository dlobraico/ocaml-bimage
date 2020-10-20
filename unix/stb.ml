open Ctypes
open Foreign

external _stbi_load_stub : unit -> unit = "stbi_load"

external _stbi_load_from_memory_stub : unit -> unit = "stbi_load_from_memory"

external _stbi_load16_stub : unit -> unit = "stbi_load_16"

external _stbi_load_16_from_memory_stub : unit -> unit
  = "stbi_load_16_from_memory"

external _stbi_loadf_stub : unit -> unit = "stbi_loadf"

external _stbi_loadf_from_memory_stub : unit -> unit = "stbi_loadf_from_memory"

external _stbi_write_png : unit -> unit = "stbi_write_png"

external _stbi_write_jpg : unit -> unit = "stbi_write_jpg"

external _stbi_write_jpg_to_func : unit -> unit = "stbi_write_jpg_to_func"

external _stbi_write_hdr : unit -> unit = "stbi_write_hdr"

let free = foreign "free" (ptr void @-> returning void)

let stbi_load_u8 =
  foreign ~release_runtime_lock:true "stbi_load"
    ( string @-> ptr int @-> ptr int @-> ptr int @-> int
    @-> returning (ptr uint8_t) )

let stbi_load_u16 =
  foreign ~release_runtime_lock:true "stbi_load_16"
    ( string @-> ptr int @-> ptr int @-> ptr int @-> int
    @-> returning (ptr uint16_t) )

let stbi_load_u16_from_memory =
  foreign ~release_runtime_lock:true "stbi_load_16_from_memory"
    ( string @-> ptr int @-> ptr int @-> ptr int @-> int
    @-> returning (ptr uint16_t) )

let stbi_load_u8_from_memory =
  foreign ~release_runtime_lock:true "stbi_load_from_memory"
    ( string @-> ptr int @-> ptr int @-> ptr int @-> int
    @-> returning (ptr uint8_t) )

let stbi_load_f32_from_memory =
  foreign ~release_runtime_lock:true "stbi_loadf_from_memory"
    ( string @-> ptr int @-> ptr int @-> ptr int @-> int
    @-> returning (ptr float) )

let stbi_load_f =
  foreign ~release_runtime_lock:true "stbi_loadf"
    ( string @-> ptr int @-> ptr int @-> ptr int @-> int
    @-> returning (ptr float) )

let read f a b c color filename =
  let width = allocate int 0 in
  let height = allocate int 0 in
  let channels = allocate int 0 in
  let n = Bimage.Color.channels color in
  let data = f filename width height channels n in
  if is_null data then
    Error (`Msg (Printf.sprintf "unable to open image: %s" filename))
  else
    let data = coerce (ptr a) (ptr b) data in
    let data' =
      Ctypes.bigarray_of_ptr array1 (!@width * !@height * !@channels) c data
    in
    let im =
      Bimage.Image.of_data color !@width !@height Bimage.Image.Interleaved data'
    in
    let () = Gc.finalise (fun _ -> free (coerce (ptr b) (ptr void) data)) im in
    Ok im

let read_from_memory f a b c color data =
  let width = allocate int 0 in
  let height = allocate int 0 in
  let channels = allocate int 0 in
  let n = Bimage.Color.channels color in
  let data = f data width height channels n in
  if is_null data then Error (`Msg "unable to decode image")
  else
    let data = coerce (ptr a) (ptr b) data in
    let data' =
      Ctypes.bigarray_of_ptr array1 (!@width * !@height * !@channels) c data
    in
    let im =
      Bimage.Image.of_data color !@width !@height Bimage.Image.Interleaved data'
    in
    let () = Gc.finalise (fun _ -> free (coerce (ptr b) (ptr void) data)) im in
    Ok im

let read_u16_from_memory color data =
  read_from_memory stbi_load_u16_from_memory uint16_t int Bimage.u16 color
    (Bytes.to_string data)

let read_u8 color filename =
  read stbi_load_u8 uint8_t int Bimage.u8 color filename

let read_u8_from_memory color data =
  read_from_memory stbi_load_u8_from_memory uint8_t int Bimage.u8 color
    (Bytes.to_string data)

let read_u16 color filename =
  read stbi_load_u16 uint16_t int Bimage.u16 color filename

let read_f32 color filename =
  read stbi_load_f float float Bimage.f32 color filename

let read_f32_from_memory color data =
  read_from_memory stbi_load_f32_from_memory float float Bimage.f32 color
    (Bytes.to_string data)

let read kind color filename =
  match read_u16 color filename with
  | Error e -> Error e
  | Ok tmp -> Ok (Bimage.Image.convert kind tmp)

let read_from_memory kind color filename =
  match read_u16_from_memory color filename with
  | Error e -> Error e
  | Ok tmp -> Ok (Bimage.Image.convert kind tmp)

let stbi_write_png =
  foreign ~release_runtime_lock:true "stbi_write_png"
    (string @-> int @-> int @-> int @-> ptr int @-> int @-> returning int)

let stbi_write_jpg =
  foreign ~release_runtime_lock:true "stbi_write_jpg"
    (string @-> int @-> int @-> int @-> ptr int @-> int @-> returning int)

let stbi_write_hdr =
  foreign ~release_runtime_lock:true "stbi_write_hdr"
    (string @-> int @-> int @-> int @-> ptr float @-> returning int)

module Stbi_write_func =
(val dynamic_funptr (ptr void @-> ptr int @-> int @-> returning void))

type context = unit ptr

let context : context typ = ptr void

let stbi_write_png_to_func =
  foreign "stbi_write_png_to_func"
    ( Stbi_write_func.t @-> context @-> int @-> int @-> int @-> ptr int @-> int
    @-> returning int )

let stbi_write_jpg_to_func =
  foreign "stbi_write_jpg_to_func"
    ( Stbi_write_func.t @-> context @-> int @-> int @-> int @-> ptr int @-> int
    @-> returning int )

let stbi_write_hdr_to_func =
  foreign "stbi_write_hdr_to_func"
    ( Stbi_write_func.t @-> context @-> int @-> int @-> int @-> ptr float
    @-> returning int )

let write_png filename image =
  let image =
    match image.Bimage.Image.layout with
    | Planar -> Bimage.Image.convert_layout Interleaved image
    | Interleaved -> image
  in
  let width, height, channels = Bimage.Image.shape image in
  let ptr = Ctypes.bigarray_start array1 (Bimage.Image.data image) in
  if stbi_write_png filename width height channels ptr (width * channels) = 0
  then Error (`Msg (Printf.sprintf "unable to load image: %s" filename))
  else Ok ()

let write_jpg ?(quality = 95) filename image =
  let image =
    match image.Bimage.Image.layout with
    | Planar -> Bimage.Image.convert_layout Interleaved image
    | Interleaved -> image
  in
  let width, height, channels = Bimage.Image.shape image in
  let ptr = Ctypes.bigarray_start array1 (Bimage.Image.data image) in
  if stbi_write_jpg filename width height channels ptr quality = 0 then
    Error (`Msg (Printf.sprintf "unable to load image: %s" filename))
  else Ok ()

let write_jpg_f ?(quality = 95) ~f image =
  let image =
    match image.Bimage.Image.layout with
    | Planar -> Bimage.Image.convert_layout Interleaved image
    | Interleaved -> image
  in
  let width, height, channels = Bimage.Image.shape image in
  let (ptr : int ptr) =
    Ctypes.bigarray_start array1 (Bimage.Image.data image)
  in
  let write_f _context data size =
    (* CR dlobraico: FIXME *)
    f (bigarray_of_ptr array1 size (Bimage.Image.kind image) data) size
  in
  Stbi_write_func.with_fun write_f (fun write_f_fptr ->
      let context = Ctypes.null in
      if
        stbi_write_jpg_to_func write_f_fptr context width height channels
          (ptr : int ptr)
          quality
        = 0
      then Error (`Msg (Printf.sprintf "unable to write image"))
      else Ok ())

let write_hdr filename image =
  let image =
    match image.Bimage.Image.layout with
    | Planar -> Bimage.Image.convert_layout Interleaved image
    | Interleaved -> image
  in
  let width, height, channels = Bimage.Image.shape image in
  let ptr = Ctypes.bigarray_start array1 (Bimage.Image.data image) in
  if stbi_write_hdr filename width height channels ptr = 0 then
    Error (`Msg (Printf.sprintf "unable to load image: %s" filename))
  else Ok ()

let write filename image =
  match Filename.extension filename |> String.lowercase_ascii with
  | ".png" ->
      let tmp = Bimage.Image.convert Bimage.u8 image in
      write_png filename tmp
  | ".jpeg" | ".jpg" ->
      let tmp = Bimage.Image.convert Bimage.u8 image in
      write_jpg filename tmp
  | ".hdr" ->
      let tmp = Bimage.Image.convert Bimage.f32 image in
      write_hdr filename tmp
  | ext ->
      Error
        (`Msg
          (Printf.sprintf "invalid file extension for writing image: %s" ext))
