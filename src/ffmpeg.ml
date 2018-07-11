type t = {
  filename: string;
  width: int;
  height: int;
  frames: int;
  mutable index: int;
}

let frames {frames; _} = frames
let index {index; _} = index
let shape {width; height; _} = width, height

let reset t =
  t.index <- 0

let get_num_frames filename =
  let proc = Unix.open_process_in ("ffprobe -v error -hide_banner -count_frames -select_streams v:0 -show_entries stream=nb_frames -of default=nokey=1:noprint_wrappers=1 " ^ filename) in
  let frames = input_line proc in
  close_in proc;
  int_of_string frames

let get_size filename =
  let proc = Unix.open_process_in ("ffprobe -v error -select_streams v:0 -show_entries stream=width,height -of csv=s=x:p=0 "  ^ filename) in
  let size = input_line proc in
  let shape = String.split_on_char 'x' size in
  match shape with
  | x::y::_ -> int_of_string x, int_of_string y
  | _ -> Error.exc `Invalid_shape

let load filename =
  let width, height = get_size filename in
  {
		filename;
    width;
    height;
		frames = get_num_frames filename;
    index = 0 ;
	}

let set_index t f =
  t.index <- f

let skip t f =
  t.index <- t.index + f

let next t =
  if t.index >= t.frames then None
  else
    try
      let open Type in
      let proc = Unix.open_process_in (Printf.sprintf "ffmpeg  -v error -hide_banner -i %s -vf 'select=gte(n\\,%d)' -vframes 1 -pix_fmt rgb24 -f rawvideo -an -" t.filename t.index) in
      let img = Image.create u8 rgb t.width t.height in
      for i = 0 to Image.length img - 1 do
        img.data.{i} <- input_byte proc;
      done;
      close_in proc;
      t.index <- t.index + 1;
      Some img
    with _ -> None