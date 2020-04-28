(* 
                     Gray scale image processing

See image.mli for more complete documentation.
 *)

module G = Graphics ;;
let cMAXRGB = 255 ;;    (* maximum RGB channel value in Graphics module *)
  
(* Pixels are floats in the range [0..1] where 0=white and 1=black. *)
type pixel = float ;;
  
(* Size is number of columns by number of rows *)
type size = int * int ;;
  
(* Each `image` stores the size along with the contents, as a list of
   rows (from top to bottom), each a list of column pixels (from left
   to right) 
 *)
type image = { size : size;
               content : float list list } ;;
               
(*....................................................................
  Utilities
 *)

(* rgb_of_gray value -- Returns an rgb color as for the `Graphics`
   module corresponding to a gray value of `value`, where `1.0` is
   black and `0.0` is white. Raises `Failure` if pixel value is out of
   range.
 *)
let rgb_of_gray (value : pixel) : G.color =
  if value > 1. || value < 0. then
    failwith "rgb_of_gray: value outside of range"
  else
    let level = int_of_float (float_of_int cMAXRGB *. (1. -. value)) in
    G.rgb level level level ;;

(*....................................................................
  Basic image functions -- creation, depiction, filtering
 *)

(* create col_size row_size contents -- Create an `image` whose values
   are in the range [0..1], with size given by `col_size` and
   `row_size`, and content as provided in `contents`. 
 *)
let create (col_size : int) (row_size : int)
           (contents : float list list)
         : image =
  {size = col_size, row_size;
   content = contents} ;;
  
(* depict img -- Presents `img` in an OCaml graphics window and waits
   for a short period to exit the window.
 *)
let depict ({size = col_size, row_size; content} : image) : unit =
  try
    (* prepare the graphics canvas *)
    G.open_graph "";
    G.clear_graph ();
    G.resize_window col_size row_size;
    
    (* draw each pixel *)
    content
    |> List.iteri (* for each row in the image content *)
         (fun row_index row ->
          row
          |> List.iteri (* for each pixel in the row *)
               (fun col_index pixel ->
                (* draw the pixel at the coordinates; note Graphics
                   module coordinates starts at lower left, so need to
		   invert the row index *)
                G.set_color (rgb_of_gray pixel);
                G.plot col_index (row_size - row_index - 1)));
    
    (* wait for a couple of seconds *)
    Unix.sleep 2
	       
  with
    (* make sure to close window if things go wrong *)
    exn -> (G.close_graph (); raise exn) ;;

(* filter f img -- Returns an image where each pixel is the
   application of `f` to the corresponding pixel in `img`.
 *)
let filter (f : pixel -> pixel)
           ({content; _} as img : image)
         : image =
  {img with content = List.map (List.map f) content} ;;

(*....................................................................
  Various image transformations -- inversion, digital halftoning
 *)

(* invert img -- Returns an image where the light to dark values in
   `img` have been inverted.  *)
let invert : image -> image =
  filter (fun p -> (1. -. p)) ;;

(* threshold img -- Digitally halftones an image into a binary image
   by thresholding each pixel at the given `threshold`, interpreted as
   a fraction (between 0 and 1) of the value space.  
 *)
let threshold (threshold : float) : image -> image =
  filter
    (fun p -> if threshold < p then 1. else 0.) ;;
         
(* dither img -- Digitally halftones an image into a binary image by
   making a pixel black randomly in proportion to its gray level.
 *)
let dither : image -> image =
  filter
    (fun p -> if Random.float 1. < p then 1. else 0.) ;;

(* error_diffuse img -- Digitally halftones `img` into a binary image
   by one-dimensional error diffusion. Relies on the fact that
   `filter` visits the pixels in a consistent row-by-row left-to-right
   order. See
   https://en.wikipedia.org/wiki/Error_diffusion#One-dimensional_error_diffusion
 *)
let error_diffuse : image -> image =
  filter
    (let error = ref 0. in
     fun p -> if 0.5 < p +. !error then
                (error := !error -. (1. -. p);
                 1.)
              else
                (error := !error -. (0. -. p);
                 0.)) ;;

