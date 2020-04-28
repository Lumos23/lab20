(* 
                     Gray scale image processing

This module provides for a set of operations on grayscale images, including

   depicting      displaying images in a graphics window
   filtering      applying a filter function to each pixel in the image
   inverting      inverting the gray levels black <-> white
   thresholding   digital halftoning by thresholding each pixel at a
                  fixed percentage
   dithering      digital halftoning by probabilistic conversion
 *)

type image ;;

(* create col_size row_size contents -- Create an image whose pixel
   values are floats in the range [0..1], with 0=white and
   1=black. Size of the image is given by `col_size` and `row_size`,
   and contents as a list of rows (from top to bottom), each a list of
   column pixels (from left to right), whose values are floats in the
   proper range. *)
val create : int -> int -> float list list -> image ;;
  
(* depict img -- Presents the `img` in an OCaml graphics window and
   waits for a few seconds before exiting the window. *)
val depict : image -> unit ;;

(* filter f img -- Returns an image where each pixel is the
   application of `f` to the corresponding pixel in `img`. *)
val filter : (float -> float) -> image -> image ;;

(* invert img -- Returns an image where the light to dark values in
   `img` have been inverted. *)
val invert : image -> image ;;

(* threshold img -- Digitally halftones `img` into a binary image
   by thresholding each pixel at the given threshold, interpreted as a
   fraction (between 0 and 1) of the value space. *)
val threshold : float -> image -> image ;;

(* dither img -- Digitally halftones `img` into a binary image by
   making a pixel black randomly in proportion to its gray level. *)
val dither : image -> image ;;

(* error_diffuse img -- Digitally halftones `img` into a binary image
   by error diffusion. *)
val error_diffuse : image -> image ;;
