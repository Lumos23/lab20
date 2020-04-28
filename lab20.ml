(*
                             CS51 Lab 20
                   Synthesis -- Digital Halftoning

                 Testing the gray-scale image module
 *)

open Image ;;

(* Display versions of a sample image, 8-bit Mona Lisa of size 250
   columns x 360 rows *)
let mona = create 250 360 Monalisa.image in
    (* show the original grayscale *)
    depict mona;
    (* ...and thresholded at 75% *)
    depict (threshold 0.75 mona);
    (* ...and dithered *)
    depict (dither mona);
    (* ...and error-diffused *)
    depict (error_diffuse mona) ;;

(*====================================================================
                              DISCUSSION

This "solution" to lab 20 is only one of a multitude of
possibilities. Yours may be quite different, perhaps much better! We
recommend comparing this version to the original lab20.ml.

We focused on the following issues, while leaving many other
alternatives unexplored. In rough order of decreasing importance:

  * Most importantly, separating out the image-processing
    functionality into a separate `Image` module, with a signature to
    enforce the abstraction. That change allows this file to simply
    open the `Image` module and run the test, as you see above. By
    enforcing the image type abstraction, alternative implementations
    are free to implement images with other data structures. (In
    particular, see the note about use of arrays below.)

  * Changing the image type to incorporate the extra information about
    sizes, which was otherwise strewn around the code. It's more
    natural to have the image itself store these aspects that affect
    the interpretation of the contents matrix.

  * Addition of a few operations to the Image module, for filtering
    and inverting images, as well as one-dimensional error diffusion
    as an additional digital halftoning method. These were mostly just
    for fun and to indicate the type of additional image manipulation
    functions that might appear in such a module, but the addition of
    filtering is of great general utility, including being used in
    abstracting the implementation of the digital halftoning
    functions.)

  * Rationalizing the orders of arguments to the functions, for
    consistency, but also with an eye toward partial application. The
    result can be seen in the use of `filter`. Since the image
    arguments are consistently placed as the final argument, the
    halftoning functions can use partial application of `filter` to
    simplify the presentation.

  * Addition of documentation throughout. Consistent documentation
    style, more coherent explanations, correct spelling, etc.

  * Consistency and quality of variable names: Using `col` and `row`
    consistently rather than `c`, `col`, `x`, etc. Consistent use of
    underscores.

  * Addition of lots of whitespace throughout and normalization of
    spacing and indentation, especially in the implementation of
    `depict`. (Standardizing the spacing in `threshold` and `dither`,
    just by itself, reveals the commonality of the two functions,
    leading to their abstraction using `filter`.)

  * Elimination of the `depict_pix` auxiliary function, which was only
    used once, and whose in-lining didn't increase the difficulty of
    understanding `depict`, especially after incorporating a suitable
    comment.

  * Addition of an auxiliary function `rgb_of_gray` to simplify and make
    more transparent the calculations being done in `depict`.

  * Fixing a bug in the call to `G.plot`, which had an off-by-one
    error that led to shifting the whole image up in the display by
    one pixel, leaving a single pixel white line at the bottom of the
    window. (Did you notice that?)

  * Introduction of some sanity checks for arguments. Still missing is
    checking the invariant that the size information in an image
    reflects the actual size of the image contents or the invariant
    that no pixel contains a value greater than that specified by the
    `values` field.

  * Use of a short synonym for the `Graphics` module and consistent
    use thereof.

  * Many other minor changes throughout. Eliminating warnings;
    judicious use of backwards application; eliminating magic numbers.

Unexplored alternatives and additions include:

  * Defining an `image` class instead of an `Image` module, again
    bringing the size information in as instance variables.

  * Implementing the contents using arrays instead of lists. This
    would allow random access to the pixels, making it possible to
    efficiently implement more general image transformations like
    convolutions or two-dimensional error diffusion. It would likely
    lead to changes in implementation technique for some functions:
    The `depict` function might change to use `for` loops. Decisions
    would need to be made as to whether the image tranformation
    functions would be destructive (changing the contents in place) or
    functional (returning new copies), which would have impact on the
    types of the functions.

  * Addition of a thorough unit testing file.

 *)
           
