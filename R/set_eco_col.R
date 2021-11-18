


#' Set colours for individual ecosystems, using gradations within ecotype
#'
#' @param colour Colour for ecotype.
#' @param x As in this is ecosystem `x` of `y` in this ecotype.
#' @param y There are `y` ecosystems in total for this ecotype.
#'
#' @return hex colour
#' @export
#'
#' @examples
set_eco_col <- function(colour, x, y) {

  if(is.na(colour)) {

    colour <- "pink"
    x <- 1
    y <- 1

  }

  colour <- ensym(colour)

  # x, as in this is x of y
  # y, how many in this colour group?

  #  a function to turn a color name into hex, including alpha.
  # https://gist.github.com/mbannert/e9fcfa86de3b06068c83
  col2hex <- function(col, alpha) {

    rgb(t(col2rgb(col)), alpha=alpha, maxColorValue=255)

  }

  if(y > 1) {

    pal <- colortools::sequential(color = colour
                                  , percentage = 100*(1/((1+ 1/3)*y))
                                  , alpha = 1
                                  , plot = FALSE
                                  )

    tail(pal,y)[x]

  } else {

    col2hex(colour)

  }


}
