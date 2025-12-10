


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

  # x, as in this is x of y
  # y, how many in this colour group?

  #  a function to turn a color name into hex, including alpha.
  # https://gist.github.com/mbannert/e9fcfa86de3b06068c83
  col2hex <- function(col, alpha) {

    rgb(t(col2rgb(col)), alpha = alpha, maxColorValue = 255)

  }

  adj <- (y - x + 1) / y
  adj <- scales::rescale(adj, to = c(0.3, 1), from = c(0, 1))

  col2hex(rlang::ensym(colour)
          , alpha = 255 * adj
          )
}
