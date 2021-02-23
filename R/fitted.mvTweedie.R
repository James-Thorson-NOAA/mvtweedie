
#' @method fitted mvTweedie
#' @export
fitted.mvTweedie <-
function(x,
                  ... )
{
  predict(x, ..., newdata=NULL)
}
