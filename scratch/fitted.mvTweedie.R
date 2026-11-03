
#' @method fitted mvtweedie
#' @export
fitted.mvtweedie <-
function(x,
                  ... )
{
  predict(x, ..., newdata=NULL)
}
