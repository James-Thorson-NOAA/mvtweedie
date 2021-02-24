
#' Get predictions from a multivariate logit interpretation of a Tweedie GLM
#'
#' It returns a tibble with predicted proportions from a Tweedie GAM model.
#'
#' The created tibble can then be plotted using \code{ggplot2}
#'
#' @export
predict_mvTweedie <-
function(
                 model,
                 exclude_terms = NULL,
                 length_out = 50,
                 values = NULL,
                 ... )
{
  if( !any(c("gam","glmmTMB") %in% class(x)) ){
    stop("`predict_mvTweedie` only implemented for mgcv and glmmTMB")
  }
  n_terms <- length(model[["var.summary"]])
  term_list <- list()
  for (term in 1:n_terms) {
    term_summary <- model[["var.summary"]][[term]]
    term_name <- names(model[["var.summary"]])[term]
    if (term_name %in% names(values)) {
      new_term <- values[[which(names(values) == term_name)]]
      if (is.null(new_term)) {
        new_term <- model[["var.summary"]][[term]][[1]]
      }
    }
    else {
      if (is.numeric(term_summary)) {
        min_value <- min(term_summary)
        max_value <- max(term_summary)
        new_term <- seq(min_value, max_value, length.out = length_out)
      }
      else if (is.factor(term_summary)) {
        new_term <- levels(term_summary)
      }
      else {
        stop("The terms are not numeric or factor.\n")
      }
    }
    term_list <- append(term_list, list(new_term))
    names(term_list)[term] <- term_name
  }
  new_data <- expand.grid(term_list)
  class(model) = c( "mvTweedie", class(model) )
  pred <- predict( model,
                   newdata = new_data,
                   se.fit = TRUE,
                   #original_class = c("gam","glm","lm"),
                   ...)
  predicted <- as.data.frame(pred)
  predictions <- cbind(new_data, predicted)
  predictions <- tibble::as_tibble(predictions)
  return(predictions)
}
