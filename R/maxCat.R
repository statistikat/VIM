#' Aggregation function for a factor variable
#'
#' The function maxCat chooses the level
#' with the most occurrences and random if the maximum is not unique.
#'
#' @param x factor vector
#' @param weights numeric vector providing weights for the observations in x
#' @export


maxCat <- function(x,weights = NULL){
  #choose cat with max prob, random if max is not unique
  is_logical <- is.logical(x)
  if(!is.factor(x))
    x <- as.factor(x)
  s <- category_scores(x, weights)
  if(sum(s>0)>1)
    s <- sample(s)
  if (is_logical)
    as.logical(names(s)[which.max(s)])
  else
    names(s)[which.max(s)]
}

category_scores <- function(x, weights) {
  if (!is.null(weights) && is.vector(weights, mode = "numeric") &&
      length(weights) == length(x) && length(x) > 0L &&
      all(is.finite(weights)) && is.null(dim(x)) &&
      (identical(class(x), "factor") || identical(class(x), c("ordered", "factor"))) &&
      !anyNA(x) && !anyNA(levels(x)) && nlevels(x) <= 100L) {
    # Match the merge's label order, and use sum() on each group in
    # donor order so that the probabilities and RNG behavior are preserved.
    groups <- split(weights, x, drop = TRUE)
    labels <- names(groups)
    totals <- unlist(lapply(groups, sum), use.names = FALSE)
    scores <- setNames(lengths(groups, use.names = FALSE) * totals, labels)
    return(scores[sort.list(labels)])
  }

  # Keep summary.factor's behavior for missing values and collapsed levels,
  # and the original aggregate behavior for other weight types and lengths.
  scores <- summary(x)
  scores <- scores[scores != 0]
  if (!is.null(weights)) {
    grouped <- merge(aggregate(weights, list(x), sum),
                     data.frame(Group.1 = names(scores), prob = scores))
    scores <- setNames(grouped$prob * grouped$x, grouped$Group.1)
  }
  scores
}
