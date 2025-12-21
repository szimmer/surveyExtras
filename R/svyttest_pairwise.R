#' Design-based pairwise two-sample t-test
#'
#' Calculate pairwise two-sample t-tests. This function is a wrapper for
#' svyby() and svycontrast() from the survey package
#'
#' @param formula One-sided formula indicating the outcome variable(s)
#' @param by One-sided formula indicating the by variable(s) - these are done jointly if more than one
#' @param design survey design object, could be subset
#' @param df.resid degrees of freedom for testing, defaults to degf(design)-2
#'
#' @returns a data.frame with the label of the comparison, the contrast estimate, the se of the contrast estimate, the t-value, the degrees of freedom, and the p-value
#'
#' @examples
#' data("api", package = "survey")
#' dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
#' svyttest_pairwise(~api00, ~stype, dclus1)
#' svyttest_pairwise(~api00, ~stype, dclus1, df.resid = 10)
#' svyttest_pairwise(~api00, ~ stype + comp.imp, dclus1)
#' svyttest_pairwise(~ I(api00 - api99), ~ stype + comp.imp, dclus1)
#' \dontrun{
#' svyttest_pairwise(~ api00 + api99, ~ stype + comp.imp, dclus1)
#' }
#' @export
svyttest_pairwise <- function(formula, by, design, df.resid = survey::degf(design) - 2) {
  if (!inherits(formula, "formula")) {
    stop("formula must be a formula")
  }
  if (length(labels(stats::terms(formula))) != 1) {
    stop("This function only works for one outcome variable. formula should only have one variable")
  }
  if (!inherits(by, "formula")) {
    stop("by must be a formula")
  }
  if (!inherits(design, "survey.design")) {
    stop("design must be a survey design object")
  }
  if (!(is.numeric(df.resid) && df.resid > 0)) {
    stop("df.resid must be a non-negative number")
  }

  means <- survey::svyby(formula, by, design, survey::svymean, covmat = TRUE)
  if (nrow(means) == 1) {
    stop("Your by variable only has one level, it should have at least 2.")
  }
  pairwise_combs <- utils::combn(nrow(means), 2)
  levs <- row.names(means)
  testi <- function(i) {
    contrast <- rep(0, nrow(means))
    contrast[pairwise_combs[, i]] <- c(1, -1)
    contlab <- paste(levs[pairwise_combs[, i]], collapse = " - ")
    cont <- survey::svycontrast(means, contrast)
    est <- as.numeric(stats::coef(cont))
    se <- as.numeric(survey::SE(cont))
    tval <- est / se
    p <- 2 * stats::pt(-abs(tval), df.resid)

    data.frame(label = contlab, contrast = est, se = se, t = tval, df.resid = df.resid, p = p)
  }

  if (nrow(means) > 2) {
    res <- lapply(seq_len(nrow(means)), testi)
    do.call("rbind", res)
  } else {
    testi(1)
  }
}
