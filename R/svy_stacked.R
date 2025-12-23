#' Design-based test comparing subpopulations to a population
#'
#' Calculate a t-test comparing a subpopulation to the overall population.
#' For example, comparing a state estimate to the national or all adults to employed adults.
#'
#' @param data a data frame or tibble to look up formulas and necessary variables for design objects
#' @param svydestype a character string indicating the type of survey design object to create.
#' Options are "svydesign" or "svrepdesign".
#' @param svydesargs a list of arguments to pass to the survey design constructor function
#" [survey::svydesign()] or [survey::svrepdesign()]) - do NOT include the data.
#' @param outcome_variable an outcome variable, specified as a character
#' @param subpop_variable a variable, specified as character, which defines the subpopulation(s)
#' comparing to the full population
#' @param subset_statement (optional) any necessary subsetting to do before the analysis, specified
#' as a character (e.g., "mpg > 4")
#' @param dftype specify what type of degrees of freedom to use either specified as one of
#' c("entire", "full", "sub", "manual") or a specified positive number where "entire" uses the
#' degrees of freedom for the entire design, "full" uses the degrees of freedom for the design after
#' applying the subset, this will be the same as "entire" if there is no subset, "sub" which uses
#' the degrees of freedom after applying both the subset and the subpopulation or "manual" where the
#' degrees of freedom is specified in ddf
#' @param ddf a positive value for degrees of freedom if using manual degrees of freedom
#' @examples
#' data(api, package="survey")
#' svy_stacked(data = apistrat,
#'             svydestype = "svydesign",
#'             svydesargs = list(strata=~stype, weights=~pw),
#'             outcome_variable = "api00",
#'             subpop_variable = "stype"
#' )
#'
#' if (rlang::is_installed("srvyrexploR")){
#'   # Compare each northeastern state average energy expenditure to all the states in the northeast
#'   data(recs_2020, package="srvyrexploR")
#'   svy_stacked(
#'     data = recs_2020,
#'     svydestype = "svrepdesign",
#'     svydesargs = list(weights = ~NWEIGHT, repweights = "NWEIGHT[1-9]+",
#'                       type = "JK1", scale = 59/60, mse = TRUE),
#'     outcome_variable = "TOTALDOL",
#'     subpop_variable = "state_postal",
#'     subset_statement = "REGIONC == 'NORTHEAST'",
#'     dftype = "full"
#'   )
#' }
#' @export
svy_stacked <- function(
    data, svydestype = c("svydesign", "svrepdesign"), svydesargs, outcome_variable, subpop_variable,
    subset_statement = NULL, dftype = "sub", ddf = NULL) {
  # Validate inputs
  checkmate::expect_data_frame(data)
  checkmate::expect_choice(svydestype, c("svydesign", "svrepdesign"))
  checkmate::expect_list(svydesargs)
  if ("data" %in% names(svydesargs)) {
    stop("'data' must NOT be included in 'svydesargs' because it is passed as a separate object")
  }
  checkmate::expect_character(outcome_variable, len=1, any.missing = FALSE)
  checkmate::expect_subset(outcome_variable, names(data))
  checkmate::expect_character(subpop_variable, len=1, any.missing = FALSE)
  checkmate::expect_subset(subpop_variable, names(data))
  checkmate::expect_character(subset_statement, len=1, any.missing = FALSE, null.ok = TRUE)
  checkmate::expect_choice(dftype, c("entire", "full", "sub", "manual"))
  if (dftype=="manual"){
    checkmate::expect_number(ddf, lower=.000000001)
  } else{
    testthat::expect_null(ddf)
  }

  # Stack data and create a group variable

  groupvar <- make.names(c(names(data), ".group"), unique=TRUE)[ncol(data)+1]

  data_full <- data_sub <- data
  data_full[groupvar] <- "full"
  data_sub[groupvar] <- "sub"

  svydesargs$data <- rbind(data_full, data_sub)

  # Crate survey design object

  if (svydestype == "svydesign" && !any(c("ids", "id") %in% names(svydesargs))) {
    svydesargs[["ids"]] <- ~1
    message("ids set to ~1 for design as none is provided.")
  }

  stacked_des <-
    switch(svydestype,
           # as_survey_design = do.call(srvyr::as_survey_design, svydesargs),
           # as_survey_rep = do.call(srvyr::as_survey_rep, svydesargs),
           svydesign = do.call(survey::svydesign, svydesargs) |> srvyr::as_survey(),
           svrepdesign = do.call(survey::svrepdesign, svydesargs) |> srvyr::as_survey()
    )


  # Subset design if subset condition provided

  if (!is.null(subset_statement)) {
    subset_quote <- rlang::parse_expr(subset_statement)
    subset_des <- stacked_des |> srvyr::filter(!!subset_quote)
  } else {
    subset_des <- stacked_des
  }

  if (dftype == "entire") {
    dfuse <- survey::degf(stacked_des)
  } else if (dftype == "full") {
    dfuse <- survey::degf(subset_des)
  } else if (dftype == "sub") {
    dfuse <- NA
  } else if (dftype == "manual"){
    dfuse <- ddf
  }

  subtest <- function(subpopval) {
    subpopvalp <- ifelse(is.numeric(data[subpop_variable]), subpopval, paste0("'", subpopval, "'"))
    subset_statement <- paste0(groupvar, "==", "'full'|", subpop_variable, "==", subpopvalp)
    subset_quote <- rlang::parse_expr(subset_statement)
    subpop_des <- subset_des |> srvyr::filter(!!subset_quote)
    if (is.na(dfuse)) {
      subset_statement <- paste0(groupvar, "==", "'sub'")
      subset_quote <- rlang::parse_expr(subset_statement)
      calc_des <- subpop_des |> srvyr::filter(!!subset_quote)
      dfuse <- survey::degf(calc_des)
    }
    compform <- stats::as.formula(paste0(outcome_variable, "~", groupvar))
    tt_out <- survey::svyttest(compform, subpop_des) |> broom::tidy()
    data.frame(
      Group = subpopval,
      estimate = unname(tt_out$estimate),
      statistic = unname(tt_out$statistic),
      p.value = 2 * stats::pt(-abs(unname(tt_out$statistic)), dfuse),
      df = dfuse
    )
  }

  subpop_levs <- unique(subset_des$variables[[subpop_variable]]) |> sort()

  subpop_levs |>
    purrr::map(subtest) |>
    purrr::list_rbind()
}
