#' Design-based test comparing subpopulations to a population
#'
#' Calculate a t-test comparing a subpopulation to the overall population.
#' For example, comparing a state estimate to the national or all adults to employed adults.
#'
#' @param data a data frame or tibble to look up formulas and necessary variables for design objects
#' @param svydestype a character string indicating the type of survey design object to create.
#' Options are "svydesign" or "svrepdesign".
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
#' @param ... arguments to pass to the survey design constructor function such as weights, strata,
#' and cluster ids. See [srvyr::as_survey_design()], [srvyr::as_survey_rep()],
#' [survey::svydesign()], or [survey::svrepdesign()]) for more detail - do NOT include the data.
#' @examples
#' data(api, package="survey")
#' svy_stacked(data = apistrat,
#'             svydestype = "svydesign",
#'             outcome_variable = "api00",
#'             subpop_variable = "stype",
#'             dftype = "sub",
#'             strata = ~stype,
#'             weights = ~pw,
#'             ids = ~1
#' )
#'
#' svy_stacked(data = apistrat,
#'             svydestype = "as_survey_design",
#'             outcome_variable = "api00",
#'             subpop_variable = "stype",
#'             dftype = "sub",
#'             strata = stype,
#'             weights = pw
#' )
#'
#' if (rlang::is_installed("srvyrexploR")){
#'   # Compare each northeastern state average energy expenditure to all the states in the northeast
#'   data(recs_2020, package="srvyrexploR")
#'   svy_stacked(
#'     data = recs_2020,
#'     svydestype = "as_survey_rep",
#'     outcome_variable = "TOTALDOL",
#'     subpop_variable = "state_postal",
#'     subset_statement = "REGIONC == 'NORTHEAST'",
#'     dftype = "full",
#'     weights = NWEIGHT,
#'     repweights = NWEIGHT1:NWEIGHT60,
#'     type = "JK1",
#'     scale = 59/60,
#'     mse = TRUE
#'   )
#' }
#' @export
svy_stacked <- function(
    data, svydestype = c("as_survey_design", "as_survey_rep", "svydesign", "svrepdesign"),
    outcome_variable, subpop_variable, subset_statement = NULL, dftype = "sub", ddf = NULL, ...) {
  # Validate inputs
  checkmate::expect_data_frame(data)
  svydestype <- match.arg(svydestype)
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

  data_stacked <- rbind(data_full, data_sub)

  # Crate survey design object

  stacked_des <-
    switch(svydestype,
           as_survey_design = srvyr::as_survey_design(.data=data_stacked, ...),
           as_survey_rep = srvyr::as_survey_rep(.data=data_stacked, ...),
           svydesign = survey::svydesign(data=data_stacked, ...) |> srvyr::as_survey(),
           svrepdesign = survey::svrepdesign(data=data_stacked, ...) |> srvyr::as_survey()
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
