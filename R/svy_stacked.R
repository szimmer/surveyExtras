#' Design-based test comparing subpopulations to a population
#'
#' Calculate a t-test comparing a subpopulation to the overall population.
#' For example, comparing a state estimate to the national or all adults to employed adults.
#'
#' @param data a data frame or tibble to look up formulas and necessary variables for design objects
#' @param svydestype a character string indicating the type of survey design object to create. Options are "svydesign" or "svrepdesign".
#' @param svydesargs a list of arguments to pass to the survey design constructor function [survey::svydesign()] or [survey::svrepdesign()]) - do NOT include the data.
#' @param outcome_variable an outcome variable, specified as a character, which is what the test is comparing
#' @param subpop_variable a variable, specified as character, which defines the subpopulation(s) comparing to the full population
#' @param subset_statement (optional) any necessary subsetting to do before the analysis, specified as a character (e.g., "mpg > 4")
#' @param dftype specify what type of degrees of freedom to use either specified as one of c("entire", "full", "sub") or a specified positive number where "entire" uses the degrees of freedom for the entire design, "full" uses the degrees of freedom for the design after applying the subset, this will be the same as "entire" if there is no subset, or "sub" which uses the degrees of freedom after applying both the subset and the subpopulation. Otherwise, if a number is specified, that is used as the degrees of freedom in all tests.
svy_stacked <- function(data, svydestype = c("svydesign", "svrepdesign"), svydesargs, outcome_variable, subpop_variable, subset_statement = NULL, dftype = "sub") {
  if (!is.data.frame(data)) {
    stop("data must be a data frame or tibble")
  }
  if (!(svydestype %in% c("as_survey_design", "as_survey_rep", "svydesign", "svrepdesign"))) {
    stop('svydestype must be either "svydesign" or "svrepdesign"')
  }
  if (!is.list(svydesargs)) {
    stop("svydesargs must be a list")
  }
  if ("data" %in% names(svydesargs)) {
    stop("data must NOT be included in svydesargs, it is passed as a separate object")
  }
  if (!is.character(outcome_variable) || length(outcome_variable) != 1 || !outcome_variable %in% names(data)) {
    stop("outcome variable must be ONE variable on the input data")
  }
  if (!is.character(subpop_variable) || length(subpop_variable) != 1 || !subpop_variable %in% names(data)) {
    stop("subpop variable must be ONE variable on the input data")
  }
  if (!is.null(subset_statement)) {
    if (!is.character(subset_statement) || length(subset_statement) != 1) {
      stop("subset_statement must be a character variable")
    }
  }

  ok_df_char <- is.character(dftype) && length(dftype) == 1 && dftype %in% c("entire", "full", "sub")
  ok_df_num <- is.numeric(dftype) && length(dftype) == 1 && dftype > 0
  if (!(ok_df_char | ok_df_num)) {
    stop("dftpe must either be one of 'entire', 'full', or 'sub' or a positive number")
  }

  groupvar <- ".group"
  cnt <- 0
  while (groupvar %in% names(data)) {
    cnt <- cnt + 1
    groupvar <- paste0(".group", cnt)
  }

  data_full <- data_sub <- data
  data_full[groupvar] <- "full"
  data_sub[groupvar] <- "sub"

  svydesargs$data <- rbind(data_full, data_sub)

  if (!any(c("ids", "id") %in% names(svydesargs))) {
    svydesargs[["ids"]] <- ~1
    message("ids set to ~1 for design as none is provided.")
  }

  stacked_des <-
    switch(svydestype,
      # as_survey_design = do.call(srvyr::as_survey_design, svydesargs),
      # as_survey_rep = do.call(srvyr::as_survey_rep, svydesargs),
      svydesign = do.call(survey::svydesign, svydesargs) |> srvyr::as_survey_design(),
      svrepdesign = do.call(survey::svrepdesign, svydesargs) |> srvyr::as_survey_design(),
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
  } else {
    dfuse <- dftype
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

  subpop_levs <- unique(subset_des$variables[[subpop_variable]])

  subpop_levs |>
    purrr::map(subtest) |>
    purrr::list_rbind()
}
