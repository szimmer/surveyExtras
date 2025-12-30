# Design-based test comparing subpopulations to a population

Calculate a t-test comparing a subpopulation to the overall population.
For example, comparing a state estimate to the national or all adults to
employed adults.

## Usage

``` r
svy_stacked(
  data,
  svydestype = c("as_survey_design", "as_survey_rep", "svydesign", "svrepdesign"),
  outcome_variable,
  subpop_variable,
  subset_statement = NULL,
  dftype = "sub",
  ddf = NULL,
  ...
)
```

## Arguments

- data:

  a data frame or tibble to look up formulas and necessary variables for
  design objects

- svydestype:

  a character string indicating the type of survey design object to
  create. Options are "svydesign" or "svrepdesign".

- outcome_variable:

  an outcome variable, specified as a character

- subpop_variable:

  a variable, specified as character, which defines the subpopulation(s)
  comparing to the full population

- subset_statement:

  (optional) any necessary subsetting to do before the analysis,
  specified as a character (e.g., "mpg \> 4")

- dftype:

  specify what type of degrees of freedom to use either specified as one
  of c("entire", "full", "sub", "manual") or a specified positive number
  where "entire" uses the degrees of freedom for the entire design,
  "full" uses the degrees of freedom for the design after applying the
  subset, this will be the same as "entire" if there is no subset, "sub"
  which uses the degrees of freedom after applying both the subset and
  the subpopulation or "manual" where the degrees of freedom is
  specified in ddf

- ddf:

  a positive value for degrees of freedom if using manual degrees of
  freedom

- ...:

  arguments to pass to the survey design constructor function such as
  weights, strata, and cluster ids. See
  [`srvyr::as_survey_design()`](http://gdfe.co/srvyr/reference/as_survey_design.md),
  [`srvyr::as_survey_rep()`](http://gdfe.co/srvyr/reference/as_survey_rep.md),
  [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html),
  or
  [`survey::svrepdesign()`](https://rdrr.io/pkg/survey/man/svrepdesign.html))
  for more detail - do NOT include the data.

## Examples

``` r
data(api, package = "survey")
svy_stacked(
  data = apistrat,
  svydestype = "svydesign",
  outcome_variable = "api00",
  subpop_variable = "stype",
  dftype = "sub",
  strata = ~stype,
  weights = ~pw,
  ids = ~1
)
#> Loading required namespace: testthat
#>   Group  estimate  statistic    p.value df
#> 1     E  12.14264  0.7721989 0.44183616 99
#> 2     H -36.46736 -2.0144633 0.04946667 49
#> 3     M -25.68736 -1.3447476 0.18489860 49

svy_stacked(
  data = apistrat,
  svydestype = "as_survey_design",
  outcome_variable = "api00",
  subpop_variable = "stype",
  dftype = "sub",
  strata = stype,
  weights = pw
)
#>   Group  estimate  statistic    p.value df
#> 1     E  12.14264  0.7721989 0.44183616 99
#> 2     H -36.46736 -2.0144633 0.04946667 49
#> 3     M -25.68736 -1.3447476 0.18489860 49

if (rlang::is_installed("srvyrexploR")) {
  # Compare each northeastern state average energy expenditure to all the states in the northeast
  data(recs_2020, package = "srvyrexploR")
  svy_stacked(
    data = recs_2020,
    svydestype = "as_survey_rep",
    outcome_variable = "TOTALDOL",
    subpop_variable = "state_postal",
    subset_statement = "REGIONC == 'NORTHEAST'",
    dftype = "full",
    weights = NWEIGHT,
    repweights = NWEIGHT1:NWEIGHT60,
    type = "JK1",
    scale = 59 / 60,
    mse = TRUE
  )
}
#>   Group   estimate  statistic      p.value df
#> 1    CT  544.62109  5.6341492 5.187204e-07 59
#> 2    ME   41.46504  0.4896011 6.262317e-01 59
#> 3    MA  190.41422  3.7962617 3.489782e-04 59
#> 4    NH  267.07308  2.7024978 8.973627e-03 59
#> 5    NJ -228.59735 -4.1848175 9.626176e-05 59
#> 6    NY   56.68835  1.4410281 1.548632e-01 59
#> 7    PA -248.17423 -6.0496585 1.069521e-07 59
#> 8    RI  354.21725  3.4053146 1.194475e-03 59
#> 9    VT  144.34982  1.8167289 7.433902e-02 59
```
