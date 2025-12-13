# Design-based pairwise two-sample t-test

Calculate pairwise two-sample t-tests. This function is a wrapper for
svyby() and svycontrast() from the survey package

## Usage

``` r
svyttest_pairwise(formula, by, design, df.resid = survey::degf(design) - 2)
```

## Arguments

- formula:

  One-sided formula indicating the outcome variable(s)

- by:

  One-sided formula indicating the by variable(s) - these are done
  jointly if more than one

- design:

  survey design object, could be subset

- df.resid:

  degrees of freedom for testing, defaults to degf(design)-2

## Value

a data.frame with the label of the comparison, the contrast estimate,
the se of the contrast estimate, the t-value, the degrees of freedom,
and the p-value

## Examples

``` r
data("api", package = "survey")
dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
svyttest_pairwise(~api00, ~stype, dclus1)
#>   label  contrast       se          t df.resid         p
#> 1 E - H  30.29663 30.02761  1.0089591       12 0.3329065
#> 2 E - M  17.42806 14.33739  1.2155668       12 0.2475237
#> 3 H - M -12.86857 33.58624 -0.3831501       12 0.7083096
svyttest_pairwise(~api00, ~stype, dclus1, df.resid = 10)
#>   label  contrast       se          t df.resid         p
#> 1 E - H  30.29663 30.02761  1.0089591       10 0.3367839
#> 2 E - M  17.42806 14.33739  1.2155668       10 0.2520677
#> 3 H - M -12.86857 33.58624 -0.3831501       10 0.7096274
svyttest_pairwise(~api00, ~ stype + comp.imp, dclus1)
#>          label   contrast       se            t df.resid         p
#> 1  E.No - H.No  -5.209821 24.81091 -0.209981084       12 0.8372053
#> 2  E.No - M.No   0.218750 46.99656  0.004654596       12 0.9963627
#> 3 E.No - E.Yes -21.406250 26.93534 -0.794727310       12 0.4422070
#> 4 E.No - H.Yes  32.504464 45.80736  0.709590411       12 0.4915166
#> 5 E.No - M.Yes   1.218750 28.61238  0.042595204       12 0.9667248
#> 6  H.No - M.No   5.428571 43.42031  0.125023777       12 0.9025748
svyttest_pairwise(~ I(api00 - api99), ~ stype + comp.imp, dclus1)
#>          label   contrast       se          t df.resid            p
#> 1  E.No - H.No   1.584821 5.846628  0.2710659       12 7.909468e-01
#> 2  E.No - M.No   9.610795 5.804971  1.6556146       12 1.236967e-01
#> 3 E.No - E.Yes -38.468750 3.955185 -9.7261560       12 4.830301e-07
#> 4 E.No - H.Yes -24.986607 4.625839 -5.4015300       12 1.596874e-04
#> 5 E.No - M.Yes -28.415179 6.420579 -4.4256409       12 8.273769e-04
#> 6  H.No - M.No   8.025974 7.285225  1.1016783       12 2.922080e-01
if (FALSE) { # \dontrun{
svyttest_pairwise(~ api00 + api99, ~ stype + comp.imp, dclus1)
} # }
```
