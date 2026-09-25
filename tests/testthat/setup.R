# Declared explicitly so renv's dependency scan records testthat: the tests
# call test_that() without naming the package, and the command that runs them
# lives in the CI workflow, which renv does not scan (MODEL-LOG L030).
library(testthat)

suppressPackageStartupMessages({
  for (f in c("utils.R", setdiff(list.files(file.path("..", "..", "R")), "utils.R"))) {
    source(file.path("..", "..", "R", f))
  }
})
