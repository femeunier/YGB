check_ed_metfile <- function(metfile, variables) {
  hfile <- hdf5r::H5File$new(metfile, mode = "r")
  # Remove variables that are not constants
  variables <- variables[variables$flag != 4, ]
  testthat::test_that(
    "All variables present in metfile",
    {
      testthat::expect_true(all(variables$variable %in% hfile$ls()$name))
    }
  )
}
