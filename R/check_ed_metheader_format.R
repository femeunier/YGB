check_ed_metheader_format <- function(ed_metheader_format, check_files = TRUE) {
  testthat::test_that(
    "Format has the correct names",
    {
      correct_names <- c("path_prefix", "nlon", "nlat", "dx", "dy", "xmin", "ymin", "variables")
      all(names(ed_metheader_format) %in% correct_names)
    }
  )
  testthat::test_that(
    "ED met header files exist and are not empty",
    {
      met_files <- PEcAn.utils::match_file(ed_metheader_format$path_prefix)
      testthat::expect_gte(length(met_files), 1)
      testthat::expect_true(all(file.exists(met_files)))
      testthat::expect_true(all(file.size(met_files) > 0))
    }
  )

  testthat::test_that(
    "Met header metadata fields are valid",
    {
      testthat::expect_true(is.numeric(ed_metheader_format$nlon))
      testthat::expect_true(is.numeric(ed_metheader_format$nlat))
      testthat::expect_true(is.numeric(ed_metheader_format$dx))
      testthat::expect_true(is.numeric(ed_metheader_format$dy))
      testthat::expect_true(is.numeric(ed_metheader_format$xmin))
      testthat::expect_true(is.numeric(ed_metheader_format$ymin))
      testthat::expect_is(ed_metheader_format$variables, "data.frame")
    }
  )

  if (check_files) {
    met_files <- PEcAn.utils::match_file(ed_metheader_format$path_prefix, suffix = "h5")
    .z <- lapply(met_files, check_ed_metfile, variables = ed_metheader_format$variables)
  }
}
