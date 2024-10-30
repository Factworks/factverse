test_that("Reading GH PAT Works", {

  # Locate the test files
  test_files <- system.file("extdata", package = "factverse")
  test_yaml <- system.file("extdata",
                           "RSecret_Example.yaml",
                           package = "factverse")

  dir.create(".RSecrets")
  file.copy(from = test_yaml,
            to = paste0(getwd(), "/.RSecrets/RSecrets.yaml"))

  path_to_pat <- factverse:::get_pat_file_path()

  testthat::expect_true(path_to_pat == paste0(getwd(), "/.RSecrets/RSecrets.yaml"))

  # Locate the test files
  test_files <- system.file("extdata", package = "factverse")
  test_yaml <- system.file("extdata",
                           "RSecret_Example.yaml",
                           package = "factverse")

  valid_pat <- get_gh_pat(path_to_gh_pat = test_yaml)

  testthat::expect_true(is(valid_pat, "GHPAT"))

  testthat::expect_true(names(valid_pat) == "Account_name_1")

  testthat::expect_true(valid_pat$Account_name_1 == "5d6g7h8")

  test_invlaid_yaml <- system.file("extdata",
                                   "RSecret_Invalid_Example.yaml",
                                   package = "factverse")

  testthat::expect_error(get_gh_pat(test_invlaid_yaml))

})
