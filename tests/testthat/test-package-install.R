test_that("Checking package installation process", {
  library(factverse)

  # install outdated version of tidyr. We will check if the functions
  # correctly pick up on the outdated version and update everything
  remotes::install_version(package = "tidyr",
                           version = "1.2.0",
                           repos = "http://cran.us.r-project.org",
                           upgrade = "never")

  # we need to overwrite the functions that require api access for testing
  get_gh_pat <- function(silent = TRUE){return(list(tidyverse = NULL))}
  get_available_packages <- function(silent = TRUE){
    return(list(tidyverse = list(packages = c("dplyr", "tidyr"),
                                 essential = c("dplyr"))))
  }

  # check if essentials can be installed. This should install the latest version
  # of dplyr
  factverse::install_essentials(gh_pat = get_gh_pat(),
                                packages = get_available_packages())

  to_update <- check_for_updates(gh_pat = get_gh_pat(),
                                 packages = get_available_packages(),
                                 return_package_list = TRUE)
  testthat::expect_identical(to_update,
                             list(tidyverse = list(requires_update = c("tidyr"),
                                                   up_to_date = c("dplyr"))))

  # check that we can update the packages automatically
  factverse::update_packages(gh_pat = get_gh_pat(),
                             packages = get_available_packages())

  # ensure that all packages are now up to date
  to_update <- check_for_updates(gh_pat = get_gh_pat(),
                                 packages = get_available_packages(),
                                 return_package_list = TRUE)
  testthat::expect_identical(to_update,
                             list(tidyverse = list(requires_update = c(),
                                                   up_to_date = c("dplyr", "tidyr"))))
})
