test_that("Checking package installation process", {
  library(factverse)

  # install outdated version of tidyr. We will check if the functions
  # correctly pick up on the outdated version and update everything
  pak::pkg_install(pkg = "tidyverse/dplyr@v1.1.2",
                   ask = FALSE,
                   upgrade = FALSE)
  pak::pkg_install(pkg = "tidyverse/tibble",
                   ask = FALSE,
                   upgrade = FALSE)

  # we need to overwrite the functions that require api access for testing
  get_gh_pat <- function(silent = TRUE){return(list(tidyverse = NULL,
                                                    someotheraccount = "kjsdfgkljh"))}
  get_available_packages <- function(silent = TRUE){
    return(list(tidyverse = list(packages = c("dplyr", "tibble"),
                                 essential = c("tibble"))))
  }

  # Check if we can get the package description
  descr <- factverse::describe_package(package_name = "dplyr",
                                       gh_pat = get_gh_pat(),
                                       packages = get_available_packages())


  # check if essentials can be installed. This should install the latest version
  # of crayon
  factverse::install_essentials(gh_pat = get_gh_pat(),
                                packages = get_available_packages())

  to_update <- factverse::check_for_updates(gh_pat = get_gh_pat(),
                                            packages = get_available_packages(),
                                            return_package_list = TRUE)
  testthat::expect_identical(to_update,
                             list(tidyverse = list(requires_update = c("dplyr"),
                                                   up_to_date = c("tibble"))))



  # check that we can update the packages automatically
  factverse::update_packages(gh_pat = get_gh_pat(),
                             packages = get_available_packages())

  # ensure that all packages are now up to date
  to_update <- check_for_updates(gh_pat = get_gh_pat(),
                                 packages = get_available_packages(),
                                 return_package_list = TRUE)
  testthat::expect_identical(to_update,
                             list(tidyverse = list(requires_update = c(),
                                                   up_to_date = c("dplyr", "tibble"))))

  testthat::expect_message(factverse::install_essentials(gh_pat = get_gh_pat(),
                                                         packages = get_available_packages()))

  # check that we can pass on arguments to pkg
  factverse::install_essentials(gh_pat = get_gh_pat(),
                                packages = get_available_packages(),
                                upgrade = TRUE)
  testthat::expect_message(factverse::install_package(package_name = "tibble",
                                                      gh_pat = get_gh_pat(),
                                                      packages = get_available_packages()))


  factverse::install_package(package_name = "tibble",
                             gh_pat = get_gh_pat(),
                             packages = get_available_packages(),
                             upgrade = TRUE)

})
