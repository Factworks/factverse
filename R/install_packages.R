#' get_available_packages
#'
#' Retrieves the names of all available packages for factverse
#' @param gh_pat GitHub PAT to access the package list. This PAT should be found
#' automatically from a local file with secrets. If not, you can also pass a custom
#' PAT to the function. Create the PAT with new_PAT.
#' @param repository_name name of the repository that contains the package list
#' @param file_path path to the yaml file in the repository that contains the package list
#' @returns list with available packages
#' @import gh
#' @import curl
#' @importFrom base64enc base64decode
#' @importFrom methods is
#' @export
#' @examples
#' \dontrun{
#' library(factverse)
#' get_available_packages()
#' }
get_available_packages <- function(gh_pat = get_gh_pat(silent = TRUE),
                                   repository_name = "package_list",
                                   file_path = "packages.yaml"){

  available_packages <- list()
  for(account in names(gh_pat)){
    available_packages[[account]] <- try(gh::gh("GET /repos/{repository_owner}/{repository_name}/contents/{file_path}",
                                                repository_owner = account,
                                                repository_name = repository_name,
                                                file_path = file_path,
                                                .token = gh_pat[[account]])[["content"]] |>
                                           base64enc::base64decode() |>
                                           rawToChar() |>
                                           yaml::read_yaml(text = _),
                                         silent = TRUE)
    if(is(available_packages[[account]], "try-error")){
      cli::cli_alert_warning(paste0("Could not find a package list for ", account, "."))
      available_packages[[account]] <- NULL
    }
  }
  available_packages <- available_packages[sapply(available_packages, length) > 0]



  class(available_packages) <- "AvailablePackages"

  return(available_packages)
}

#' print.AvailablePackages
#'
#' Print the output from get_available_packages
#' @param x output from get_available_packages
#' @param ... not used
#' @importFrom utils installed.packages
#' @import cli
#' @export
#' @examples
#' \dontrun{
#' library(factverse)
#' get_available_packages()
#' }
print.AvailablePackages <- function(x, ...){

  installed_packages <- rownames(installed.packages())

  for(account in names(x)){
    pkg_installed <- x[[account]]$packages[x[[account]]$packages %in% installed_packages]
    pkg_not_installed <- x[[account]]$packages[!x[[account]]$packages %in% installed_packages]
    if(length(pkg_installed) > 0){
      cli::cli_inform(message = paste0("Packages available from ", account, " that are already installed:"))
      for(pkg in pkg_installed){
        cli::cli_alert_success(pkg)
      }
    }

    if(length(pkg_not_installed) > 0){
      cli::cli_inform(message = paste0("Packages available from ", account, " that are not installed:"))
      for(pkg in pkg_not_installed){
        cli::cli_alert_danger(pkg)
      }
    }
  }
}

#' check_for_updates
#'
#' Checks if there are updates for any of the factverse packages.
#'
#' @param gh_pat GitHub PAT to access the package list
#' @param packages list with the names of the packages for each repository that
#' are available
#' @param return_package_list should the list of packages be returned?
#' @returns (optional) a list with outdated and up to date packages
#' @import cli
#' @importFrom utils compareVersion
#' @importFrom utils installed.packages
#' @importFrom methods is
#' @export
#' @examples
#' \dontrun{
#' library(factverse)
#' check_for_updates()
#' }
check_for_updates <- function(gh_pat = get_gh_pat(silent = TRUE),
                              packages = get_available_packages(gh_pat = get_gh_pat(silent = TRUE),
                                                                repository_name = "package_list",
                                                                file_path = "packages.yaml"),
                              return_package_list = FALSE){


  installed_packages <- installed.packages()

  result <- list()

  for(account in names(packages)){
    result[[account]] <- list(requires_update = c(),
                              up_to_date = c())

    for(pkg in packages[[account]]$packages[packages[[account]]$packages %in% rownames(installed_packages)]){
      # get the latest version from GitHub
      lastest_version <- try(get_latest_package_sha(gh_pat = gh_pat,
                                                    repository_owner = account,
                                                    repository_name = pkg),
                             silent = TRUE)
      if(is(lastest_version, "try-error")){
        cli::cli_alert_info(paste0("Could not find a version on GitHub for ",
                                   pkg,
                                   ". Try factverse::install_package(\"", pkg, "\")."))
        next
      }

      installed_version <- try(get_package_sha(pkg),
                               silent = TRUE)
      if(is(installed_version, "try-error")){
        cli::cli_alert_info(paste0("Could not find a SHA version in the package description of ",
                                   pkg,
                                   ". Did you install the package locally? ",
                                   "Please install the latest version ",
                                   "with factverse::install_package(\"", pkg, "\")."))
        next
      }

      if(lastest_version != installed_version){
        result[[account]]$requires_update <- c(result[[account]]$requires_update, pkg)
        cli::cli_alert_warning(text = paste0("A newer version for ",
                                             pkg,
                                             " is available. Install the latest version with factverse::install_package(\"",
                                             pkg, "\")."))
      }else{
        result[[account]]$up_to_date <- c(result[[account]]$up_to_date, pkg)
        cli::cli_alert_success(text = paste0("Your version of ", pkg, " is up to date!"))
      }
    }
  }

  if(return_package_list)
    return(result)
}

#' get_latest_package_sha
#'
#' Retrieves the latest sha of an R package from the main branch.
#'
#' @param gh_pat GitHub PAT to access the package list
#' @param repository_owner name of the owner of the repository on GitHub
#' @param repository_name name of the repository that contains the package list
#' @import gh
#' @importFrom methods is
#' @keywords internal
get_latest_package_sha <- function(gh_pat = get_gh_pat(silent = TRUE),
                                   repository_owner,
                                   repository_name){

  for(branch in c("main", "master")){
    package_sha <- try(gh::gh("GET /repos/{repository_owner}/{repository_name}/git/refs/heads/{branch}",
                              repository_owner = repository_owner,
                              repository_name = repository_name,
                              branch = branch,
                              .token = gh_pat[[repository_owner]])$object$sha)
    if(!is(package_sha, "try-error")){
      return(package_sha)
    }
  }
  stop("Could not find a version for ", repository_name, ". Try factverse::install_package(\"", repository_name, "\").")
}

#' get_package_sha
#'
#' Retrieves the sha of an installed R package.
#'
#' @param package_name name of the package
#' @importFrom utils packageDescription
#' @keywords internal
#' @returns the SHA of the package
get_package_sha <- function(package_name){

  desc <- packageDescription(package_name)
  if(is.null(desc$RemoteSha))
    stop("Could not find a version for ", package_name, ". Did you install the package locally? ",
         "Please install the latest version ",
         "with factverse::install_package(\"", package_name, "\").")
  return(desc$RemoteSha)
}


#' install_package
#'
#' Installs a package from a private GitHub repository.
#'
#' @param package_name name of the package that should be installed
#' @param gh_pat GitHub PAT to access the package list
#' @param packages list with the names of the packages for each repository that
#' are available
#' @param ... additional arguments passed to pak::pkg_install()
#' @import pak
#' @import withr
#' @export
#' @examples
#' \dontrun{
#' library(factverse)
#' install_package("factworks")
#' }
install_package <- function(package_name,
                            gh_pat = get_gh_pat(silent = TRUE),
                            packages = get_available_packages(gh_pat = get_gh_pat(silent = TRUE),
                                                              repository_name = "package_list",
                                                              file_path = "packages.yaml"),
                            ...){

  if(length(package_name) != 1)
    stop("You cannot use this function to install multiple package. Please specify only one package name.")
  account <- sapply(packages, function(x) package_name %in% x$packages)
  account <- names(account)[account]
  if(length(account) != 1)
    stop(paste0("Could not find an account with the following package: ", package_name, "."))

  withr::with_envvar(new = c("GITHUB_PAT" = gh_pat[[account]]),
                     pak::pkg_install(pkg = paste0(account, "/", package_name)))

}

#' install_essentials
#'
#' Installs all essential packages (core packages) indicated in a package list.
#'
#' @param gh_pat GitHub PAT to access the package list
#' @param packages list with the names of the packages for each repository that
#' are available
#' @param ... additional arguments passed to pak::pkg_install()
#' @import cli
#' @export
#' @examples
#' \dontrun{
#' library(factverse)
#' install_essentials()
#' }
install_essentials <- function(gh_pat = get_gh_pat(silent = TRUE),
                               packages = get_available_packages(gh_pat = get_gh_pat(silent = TRUE),
                                                                 repository_name = "package_list",
                                                                 file_path = "packages.yaml"),
                               ...){

  for(account in names(packages)){
    for(essential in packages[[account]]$essential){
      cli::cli_inform(paste0("Installing ", essential, ":"))
      install_package(package_name = essential,
                      gh_pat = gh_pat,
                      packages = packages,
                      ... = ...)
    }
  }
}

#' update_packages
#'
#' Updates all installed packages with the latest version.
#'
#' @param gh_pat GitHub PAT to access the package list
#' @param packages list with the names of the packages for each repository that
#' are available
#' @param ... additional arguments passed to pak::pkg_install()
#' @export
#' @examples
#' \dontrun{
#' library(factworks)
#' update_packages()
#' }
update_packages <- function(gh_pat = get_gh_pat(silent = TRUE),
                            packages = get_available_packages(gh_pat = get_gh_pat(silent = TRUE),
                                                              repository_name = "package_list",
                                                              file_path = "packages.yaml"),
                            ...){

  pkg_list <- check_for_updates(gh_pat = gh_pat,
                                packages = packages,
                                return_package_list = TRUE)

  for(account in names(pkg_list)){
    for(pkg in pkg_list[[account]]$requires_update){
      cli::cli_alert_info(text = paste0("Installing the latest version of ", pkg))
      install_package(package_name = pkg,
                      gh_pat = gh_pat,
                      packages = packages,
                      ... = ...)
    }
  }
}
