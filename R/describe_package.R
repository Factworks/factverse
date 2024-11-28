#' describe_package
#'
#' Fetches the description of a package from GitHub.
#'
#' When using factverse, it can sometimes be difficult to figure out what each
#' of the packages does. This function fetches a description of the package from
#' GitHub.
#'
#' @param package_name name of the package
#' @param description_file name of the file that should be downloaded. By default,
#' factverse will first look for the README.md, and then for the DESCRIPTION.
#' @param gh_pat GitHub PAT to access the package list
#' @param packages list with the names of the packages for each repository that
#' are available
#' @importFrom base64enc base64decode
#' @export
#' @examples
#' \dontrun{
#' describe_package("testpackage")
#' }
describe_package <- function(package_name,
                             description_file = c("README.md", "DESCRIPTION"),
                             gh_pat = get_gh_pat(silent = TRUE),
                             packages = get_available_packages(gh_pat = get_gh_pat(silent = TRUE),
                                                               repository_name = "package_list",
                                                               file_path = "packages.yaml")){

  for(account in names(packages)){

    # Check if the package we are looking for
    # is found at that account.
    if(!package_name %in% packages[[account]]$packages)
      next

    for(descr_file in description_file){
      package_description <- try(gh::gh("GET /repos/{repository_owner}/{repository_name}/contents/{file_path}",
                                        repository_owner = account,
                                        repository_name = package_name,
                                        file_path = description_file,
                                        .token = gh_pat[[account]])[["content"]] |>
                                   base64enc::base64decode() |>
                                   rawToChar(),
                                 silent = TRUE)
      if(!is(package_description, "try-error")){
        break
      }
    }
    if(is(package_description, "try-error"))
      stop("Could not find a description for the package ", package_name, ".")

    cli::cli_inform(paste0("Here is the description for the package ", package_name, ":"))
    cat(package_description)

    return(invisible(NULL))

  }

  stop("Could not find the package ", package_name, ".")
}
