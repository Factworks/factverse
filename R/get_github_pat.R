#' get_R_directory
#'
#' Get the R directory where R packages are installed. By default, this
#' will look for the installation in AppData. If this is not found, it uses
#' the first result. If the directory is ~/AppData/Local/R/win-library/4.4,
#' this function will return AppData/Local/R/
#' @returns string with path to the R folder
#' @keywords internal
get_R_directory <- function(){
  paths <- .libPaths()
  if(any(grepl(pattern = "AppData", paths))){
    path <- paths[grepl(pattern = "AppData", paths)]
  }else{
    path <- paths[1]
  }
  # Only go to the R folder
  path <- gsub(pattern = "/R/.+$", replacement = "/R", x = path)
  return(path)
}

#' get_pat_file_path
#'
#' Retrieves the path of the file that stores the secrets (access tokens) for
#' GitHub.
#' @param folder_name specify the name of the folder in which the secrets file is stored
#' @param file_name specify the name of the file in which the secrets are stored
#' @returns the path of the file with the secrets
#' @keywords internal
get_pat_file_path <- function(folder_name = ".RSecrets",
                              file_name = "RSecrets.yaml"){
  return(paste0(get_R_directory(), "/", folder_name, "/", file_name))
}

#' get_gh_pat
#'
#' Retrieves the GitHub PAT from the local secrets to access the private GitHub
#' repositories. The file may contain multiple GitHub PATs, in which case the
#' first valid one is returned (the first one where the expiry date is in the future).
#' @param path_to_gh_pat path to the file containing the GitHub PAT
#' @param silent should the function inform about the successful retrieval of the API key?
#' @returns list with PATs
#' @importFrom yaml read_yaml
#' @import cli
#' @export
#' @examples
#' \dontrun{
#' library(factverse)
#' test_files <- system.file("extdata", package = "factverse")
#' test_yaml <- system.file("extdata",
#'                          "RSecret_Example.yaml",
#'                          package = "factverse")
#'
#' factverse::get_gh_pat(test_yaml)
#' }
get_gh_pat <- function(path_to_gh_pat = get_pat_file_path(),
                       silent = FALSE){

  if(!file.exists(path_to_gh_pat)){
    stop("Could not find the file with the GitHub key at ", path_to_gh_pat, ". Please contact IT.")
  }

  secrets <- yaml::read_yaml(file = path_to_gh_pat)
  gh_secrets <- secrets$GitHub

  # There can be multiple PATs. We will use the first one we find that is still valid
  get_first_valid <- function(pat){
    PAT <- pat$PAT
    for(pt in seq_along(PAT$Key)){
      if(PAT$Valid_until[pt] >= Sys.Date()){
        return(PAT$Key[pt])
      }
    }
  }

  PATs <- sapply(gh_secrets,
                 get_first_valid)

  PATs <- PATs[sapply(PATs, length) == 1]

  if(length(PATs) == 0)
    stop("Could not find a valid GitHub key! Please contact IT.")

  if(length(setdiff(names(gh_secrets), names(PATs))) > 0){
    cli::cli_alert_warning(paste0("Could not find valid Keys for the following accounts: ",
                                  paste0(setdiff(names(gh_secrets), names(PATs)),
                                         collapse = ","), "."))
  }

  class(PATs) <- "GHPAT"
  return(PATs)
}

#' new_PAT
#'
#' @param account_name name of the GitHub account
#' @param PAT PAT to access the GitHub account repositories
#' @returns returns an object of class GHPAT
#' @export
#' @examples
#' new_PAT(account_name = "MyAccount",
#'         PAT = "ksjg123")
new_PAT <- function(account_name,
                    PAT){
  PATs <- list()
  PATs[[account_name]] <- PAT
  class(PATs) <- "GHPAT"
  return(PATs)
}

