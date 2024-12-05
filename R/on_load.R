#' .onAttach
#'
#' This function is executed every time library(factverse) is called.
#' It just looks fancy.
#' @param libname not used
#' @param pkgname not used
#' @importFrom utils packageVersion
#' @returns nothing
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("
  ___              __
.'  _|.---.-.----.|  |_.--.--.-----.----.-----.-----.
|   _||  _  |  __||   _|  |  |  -__|   _|__ --|  -__|
|__|  |___._|____||____|\\___/|_____|__| |_____|_____|

",
             "Version ",
             packageVersion("factverse")),
      "\n")
}
