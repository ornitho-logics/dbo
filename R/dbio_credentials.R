#' Manage database credentials in a my.cnf file
#'
#' Create (if missing), open for editing, or delete a `my.cnf`
#' containing database credential groups for different hosts.
#'
#' The location of the config file is read from `options(dbo.my.cnf)`.
#'
#' @param delete Logical scalar. If `TRUE`, deletes the config file at
#'   `getOption("dbo.my.cnf")`. Defaults to `FALSE`.
#'
#' @return Invisibly returns the config file path.
#'
#' @note
#' If you also use a `my.cnf` with CLI tools that support `--defaults-group-suffix`,
#' ensure any `[name]` group is duplicated as `[clientname]` so both group names work.
#'
#' @export 
#' 
my.cnf <- function(delete = FALSE) {

  path = getOption("dbo.my.cnf")

  if(!delete) {
    if(!file.exists(path)){
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

      file.copy(system.file("my.cnf", package = "dbo"), path)

    }

    if(interactive()) {
      message("Fill in one or several configuration groups and save.")
      file.edit(path)
    }

  }

  if(delete) file.remove(path)



}