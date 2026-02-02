#' Connect to a database
#'
#' `dbcon()` creates a DBI connection. By default it tries servers in order and
#' uses credentials and defaults from the `dbo` package options.
#'
#' Connection parameters are taken from:
#' - `options(dbo.my.cnf)` as `default.file` (for login groups)
#' - `options(dbo.tz)` as `timezone`
#'
#' @param server Character vector of server (group) names in the `my.cnf` file.
#'   Servers are tried in order until a connection succeeds.
#' @param db Optional database name to `USE` after connecting.
#' @param driver Driver identifier. Currently supports `"MariaDB"` (default).
#' @param ... Further arguments forwarded to [DBI::dbConnect()].
#'
#' @return A DBI connection object.
#' @export
#'
#' @seealso [DBI::dbConnect()], [DBI::dbDisconnect()]
#'
#' @note
#' `timezone` and `default.file` are set via `options()` (typically at package
#' startup). The selected `server` entry is passed as `group`.
#'

dbcon <- function(server = c("scidb","scidb_replica"), db , driver = "MariaDB") {


  if( driver ==  "MariaDB" ) {
    con = dbConnect(
      drv          = RMariaDB::MariaDB(),
      timezone     = getOption("dbo.tz")  , 
      default.file = getOption('dbo.my.cnf'), 
      group        = server[1],
      timeout      = 3600
    ) |> try(silent = TRUE)

    if(length(server)> 1 && inherits(con, "try-error")){
      warning(glue("Connection to {dQuote(server[1])} failed, using { dQuote(server[2]) }."))

    con <- dbConnect(
      drv          = RMariaDB::MariaDB(),
      timezone     = getOption("dbo.tz"),
      default.file = getOption('dbo.my.cnf'),
      group        = server[2],
      timeout      = 3600
    )


    }

    if (!missing(db)) {
      dbExecute(con, glue("USE {db}"))
    }

    }

    con

  }

#' @rdname dbcon
#' @param con    The connection object.
#' @export
setGeneric("closeCon", function(con)   standardGeneric("closeCon") )


#' @export
#' @rdname dbcon
setMethod("closeCon",
          signature  = c(con = "MariaDBConnection"),
          definition = function(con) {
      dbDisconnect(con)
    })
