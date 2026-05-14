#' @title importData: Import views directly from HTLN bladderpod database
#'
#' @description This function imports all views in the HTLN MoBlad_BloodyHill
#' Microsoft Access DB. Each view is added to a VIEWS_HTLN_BLDP environment
#' in your workspace, or to your global environment based on whether
#' new_env = TRUE or FALSE.
#'
#' @importFrom dplyr collect rename tbl
#' @importFrom magrittr %>%
#'
#' @param instance Specify whether you are connecting to the local instance or
#' server.
#' \describe{
#' \item{"local"}{Default. Connects to local install of frontend database}
#' \item{"server"}{Connects to main backend on server. Note that you must
#' have permission to access the server, and connection speeds are likely to be
#' much slower than the local instance. You must also be connected to VPN or
#' NPS network.}}
#'
#' @param server Quoted name of the server to connect to, if instance = "server".
#' Valid input is the server address to connect to the main database. If
#' connecting to the local instance, leave blank.
#'
#' @param path Path to where the frontend database, if instance = "local".
#'
#' @param new_env Logical. Specifies which environment to store views in.
#' If \code{TRUE}(Default), stores views in VIEWS_HTLN_BLDP environment.
#' If \code{FALSE}, stores views in global environment
#'
#' @param name Character. Specifies the name of the database.
#'
#' @examples
#' \dontrun{
#' # Import using default settings of local instance, server = 'localhost' and add VIEWS_MIDN_NCBN environment
#' importData()
#'
#' # Import using computer name (# should be real numbers)
#' importData(server = "HTLN-######", new_env = TRUE)
#'
#' # Import from main database on server
#' importData(server = "INP###########\\########", instance = "server", new_env = TRUE)
#' }
#'
#' @return HTLN bladderpod database views in specified environment
#'
#' @export

importData <- function(instance = c("local", "server"), server = NA, new_env = TRUE, path = NA, name = "MoBlad_BloodyHill"){

  # selecting local or server
  instance <- match.arg(instance)

  # Checking that suggested packages required for this function are installed
  if(!requireNamespace("DBI", quietly = TRUE)){
    stop("Package 'DBI' needed for this function to run. Please install it.",
         call. = FALSE)
  }

  if(!requireNamespace("odbc", quietly = TRUE)){
    stop("Package 'odbc' needed for this function to run. Please install it.",
         call. = FALSE)
  }

  if(!requireNamespace("dplyr", quietly = TRUE)){
    stop("Package 'dplyr' needed for this function to run. Please install it.",
         call. = FALSE)
  }

  # Setting up connection
  server_con <- ifelse(instance == 'local',
                       "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                       server)

  error_mess <- paste("Unable to connect to SQL database.",
                      ifelse(instance == 'server',
                             paste0("Make sure you are connected to VPN or NPS network, and server is spelled correctly."),
                             paste0("Make sure you have a local installation of the database (see examples).")))

  tryCatch(
    conn <- odbc::dbConnect(odbc(),
                            .connection_string = paste0(server_con,
                                                        path,
                                                        name)),
    error = function(e){
      stop(error_mess)
    },
    warning = function(w){
      stop(error_mess)
    }
  )

  # get names of views
  view_list_db <- DBI::dbListTables(conn)

  # selecting tbls and tlus
  view_list_db <- view_list_db[grepl("tbl|tlu", view_list_db)]

  # setting progress bar
  pb <- txtProgressBar(min = 0,
                       max= length(view_list_db),
                       style = 3)

  # Importing views
  view_import <- lapply(seq_along(view_list_db), function(x){

    # progress bar
    setTxtProgressBar(pb, x)

    # get all tables from the database
    view <- view_list_db[[x]]

    # creating list of tables
    tab <- tbl(conn,
               view) |>
      collect() |>
      as.data.frame()

    return(tab)
  })

  # closing progress bar
  close(pb)

  # disconnecting
  DBI::dbDisconnect(conn)

  # renaming tables
  view_import <- setNames(view_import,
                          view_list_db)

  # assigning environment
  if(new_env == TRUE){
    VIEWS_HTLN_BLDP <<- new.env()
    list2env(view_import,
             envir = VIEWS_HTLN_BLDP)
  } else {
    list2env(views_import, envir = .GlobalEnv)}

}
