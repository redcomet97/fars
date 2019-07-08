#' BUILDING R PACKAGES
#' fars_functions.R
#'
#' @author Yi Shen
#'
#'
#'
# ----------------------------------------------------------------------------------
#' Reading file from the Fatality Analysis Reporting System.
#' This function reads in a .csv file and outputs a dataframe of the csv file.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param filename a character string of the intended file to be read. Working directory must be correctly set.
#' @return Data frame consisting of the data from the csv file supplied in filename parameter. If no such file exists
#' then an error is returned.
#'
#' @examples
#' library(readr)
#' library(dplyr)
#' data_2013 <- fars_read(accident_2013.csv.bz2")
#' @seealso \code{\link{make_filename}}
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


# ----------------------------------------------------------------------------------
#' Make data file name:
#' Creates a .csv file name for a given year to be used for fars_read
#'
#' @param year the intended year of data of interest as an integer or string
#' @return  the file name as a character string.
#'
#' #' examples
#' make_filename(2015)
#'
#' @seealso \code{\link{fars_read}}
#' @seealso \code{\link{fars_read_years}}
#'
#'@export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}



# ------------------------------------------------------------------------------------
#' Read FARS Data Years
#'
#' Reads FARS data for a list of years, sorted by month.
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr "%>%"
#'
#' @param years a vector of years
#' @return a dataframe of entries of the years sorted by month, or a warning if a year is invalid e.g., no file
#' exists for said year.
#'
#' @examples
#' fars_read_years(c(2013,2015))
#'
#' @seealso \code{\link{fars_read}}
#' @seealso \code{\link{make_filename}}
#' @seealso \code{\link{fars_summarize_years}}
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


# ---------------------------------------------------------------------------------
#' FARS data Summary Years
#'
#' summarizes FARS data for multiple years.
#'
#' @importFrom  dplyr bind_rows summarize group_by
#' @importFrom tidyr spread
#' @importFrom magrittr "%>%"
#'
#' @param years a vector containing a list of years to be sorted and summarized
#' @return a data frame summarizing the number of fatal accidents each month sorted by year
#'
#'
#' @seealso \code{\link{fars_read_years}}
#'
#' @examples
#' fars_summarize_years(c(2013,2014,2015))
#' plot(fars_summarize_years(c(2014,2015)))
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#----------------------------------------------------------------------------------
#' Fars Map State
#'
#' Displays the state map for a given year, highlighting the area with points to represent a fatal accident occuring
#' at that location
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @seealso \code{\link{make_filename}}
#' @seealso \code{\link{fars_read}}
#'
#' @param state.num the state number that corresponds to either a US state or territory. If an invalid state number
#' is used, an erorr will occur.
#' @param year the year of interest. can be string or integer.
#' @return the corresponding state map with the fatal accidents represented as points
#'
#' @examples
#' fars_map_state(50,2013)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
