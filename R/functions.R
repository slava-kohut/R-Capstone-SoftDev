#' Load and clean the NOAA Significant equation dataset
#'
#' \code{eq_clean_data} loads the NOAA dataset into memory and cleans it for easier mapping.
#' The user provides a path to the data set and its name.
#'
#' @param fileLink string or connection. A path to the NOAA dataset.
#' @param fileName string. The dataset name (if renamed).
#'
#' @examples
#' \donttest{
#' eq_clean_data('../')
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom tidyr unite
#'
#' @export
#'
eq_clean_data <- function(fileLink, fileName = 'signif.txt'){

  cleanData <- suppressMessages(readr::read_tsv(paste0(fileLink, fileName)))

  # LATITUDE and LONGITUDE to numeric
  cleanData <- cleanData %>% dplyr::mutate(LATITUDE = as.numeric(LATITUDE),
                                           LONGITUDE = as.numeric(LONGITUDE))

  #
  # more cleaning
  #
  cleanData <- cleanData %>% dplyr::mutate(EPOCH = ifelse(YEAR < 0, 'BC', 'AD')) %>% # epoch: AD or BC, no negative years
    dplyr::mutate(YEAR = as.character(YEAR)) %>%
    dplyr::mutate(MONTH = ifelse(is.na(MONTH), 1, MONTH)) %>% # set NAs to 1 for MONTH and DAY
    dplyr::mutate(DAY = ifelse(is.na(DAY), 1, DAY)) %>%
    dplyr::mutate(YEAR = sub('^-', '', YEAR)) %>%
    tidyr::unite('DATE', YEAR, MONTH, DAY, sep = '-') %>%
    dplyr::mutate(DATE = as.Date(DATE, '%Y-%m-%d')) %>%
    dplyr::mutate(LOCATION_NAME = eq_location_clean(LOCATION_NAME))

  cleanData
}

#' Clean location data from the NOAA data set
#'
#' \code{eq_location_clean} extracts a first string preceded by a colon in the LOCATION column
#' and converts it to the title case.
#'
#' @param locCol A character vector. Location strings that need to be converted.
#'
#' @examples
#' eq_location_clean('COLOMBIA: BOGOTA')
#' \donttest{
#' data("eq_data"); eq_location_clean(eq_data$LOCATION_NAME)
#' }
#'
#' @importFrom stringr str_extract
#'
#' @export
#'
eq_location_clean <- function(locCol){
    stringr::str_extract(tolower(locCol), '[^:]+') %>% tools::toTitleCase()
}

#' Create earthquake maps
#'
#' \code{eq_map} creates a regional earthquake map using a subset of the NOAA Significant Earthquake dataset.
#'
#' @param data A data frame (tibble). A subset of the NOAA Significant Earthquakes dataset. This dataset is used
#'  for mapping.
#' @param annot_col A string. A dataset column from the dataset used for earthquake labels.
#'  This column can be created using \code{eq_create_label}.
#' @examples
#' \donttest{data("eq_data"); eq_data %>% filter(COUNTRY == 'MEXICO') %>%
#' eq_map()
#' }
#' \donttest{data("eq_data"); eq_data %>% filter(COUNTRY == 'MEXICO') %>%
#' eq_map(annot_col = 'EQ_PRIMARY')
#' }
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#' @importFrom stats as.formula
#'
#' @export
#'
eq_map <- function(data = data, annot_col = "DATE"){
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addCircleMarkers(data = data, lng = ~LONGITUDE,
                     lat = ~LATITUDE, radius = ~EQ_PRIMARY,
                     popup = stats::as.formula(paste0('~','as.character(',annot_col,')')))
}

#' Create labels for earthquake maps
#'
#' \code{eq_create_label} creates labels for earthquake maps.
#' The labels include: location, magnitude, and the total number of deaths.
#'
#' @param loc A character vector that contains locations.
#' @param magnitude A numeric vector that contains earthquake magnitudes.
#' @param deaths A numeric vector that contains death counts.
#'
#' @return An HTML-formatted string (strings) that is used as a label
#'
#' @examples
#' eq_create_label('Mexico', 8.0, 2000)
#'
#' @export
#'
eq_create_label <- function(loc, magnitude, deaths){
  paste0("<b>Location:</b> ", as.character(loc),
        "<br />",
        "<b>Magnitude:</b> ", as.character(magnitude),
        "<br />",
        "<b>Total deaths:</b> ", as.character(deaths))
}
