# Project: Tucson Urban Trees and Biodiversity
# Script: Function "query_gbif" to download records from GBIF for a set of coordinates and list of taxa
# Credit: Jeff Oliver jcoliver@arizona.edu
# Heatherlee Leary
# hleary.wildlife@outlook.com
# 2022-09-13


#' Download records from GBIF for a set of coordinates and list of taxa
#' 
#' @param taxon_keys numeric vector of taxon keys to search for
#' @param lon_limits numeric vector of length two givein minimum and maximum
#' decimal longitude of search query
#' @param lat_limits numeric vector of length two giving minimum and maximum 
#' decimal latitude of search query
#' @param verbose logical indicating whether or not to print status messages
#' @param cols columns to retain; if \code{NULL}, returns all columns that are 
#' returned from a call to \code{rgbif::occ_search}
#' @param dataset_names character vector of dataset names to restrict results;
#' if \code{NULL}, all datasets from query will be returned
#' 
#' @return data frame of observations returned from GBIF
query_gbif <- function(taxon_keys, lon_limits, lat_limits, verbose = FALSE,
                       cols = c("decimalLatitude", "decimalLongitude",
                                "individualCount", "family", "species", "year", 
                                "month", "day", "datasetName", "gbifID",
                                "lifeStage"),
                       dataset_names = c("eButterfly", "iNaturalist research-grade observations")) {
  if (!require(rgbif)) {
    stop("GBIF queries require the rgbif library")
  }
  if (!require(dplyr)) {
    stop("GBIF queries require the dplyr library")
  }
  
  # Count number of observations in the rectangle, as pagination might be 
  # necessary; actually performs one search per taxonKey value
  gbif_count <- rgbif::occ_search(taxonKey = taxon_keys,
                                  limit = 1,
                                  decimalLongitude = paste(lon_limits[1:2],
                                                           collapse = ","),
                                  decimalLatitude = paste(lat_limits[1:2],
                                                          collapse = ","))
  # 
  # For each family, get count and paginate as necessary
  gbif_obs_list <- list()
  for (taxon_key in taxon_keys) {
    # Pull out the total count of records for this taxon in this rectangle
    taxon_count <- gbif_count[[as.character(taxon_key)]]$meta$count
    if (taxon_count > 0) {
      page <- 1
      start <- 0
      while(start <= taxon_count) {
        if (verbose) {
          #TODO: Could do better job with numbers (start at +1, check for min(start+300, taxon_count))
          message(paste0("Downloading ", (start + 1), "-", 
                         min((start+300), taxon_count), " of ", 
                         taxon_count, " for taxon key ", taxon_key))
        }
        gbif_obs <- rgbif::occ_search(taxonKey = taxon_key,
                                      decimalLongitude = paste(lon_limits[1:2],
                                                               collapse = ","),
                                      decimalLatitude = paste(lat_limits[1:2],
                                                              collapse = ","),
                                      start = start,
                                      limit = 300)
        if (page == 1) {
          gbif_obs_list[[as.character(taxon_key)]] <- gbif_obs$data
        } else {
          gbif_obs_list[[as.character(taxon_key)]] <- dplyr::bind_rows(gbif_obs_list[[as.character(taxon_key)]],
                                                                       gbif_obs$data)
        }
        page <- page + 1
        start <- (page - 1) * 300
      }
    } else {
      gbif_obs_list[[as.character(taxon_key)]] <- NULL
    }
  }
  # We now have a list with observations for all families; combine to single 
  # data frame
  all_obs <- dplyr::bind_rows(gbif_obs_list)
  
  # Apply dataset restrictions as necessary
  if (nrow(all_obs) > 0 & length(dataset_names) > 0) {
    if (verbose) {
      message(nrow(all_obs), " observations before filtering by dataset name")
    }
    all_obs <- all_obs %>%
      dplyr::filter(datasetName %in% dataset_names)
    if (verbose) {
      message(nrow(all_obs), " observations after filtering by dataset name")
    }
  }
  
  # Restrict columns as necessary
  if (nrow(all_obs) > 0 & length(cols) > 0) {
    # Some results returned from GBIF do not always include all columns 
    # (e.g. individualCount doesn't always come through); only select on 
    # columns that do exist
    all_obs <- all_obs %>%
      dplyr::select(dplyr::intersect(cols, colnames(all_obs)))
  }
  return(all_obs)
}

