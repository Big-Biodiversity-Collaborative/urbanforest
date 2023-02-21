# Project: Tucson Urban Trees and Biodiversity
# Script: Function "query_gbif" to download records from GBIF for a set of coordinates and list of taxa
# Credit: Jeff Oliver, University of Arizona, jcoliver@arizona.edu
# Author: Heatherlee Leary, University of Arizona, hleary.wildlife@outlook.com


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
#' @param dataset_keys character vector of dataset keys to restrict results;
#' if \code{NULL}, all datasets from query will be returned
#' @param max_attempts integer number of maximum number of attempts for each 
#' query to GBIF
#' 
#' @return data frame of observations returned from GBIF, if queries failed 
#' after \code{max_attempts} attempts, returns NULL.
query_gbif <- function(taxon_keys, lon_limits, lat_limits, verbose = FALSE,
                       cols = c("decimalLatitude", "decimalLongitude",
                                "individualCount", "family", "species", "year", 
                                "month", "day", "datasetKey", "datasetName", 
                                "gbifID", "lifeStage"),
                       dataset_keys = c("4fa7b334-ce0d-4e88-aaae-2e0c138d049e", 
                                        "50c9509d-22c7-4a22-a47d-8c48425ef4a7"),
                       year_range = ("2017,2021"), max_attempts = 5) {
  if (!require(rgbif)) {
    stop("GBIF queries require the rgbif library")
  }
  if (!require(dplyr)) {
    stop("GBIF queries require the dplyr library")
  }
  
  # The function rgbif::occ_search only allows multiple values for ONE 
  # argument at a time. That is, we could pass it a vector > 1 for *either* 
  # the taxonKey argument *OR* datasetKey, but not both. For this reason, will, 
  # for now, iterate over values in datasetKey and send (possibly) multiple 
  # values to taxonKey
  
  # List that will hold all observations, one element per datasetKey per 
  # taxonKey
  gbif_obs_list <- list()
  for (datasetKey_i in 1:length(dataset_keys)) {
    datasetKey = dataset_keys[datasetKey_i]
    # Count number of observations in the rectangle, for this datasetKey, 
    # as pagination might be necessary; actually performs one search per 
    # taxonKey value
    
    # Using try/catch in cases of timeout
    attempts <- 0
    success <- FALSE
    while (attempts < max_attempts & !success) {
      if (attempts > 0) {
        if (verbose) {
          message("...attempt ", attempts, " failed; retrying")
        }
      }
      Sys.sleep(time = abs(rnorm(n = 1))) # delay between requests
      try(expr = {
        gbif_count <- rgbif::occ_search(taxonKey = taxon_keys,
                                        limit = 1,
                                        decimalLongitude = paste(lon_limits[1:2],
                                                                 collapse = ","),
                                        decimalLatitude = paste(lat_limits[1:2],
                                                                collapse = ","),
                                        year = year_range,
                                        hasGeospatialIssue = FALSE,
                                        datasetKey = datasetKey)
        success <- TRUE
      }, silent = TRUE)
      attempts <- attempts + 1
    }
    
    if (attempts >= max_attempts & !success) {
      message("...Count failed after ", attempts, " attempts; no data for this dataset.")
      return(NULL)
    }
    
    if (!is.null(gbif_count)) {
      # Figure out total number of observations (for all taxa in taxon_keys)
      if (length(taxon_keys) > 1) {
        # For queries of multiple taxa, returned value is series of nested lists; 
        # need to extract the value from count from each of them
        # Oof. Rough.
        num_obs <- sum(unlist(lapply(X = lapply(X = gbif_count, 
                                                FUN = "[[", "meta"),
                                     FUN = "[[", "count")))
      } else {
        # If there is only one taxon_key, returned value is different (one less 
        # level of list nesting)
        num_obs <- gbif_count$meta$count
      }
      # Do a search for all observations if there was at least one observation
      if (num_obs > 0) {
        page <- 1
        start <- 0
        while(start <= num_obs) {
          end_obs <- min((start + 300), num_obs)
          if (verbose) {
            message(paste0("Downloading ", (start + 1), "-", 
                           end_obs, " of ", num_obs))
          }
          
          # Using try/catch in cases of timeout
          attempts <- 0
          success <- FALSE
          while (attempts < max_attempts & !success) {
            if (attempts > 0) {
              if (verbose) {
                message("...attempt ", attempts, " failed; retrying")
              }
            }
            try(expr = {
              if (end_obs %% 3000 == 0) {
                # Every 3000th observation, add a longer delay
                Sys.sleep(time = abs(rnorm(n = 1, mean = 2))) # delay between requests
              } else {
                Sys.sleep(time = abs(rnorm(n = 1))) # delay between requests
              }
              gbif_query <- rgbif::occ_search(taxonKey = taxon_keys,
                                              decimalLongitude = paste(lon_limits[1:2],
                                                                       collapse = ","),
                                              decimalLatitude = paste(lat_limits[1:2],
                                                                      collapse = ","),
                                              start = start,
                                              limit = 300,
                                              year = year_range,
                                              hasGeospatialIssue = FALSE,
                                              datasetKey = datasetKey)
              success <- TRUE
            }, silent = TRUE)
            attempts <- attempts + 1
          }
          
          if (attempts >= max_attempts & !success) {
            message("...Retrieval failed after ", attempts, " attempts; data may be incomplete.")
            return(NULL)
          }
          
          # Need to add results of this query to the larger gbif_obs_list (which 
          # has one element per datasetKey), but if there were multiple 
          # taxon_keys passed to the search, will need to extract those data from
          # separate list elements (there is one per taxon)
          if (length(taxon_keys) > 1) {
            # Extract the "data" element from each element of the query result
            data_list <- lapply(X = gbif_query, FUN = "[[", "data")
            gbif_obs <- dplyr::bind_rows(data_list)
          } else {
            # For a single taxon query, can go right to data object
            gbif_obs <- gbif_query$data
          }
          
          # Need to act differently if this is first or subsequent page
          if (page == 1) {
            # For first page of results, we just add the observations at the 
            # appropriate element of the list
            gbif_obs_list[[datasetKey]] <- gbif_obs
          } else {
            # All subsequent pages are bound to existing element for this 
            # datasetKey
            gbif_obs_list[[datasetKey]] <- dplyr::bind_rows(gbif_obs_list[[datasetKey]],
                                                            gbif_obs)
          }
          page <- page + 1
          start <- (page - 1) * 300
        } # End while pagination
      } # End conditional for at least one observation
    } # End conditional for successful count
  } # End iteration over all dataset_keys
  
  # We now have a list with observations for all dataset_keys; combine to 
  # single data frame
  all_obs <- dplyr::bind_rows(gbif_obs_list)
  
  # Select only desired columns
  # Some results returned from GBIF do not always include all columns 
  # (e.g. individualCount doesn't always come through); only select on 
  # columns that *do* exist
  if (nrow(all_obs) > 0 & length(cols) > 0) {
    all_obs <- all_obs %>%
      dplyr::select(dplyr::intersect(cols, colnames(all_obs)))
  }
  
  return(all_obs)
  
    ########################################
    # OLD BELOW
    ########################################
    
    for (taxon_key in taxon_keys) {
      # Pull out the total count of records for this taxon in this rectangle
      # If there was only one taxon key, returned value is a result list with 
      # five elements, but if there is more than one taxon_key, the result is a 
      # list of lists
      if (length(taxon_keys) > 1) {
        taxon_count <- gbif_count[[as.character(taxon_key)]]$meta$count
      } else {
        taxon_count <- gbif_count$meta$count
      }
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
                                        limit = 300,
                                        year = year_range)
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