
#' Import and process NMR spectral data
#'
#' @description Imports multiple spectra files and then combines and cleans the data.
#'
#' @param path Directory where the spectra files are saved
#' @param method KP_TODO
#' @param pattern Filename pattern to search for (by default "*.csv$")
#' @param quiet Print diagnostic messages? Logical
#' @return The data from all files found, concatenated into a single
#' data frame and sorted.
#'
#' @importFrom dplyr mutate filter select arrange %>%
#' @importFrom utils read.table
#' @export
import_nmr_spectra_data <- function(path, method,
                                    pattern = "*.csv$", quiet = FALSE) {
  # Quiet R CMD CHECK notes
  sampleID <- ppm <- NULL

  # import and combine spectra data files
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  if(!quiet) message("Found ", length(files), " files")

  if (length(files) == 0) {
    stop("No files found!")
  } else if (method == "mnova") {
    spectra_dat <- lapply(files, function(f) {
      # these files are tab-delimited with no header
      df <- read.table(f, header = FALSE, col.names = c("ppm", "intensity"))
      df$sampleID <- basename(f)
      df
    })
  } else if (method == "topspin") {
    spectra_dat <- lapply(files, function(f) {
      # these files are tab-delimited with no header
      df <- read.csv(f, header = FALSE, fill = TRUE, col.names = c("x", "intensity", "y", "ppm"))
      df$sampleID <- basename(f)
      df
    })
  } else {
    stop("Appropriate methods are 'mnova' and 'topspin'")
  }

  # clean the spectral data
  spectra_dat %>%
    bind_rows() %>%
    mutate(sampleID = gsub(".csv", "", sampleID, fixed = TRUE)) %>%
    arrange(sampleID, ppm)
}


#' Assign compound classes using the chosen binset
#'
#' @description Assign group (bin name) to each row of the data based on
#' the \code{ppm} column.
#'
#' @param dat Input dataframe. This could be spectral data, or peak picked data.
#' Must include a `ppm` column for compound class assignment
#' @param binset A binset; e.g. \code{\link{bins_Clemente2012}},
#' \code{\link{bins_Hertkorn2013}}, etc., or a similarly-structured data frame
#'
#' @return The input dataframe with a new \code{group} column whose entries
#' are drawn from the binset. Entries will be \code{NA} if a \code{ppm}
#' value does not fall into any group.
#' @export
assign_compound_classes <- function(dat, binset) {

  # Assign group (bin name) to each row of the data based on 'ppm'
  # We were previously merging and filtering to do this, which worked,
  # but is slow and memory-intensive; this is 3x as fast and efficient

  # Helper function that finds which binset row matches a value
  # Specifically, start <= x (ppm) <= stop
  match_bins <- function(x, binset) {
    y <- which(x >= binset$start & x <= binset$stop)
    if(length(y) == 1) return(y) else return(NA_integer_)
  }

  # For each row, call match_bins above
  bin_vals <- sapply(dat$ppm, match_bins, binset = binset)
  dat$group <- binset$group[bin_vals]
  dat
}


# IV. PROCESS PEAKS -------------------------------------------------------


#' Process picked peaks data
#'
#' @description Use this function to process data of peaks picked with NMR software.
#'
#' @param PEAKS_FILES file path for peaks data (input). All the peaks files are
#' saved as individual .csv files
#' @param METHOD format of input data, depending on how the data were exported.
#' "multiple columns": use when data are in split-column format, obtained by
#' pasting "peaks table" in MNova. "single column": use when data are in
#' single-column format, exported from MNova as "peaks script".
#' @param METHOD KP_TODO
#'
#' @return A dataframe with columns describing
#'   sample ID, ppm, intensity, area, group name.
#'
#' @importFrom dplyr mutate filter select left_join bind_rows rename %>%
#' @importFrom utils read.table
#' @importFrom utils read.csv read.delim
#' @export
process_peaks <- function(PEAKS_FILES, METHOD) {
  # Quiet R CMD CHECK notes
  ppm <- Intensity <- row_number <- NULL

  # import and process picked peaks data
  # data are typically saved as multiple files
  # import and compile

  ## then, set the file path for the peaks data
  filePaths_peaks <- list.files(path = PEAKS_FILES, pattern = "*.csv", full.names = TRUE)

  if (METHOD == "multiple columns") {
    ## if peaks data are provided in split-column format
    peaks_rawdat <- bind_rows(lapply(filePaths_peaks, function(path) {
      # this function will import all the data files and combine for all samples
      # first, we run the function to clean a single file
      # the input data are spread across multiple columns, so use this function to align columns

      align_columns <- function(path) {
        # Step 1. import file.
        # check.names=FALSE because columns have duplicate names, and we want to leave as is
        df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

        # Step 2. confirm that the data are in 9-column groups
        noname_cols <- which(names(df) == "")
        if (!all(diff(noname_cols) == 9)) {
          stop("Formatting problem: data don't appear to be in 9-column groups")
        }
        names(df)[noname_cols] <- "Obs" # give them a name

        # Step 3. Extract each group in turn and store temporarily in a list
        nmr_list <- lapply(noname_cols, function(x) df[x:(x + 8)])

        # Step 4. Finally, bind everything into a single data frame
        # This uses dplyr but we could also use base R: do.call("rbind", nmr_list)
        nmr_dat <- bind_rows(nmr_list)

        # Step 5. Create a new column that includes source sample name
        nmr_dat[["source"]] <- rep(path, nrow(df))

        nmr_dat
      }

      # now create an object from the function
      align_columns(path)
      # this will be repeated for each file in the input folder
    }))
  } else {
    if (METHOD == "single column") {
      ## if peaks data are provided in single-column format
      peaks_rawdat <- bind_rows(lapply(filePaths_peaks, function(path) {
        # the files are tab-delimited, so read.csv will not work. import using read.table
        # there is no header. so create new column names
        # then add a new column `source` to denote the file name
        df <- read.delim(path,
                         col.names = c(
                           "ppm", "Intensity", "Width", "Area", "Type",
                           "Flags", "Impurity/Compound", "Annotation"
                         )
        )
        df[["source"]] <- path
        df
      }))
    } else {
      if (METHOD == "topspin") {
        ## if peaks data are provided in topspin format
        peaks_rawdat <- bind_rows(lapply(filePaths_peaks, function(path) {
          # the files are tab-delimited, so read.csv will not work. import using read.table
          # there is no header. so create new column names
          # then add a new column `source` to denote the file name
          df <- read.csv(path,
                         col.names = c("peak", "ppm", "Intensity", "Annotation")
          )
          df[["source"]] <- path
          df
        }))
      } else {
        stop("Available methods: `multiple columns`, `single column`, or `topspin`")
      }
    }
  }
  # clean the source column

  peaks_rawdat %>%
    filter(ppm >= 0 & ppm <= 10) %>%
    filter(Intensity > 0) %>%
    filter(!is.na(ppm)) %>%
    # filter(!Flags == "Weak") %>%
    mutate(source = gsub(paste0(PEAKS_FILES, "/"), "", source, fixed = TRUE),
           source = gsub(".csv", "", source, fixed = TRUE)) %>%
    rename(sampleID = source) %>%
    force()
}
