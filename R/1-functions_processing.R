
#' Import and process NMR spectral data
#'
#' @description Imports multiple spectra files and then combines and cleans the data.
#'
#' @param path Directory where the spectra files are saved
#' @param method Software used for initial processing of NMR spectra (before using this package). Available options include "mnova" and "topspin".
#' @param pattern Filename pattern to search for (by default "*.csv$")
#' @param quiet Print diagnostic messages? Logical
#' @return A \code{link{data.frame}} with data from all files found,
#' concatenated and sorted.
#'
#' @importFrom utils read.table
#' @export
#' @author Kaizad Patel
#' @examples
#' sdir <- system.file("extdata", "kfp_hysteresis", "spectra_mnova", package = "nmrrr")
#' nmr_import_spectra(path = sdir, method = "mnova")
nmr_import_spectra <- function(path, method,
                               pattern = "*.csv$", quiet = FALSE) {
  # Quiet R CMD CHECK notes
  sampleID <- ppm <- NULL

  # import and combine spectra data files
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  if (!quiet) message("Found ", length(files), " files")

  if (length(files) == 0) {
    stop("No files found!")
  }

  if (method == "mnova") {
    spectra_dat <- lapply(files, function(f) {
      # these files are tab-delimited with no header
      df <- read.table(f, header = FALSE, fill = TRUE, col.names = c("ppm", "intensity"))
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
    stop("Available methods: 'mnova' or 'topspin'")
  }

  # clean the spectral data
  sdat <- do.call("rbind", spectra_dat)
  sdat$sampleID <- gsub(".csv", "", sdat$sampleID, fixed = TRUE)
  sdat <- sdat[order(sdat$sampleID, sdat$ppm),]
  weak_as_tibble(sdat)
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
#' @return The input data with a new \code{group} column whose entries
#' are drawn from the binset. Entries will be \code{NA} if a \code{ppm}
#' value does not fall into any group.
#' @export
#' @author Kaizad Patel
#' @examples
#' sdir <- system.file("extdata", "kfp_hysteresis", "spectra_mnova", package = "nmrrr")
#' spec <- nmr_import_spectra(path = sdir, method = "mnova")
#' nmr_assign_bins(spec, bins_Clemente2012)
nmr_assign_bins <- function(dat, binset) {

  # Assign group (bin name) to each row of the data based on 'ppm'
  # We were previously merging and filtering to do this, which worked,
  # but is slow and memory-intensive; this is 3x as fast and efficient

  # Helper function that finds which binset row matches a value
  # Specifically, start <= x (ppm) <= stop
  match_bins <- function(x, binset) {
    # By definition, bins are open on the left and closed on the right
    # https://en.wikipedia.org/wiki/Interval_(mathematics)
    y <- which(x > binset$start & x <= binset$stop)
    if (length(y) == 1) {
      return(y)
    } else {
      return(NA_integer_)
    }
  }

  # For each row, call match_bins above
  bin_vals <- sapply(dat$ppm, match_bins, binset = binset)
  dat$group <- binset$group[bin_vals]
  dat
}


#' Import and process picked peaks data
#'
#' @description Process data of peaks picked with NMR software.
#'
#' @param path Directory where the peaks data are saved
#' @param method Format of input data, depending on how the data were exported.
#' "multiple columns": data are in split-column format, obtained by
#' pasting "peaks table" in MNova. "single column": data are in
#' single-column format, exported from MNova as "peaks script".
#' @param pattern Filename pattern to search for (by default "*.csv$")
#' @param quiet Print diagnostic messages? Logical

#' @return A dataframe with columns describing
#'   sample ID, ppm, intensity, area, group name.
#'
#' @importFrom utils read.table
#' @importFrom utils read.csv read.delim
#' @export
#' @author Kaizad Patel
#' @examples
#' sdir <- system.file("extdata", "kfp_hysteresis", "peaks_mnova_multiple", package = "nmrrr")
#' nmr_import_peaks(path = sdir, method = "multiple columns")
nmr_import_peaks <- function(path, method, pattern = "*.csv$", quiet = FALSE) {
  # Quiet R CMD CHECK notes
  ppm <- Intensity <- row_number <- sampleID <- NULL

  # Import and process picked peaks data; typically saved as multiple files

  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  if (!quiet) message("Found ", length(files), " files")

  if (length(files) == 0) {
    stop("No files found!")
  }

  if (method == "multiple columns") {
    # Peaks data are provided in split-column format
    peaks_rawdat <- lapply(files, function(f) {
      # This function will import all the data files and combine for all samples

      align_columns <- function(f) {
        # The input data are spread across multiple columns
        # Step 1. import file; check.names=FALSE because columns have
        # duplicate names, and we want to leave as is
        df <- read.csv(f,
          stringsAsFactors = FALSE,
          check.names = FALSE,
          colClasses = c(Annotation = "character")
        )

        # Step 2. confirm that the data are in 9-column groups
        noname_cols <- which(names(df) == "")
        if (ncol(df) < 9 || !all(diff(noname_cols) == 9)) {
          stop("Formatting problem: data don't appear to be in 9-column groups")
        }
        names(df)[noname_cols] <- "Obs" # give them a name

        # Step 3. Extract each group in turn and store temporarily in a list
        nmr_list <- lapply(noname_cols, function(x) df[x:(x + 8)])

        # Step 4. Finally, bind everything into a single data frame
        nmr_dat <- do.call("rbind", nmr_list)

        # Step 5. Create a new column that includes source sample name
        nmr_dat[["sampleID"]] <- basename(f)
        nmr_dat[["Impurity/Compound"]] <- as.character(nmr_dat[["Impurity/Compound"]])
        nmr_dat
      }

      # now create an object from the function
      align_columns(f)
      # this will be repeated for each file in the input folder
    })
  } else {
    if (method == "single column") {
      peaks_rawdat <- lapply(files, function(f) {
        # The files are tab-delimited, so import using read.table
        # There is no header, so create new column names
        df <- read.delim(f,
          stringsAsFactors = FALSE,
          check.names = FALSE,
          header = FALSE,
          col.names = c(
            "ppm", "Intensity", "Width", "Area",
            "Impurity/Compound", "Annotation", "junk1", "junk2"
          )
        )
        df[["sampleID"]] <- basename(f)
        df[["junk1"]] <- df[["junk2"]] <- NULL
        df
      })
    } else {
      if (method == "topspin") {
        peaks_rawdat <- lapply(files, function(f) {
          # Files are comma-delimited, with a header
          df <- read.csv(f,
            stringsAsFactors = FALSE,
            header = TRUE,
            col.names = c("peak", "ppm", "Intensity", "Annotation"),
            colClasses = c(Annotation = "character")
          )
          df[["sampleID"]] <- basename(f)
          df
        })
      } else {
        stop("Available methods: 'multiple columns', 'single column', or 'topspin'")
      }
    }
  }

  # Combine, filter, and clean the sampleID column
  pdat <- do.call("rbind", peaks_rawdat)
  pdat$sampleID <- gsub(".csv", "", pdat$sampleID, fixed = TRUE)
  weak_as_tibble(pdat)
}
