
# I. NMR BINS -------------------------------------------------------------

#' Choose a bin set to group and integrate the NMR peaks
#'
#' @description The NMR spectrum can be split into several bins, based on
#' chemical shift (ppm). Choose a binset to group the peaks.
#' Binsets are specific to nuclei and solvents.
#'
#' @param BINSET Choose the binset you want.
#'   Options include: "Clemente2012", "Lynch2019", "Hertkorn2013_MeOD", "Mitchell2018"
#'
#' @return A dataframe with columns describing the group name (sometimes
#' abbreviated), start and stop boundaries, and a longer, more complete
#' description of the group.
#'
#' @references
#' JS Clemente et al. 2012. “Comparison of Nuclear Magnetic Resonance Methods
#' for the Analysis of Organic Matter Composition from Soil Density and
#' Particle Fractions.” Environmental Chemistry. https://doi.org/10.1071/EN11096.
#'
#' LM Lynch et al. 2019. “Dissolved Organic Matter Chemistry and Transport
#' along an Arctic Tundra Hillslope.” Global Biogeochemical Cycles.
#' https://doi.org/10.1029/2018GB006030.
#'
#' P Mitchell et al. 2018. “Nuclear Magnetic Resonance Analysis of Changes
#' in Dissolved Organic Matter Composition with Successive Layering on Clay
#' Mineral Surfaces.” Soil Systems. https://doi.org/10.3390/soils2010008.
#' @importFrom dplyr arrange row_number mutate %>%
#' @importFrom utils read.delim
#' @export
set_bins <- function(BINSET) {
  # Quiet R CMD CHECK notes
  start <- NULL

  filePath_bins <- list.files(path = "bins", pattern = BINSET, full.names = TRUE)

  if (length(filePath_bins) > 1) {
    stop("too many options!")
  }
  if (length(filePath_bins) == 0) {
    stop("option not available")
  }

  read.delim(filePath_bins, header = TRUE) %>%
    arrange(start) %>%
    mutate(number = row_number())
}


#

# II. IMPORT SPECTRA DATA -------------------------------------------------

#' Import and process NMR spectral data
#'
#' @description Imports multiple spectra files and then combines and cleans the data.
#'
#' @param
#' SPECTRA_FILES path/directory where the spectra files are saved
#' @param METHOD KP_TODO
#' @return A dataframe with columns describing the group name (sometimes
#' abbreviated), start and stop boundaries, and a longer, more complete
#' description of the group.
#'
#' @importFrom dplyr mutate filter select arrange %>%
#' @importFrom utils read.table
#' @export
import_nmr_spectra_data <- function(SPECTRA_FILES, METHOD) {
  # Quiet R CMD CHECK notes
  sampleID <- ppm <- NULL

  # import and combine spectra data files
  filePaths_spectra <- list.files(path = SPECTRA_FILES, pattern = "*.csv", full.names = TRUE)

  if (length(filePaths_spectra) == 0) {
    stop("no .csv files found!")
  } else {
    if (METHOD == "mnova") {
      spectra_dat <- do.call(rbind, lapply(filePaths_spectra, function(path) {
        # the files are tab-delimited, so read.csv will not work. import using read.table
        # there is no header. so create new column names
        # then add a new column `source` to denote the file name
        df <- read.table(path, header = FALSE, col.names = c("ppm", "intensity"))
        df[["source"]] <- path
        df
      }))
    } else {
      if (METHOD == "topspin") {
        spectra_dat <- do.call(rbind, lapply(filePaths_spectra, function(path) {
          # the files are tab-delimited, so read.csv will not work. import using read.table
          # there is no header. so create new column names
          # then add a new column `source` to denote the file name
          df <- read.csv(path, header = FALSE, fill = TRUE, col.names = c("x", "intensity", "y", "ppm"))
          df[["source"]] <- path
          df
        }))
      } else {
        stop("appropriate methods are mnova and topspin")
      }
    }
  }

  # clean the spectral data
  spectra_dat %>%
    mutate(source = as.character(source),
           source = gsub(paste0(SPECTRA_FILES, "/"), "", source, fixed = TRUE),
           source = gsub(".csv", "", source, fixed = TRUE)) %>%
    rename(sampleID = source) %>%
    arrange(sampleID, ppm) %>%
    force()
}

#
# III. ASSIGN BINS --------------------------------------------------------

#' Assign compound classes using the chosen binset
#'
#' @description Use this function to import multiple spectra files, combine them,
#' and then process/clean the data.
#'
#' @param dat Input dataframe. This could be spectral data, or peak picked data.
#' Must include a `ppm` column for compound class assignment.
#' @param BINSET Choose the binset you want. Options include: "Clemente2012",
#' "Lynch2019", "Hertkorn2013_MeOD", "Mitchell2018"
#'
#' @return A dataframe with columns describing the group name
#' (sometimes abbreviated), start and stop boundaries, and a longer, more
#' complete description of the group.
#'
#' @references
#' JS Clemente et al. 2012. “Comparison of Nuclear Magnetic Resonance Methods
#' for the Analysis of Organic Matter Composition from Soil Density and Particle
#' Fractions.” Environmental Chemistry. https://doi.org/10.1071/EN11096.
#'
#' LM Lynch et al. 2019. “Dissolved Organic Matter Chemistry and Transport
#' along an Arctic Tundra Hillslope.” Global Biogeochemical Cycles.
#' https://doi.org/10.1029/2018GB006030.
#'
#' P Mitchell et al. 2018. “Nuclear Magnetic Resonance Analysis of Changes in
#' Dissolved Organic Matter Composition with Successive Layering on Clay
#' Mineral Surfaces.” Soil Systems. https://doi.org/10.3390/soils2010008.
#' @importFrom dplyr mutate filter select %>%
#' @importFrom utils read.table
#' @export
assign_compound_classes <- function(dat, BINSET) {
  # Quiet R CMD CHECK notes
  group <- start <- ppm <- NULL

  # load binsets
  bins_dat <- set_bins(BINSET) %>% select(group, start, stop)

  # assign bins to each point
  subset(
    merge(dat, bins_dat),
    start <= ppm & ppm <= stop
  ) %>%
    select(-start, -stop)
}



assign_compound_classes_v2 <- function(dat, BINSET) {
  # Quiet R CMD CHECK notes
  group <- start <- NULL

  # load binsets
  bins <- set_bins(BINSET) %>%
    select(group, start, stop) %>%
    arrange(start, stop)
  # bins <- readr::read_tsv("bins/Clemente2012.txt")

  # identify gaps between bins
  gaps <- c(utils::head(bins$stop, -1) != bins$start[-1], TRUE)
  # create new gap bins
  gapbins <- dplyr::tibble(group = NA_character_, start = bins$stop[gaps])
  newbins <- rbind(bins[c("group", "start")], gapbins) %>% arrange(start)
  newbins[is.na(newbins)] <- "NANA"

  dat$group <- cut(dat$ppm, newbins$start, labels = head(newbins$group, -1), right = FALSE)
  dat %>% filter(group != "NANA")
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
#' @importFrom utils read.csv
#' @export
process_peaks <- function(PEAKS_FILES, METHOD) {
  # Quiet R CMD CHECK notes
  ppm <- Intensity <- NULL

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
