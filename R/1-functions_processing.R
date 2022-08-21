
#' Import and process NMR spectral data
#'
#' @description Imports multiple spectra files and then combines and cleans the data.
#'
#' @param
#' SPECTRA_FILES path/directory where the spectra files are saved
#' @param METHOD KP_TODO
#' @param pattern Filename pattern (by default "*.csv$")
#' @return A dataframe with columns describing the group name (sometimes
#' abbreviated), start and stop boundaries, and a longer, more complete
#' description of the group.
#'
#' @importFrom dplyr mutate filter select arrange %>%
#' @importFrom utils read.table
#' @export
import_nmr_spectra_data <- function(SPECTRA_FILES, METHOD, pattern = "*.csv$") {
  # Quiet R CMD CHECK notes
  sampleID <- ppm <- NULL

  # import and combine spectra data files
  files <- list.files(path = SPECTRA_FILES, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    stop("No files found!")
  } else {
    if (METHOD == "mnova") {
      spectra_dat <- do.call(rbind, lapply(files, function(filename) {
        # these files are tab-delimited with no header
        df <- read.table(filename, header = FALSE, col.names = c("ppm", "intensity"))
        df$sampleID <- basename(filename)
        df
      }))
    } else {
      if (METHOD == "topspin") {
        spectra_dat <- do.call(rbind, lapply(files, function(filename) {
          # these files are tab-delimited with no header
          df <- read.csv(filename, header = FALSE, fill = TRUE, col.names = c("x", "intensity", "y", "ppm"))
          df$sampleID <- basename(filename)
          df
        }))
      } else {
        stop("Appropriate methods are 'mnova' and 'topspin'")
      }
    }
  }

  # clean the spectral data
  spectra_dat %>%
    mutate(sampleID = gsub(".csv", "", sampleID, fixed = TRUE)) %>%
    arrange(sampleID, ppm)
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
  bins_dat <- BINSET %>% select(group, start, stop)

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
  bins <- BINSET %>%
    select(group, start, stop) %>%
    arrange(start, stop)

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
#' @importFrom dplyr mutate filter select left_join bind_rows rename %>%
#' @importFrom utils read.table
#' @importFrom utils read.csv read.delim
#' @export
process_peaks <- function(path, method, pattern = "*.csv$", quiet = FALSE) {
  # Quiet R CMD CHECK notes
  ppm <- Intensity <- row_number <- NULL

  # Import and process picked peaks data; typically saved as multiple files

  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  if(!quiet) message("Found ", length(files), " files")

  if (METHOD == "multiple columns") {
    # Peaks data are provided in split-column format
    peaks_rawdat <- lapply(files, function(f) {
      # This function will import all the data files and combine for all samples

      align_columns <- function(f) {
        # the input data are spread across multiple columns
        # Step 1. import file.
        # check.names=FALSE because columns have duplicate names, and we want to leave as is
        df <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)

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
        nmr_dat[["sampleID"]] <- basename(f)
        nmr_dat
      }

      # now create an object from the function
      align_columns(f)
      # this will be repeated for each file in the input folder
    })
  } else {
    if (METHOD == "single column") {
      peaks_rawdat <- lapply(files, function(f) {
        # The files are tab-delimited, so import using read.table
        # There is no header, so create new column names
        df <- read.delim(path,
<<<<<<< Updated upstream
          col.names = c(
            "ppm", "Intensity", "Width", "Area", "Type",
            "Flags", "Impurity/Compound", "Annotation"
          )
=======
                         stringsAsFactors = FALSE,
                         col.names = c(
                           "ppm", "Intensity", "Width", "Area", "Type",
                           "Flags", "Impurity/Compound", "Annotation"
                         )
>>>>>>> Stashed changes
        )
        df[["sampleID"]] <- basename(f)
        df
      })
    } else {
      if (METHOD == "topspin") {
        peaks_rawdat <- lapply(files, function(f) {
          # the files are tab-delimited, so read.csv will not work. import using read.table
          # there is no header. so create new column names
          # then add a new column `source` to denote the file name
          df <- read.csv(path,
<<<<<<< Updated upstream
            col.names = c("peak", "ppm", "Intensity", "Annotation")
=======
                         stringsAsFactors = FALSE,
                         col.names = c("peak", "ppm", "Intensity", "Annotation")
>>>>>>> Stashed changes
          )
          df[["sampleID"]] <- basename(f)
          df
        })
      } else {
        stop("Available methods: 'multiple columns', 'single column', or 'topspin'")
      }
    }
  }

  # clean the source column

  peaks_rawdat %>%
    bind_rows() %>%
    filter(ppm >= 0 & ppm <= 10) %>%
    filter(Intensity > 0) %>%
    filter(!is.na(ppm)) %>%
    # filter(!Flags == "Weak") %>%
    mutate(sampleID = gsub(".csv", "", sampleID, fixed = TRUE))
}
