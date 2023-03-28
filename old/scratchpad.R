# update package descriptions
roxygen2::roxygenise()


#
# TESTING NMRRR WORKFLOW --------------------------------------------------
library(tidyverse)
source("R/1-functions_processing.R")
source("R/1-functions_relabund.R")
source("R/1-functions_graphs.R")
source("R/utils.R")

# testing with `kfp_hysteresis` data --------------------------------------

## data generated using MNova
## spectra files and multi-column peaks files

# 1. process spectra
hyst_spectra_processed = nmr_import_spectra(path = "inst/extdata/kfp_hysteresis/spectra_mnova",
                                            method = "mnova") %>%
  filter(ppm >= 0 & ppm <= 10)



x = utils::read.table("inst/extdata/kfp_hysteresis/spectra_mnova/29.csv",
           header = FALSE, fill = TRUE, col.names = c("ppm", "intensity"))

hyst_spectra_processed_bins = nmr_assign_bins(dat = hyst_spectra_processed,
                                              binset = bins_Clemente2012)

hyst_spectra_processed_bins_noNA =
  hyst_spectra_processed_bins %>%
  filter(!is.na(group))

# spectra graphs

corekey = read.csv("inst/extdata/kfp_hysteresis/corekey_hyst_KFP.csv") %>%
  mutate_all(as.character)

hyst_spectra_processed_bins_corekey = hyst_spectra_processed_bins %>% left_join(corekey)

nmr_plot_spectra(dat = hyst_spectra_processed,
                 binset = bins_Clemente2012,
                 label_position = 5,
                 mapping = aes(x = ppm, y = intensity, group = sampleID, color = sampleID),
                 stagger = 0.5) +
  labs(subtitle = "binset: Clemente et al. 2012")+
  ylim(0, 5.5)

nmr_plot_spectra(dat = hyst_spectra_processed_bins_corekey,
           BINSET = bins_Clemente2012,
           LABEL_POSITION = 5,
           aes(x = ppm, y = intensity, group = sampleID, color = treatment),
           STAGGER = 0.5) +
  labs(subtitle = "binset: Clemente et al. 2012")+
  ylim(0, 5.5)+
 # facet_wrap(~treatment)+
  NULL




# 2. process peaks
hyst_peaks_processed_mult = process_peaks(PEAKS_FILES = "inst/extdata/kfp_hysteresis/peaks_mnova_multiple", METHOD = "multiple columns")
hyst_peaks_processed_mult_bins = assign_compound_classes_v2(dat = peaks_processed_mult, BINSET = "Clemente")

# 3. compute relabund - by sample
hyst_relabund_cores_auc = compute_relabund_cores(DAT = spectra_processed_bins, METHOD = "AUC")
hyst_relabund_cores_peaks_mult = compute_relabund_cores(DAT = peaks_processed_mult_bins, METHOD = "peaks")

# 4. compute relabund - summary by treatment
COREKEY = "inst/extdata/kfp_hysteresis/corekey_hyst_KFP.csv"
TREATMENTS = quos(treatment)

hyst_relabund_summary_auc = compute_relabund_treatments(RELABUND_CORES = relabund_cores_auc, TREATMENTS, COREKEY)
hyst_relabund_summary_peaks_mult = compute_relabund_treatments(RELABUND_CORES = relabund_cores_peaks_mult, TREATMENTS, COREKEY)



#
#

# testing with `kfp_drydown_d2o` data -----------------------------------------


## data generated using MNova
## spectra files and multi-column peaks files

# 1. process spectra
drydown_d2o_spectra_processed = nmr_import_spectra(path = "inst/extdata/kfp_drydown/spectra_mnova_d2o",
                                               method = "mnova")

# spectra graphs
nmr_plot_spectra(dat = drydown_d2o_spectra_processed,
                 binset = bins_Lynch2019,
                 label_position = 5,
                 mapping = aes(x = ppm, y = intensity, group = sampleID, color = sampleID),
                 stagger = 0.5) +
  labs(subtitle = "binset: Lynch et al. 2019")+
  ylim(0, 8)+
  facet_wrap(~sampleID)

nmr_plot_spectra(dat = drydown_d2o_spectra_processed,
                 binset = bins_Lynch2019,
                 label_position = 5,
                 mapping = aes(x = ppm, y = intensity, group = sampleID, color = sampleID),
                 stagger = 0.5) +
  labs(subtitle = "binset: Lynch et al. 2019")+
  ylim(0, 5.5)+
  xlim(10, -2)
  # facet_wrap(~treatment)+
  NULL









# testing with `amp_tempest` data -----------------------------------------

## data generated using MNova
## single-column peaks files


# 1. process spectra (?)
tempest_spectra_processed = import_nmr_spectra_data(SPECTRA_FILES = "inst/extdata/amp_tempest/spectra", METHOD = "mnova") %>% filter(ppm >= 0 & ppm <= 10)
tempest_spectra_processed_bins = assign_compound_classes_v2(tempest_spectra_processed, BINSET = "Hertkorn") %>% filter(!is.na(group))

# spectra graphs (not functions yet)
# tempest_spectra_processed_bins %>%
#   filter(!is.na(group)) %>% filter(group != "NA") %>%
#   ggplot(aes(x = ppm, y = intensity, color = group))+ geom_line()+ facet_wrap(~sampleID) +
#   ylim()
#   scale_x_reverse()


# 2. process peaks
tempest_peaks_processed_sing = process_peaks(PEAKS_FILES = "inst/extdata/amp_tempest/peaks_mnova_single", METHOD = "single column")
tempest_peaks_processed_sing_bins = assign_compound_classes_v2(dat = tempest_peaks_processed_sing, BINSET = "Hertkorn")


# 3. compute relabund - by sample
tempest_relabund_cores_peaks_sing = compute_relabund_cores(DAT = tempest_peaks_processed_sing_bins, METHOD = "peaks")
# tempest_relabund_summary_peaks_sing = compute_relabund_treatments(RELABUND_CORES = tempest_relabund_cores_peaks_sing, TREATMENTS, COREKEY)

# 4. compute relabund - summary by treatment


#
# testing with `meb_incubation` data --------------------------------------

## data generated using TopSpin
## spectra and peaks files

# 1. process spectra
meb_spectra_processed_topspin = import_nmr_spectra_data(SPECTRA_FILES = "inst/extdata/meb_burn/spectra_topspin", METHOD = "topspin")

# 2. process peaks
meb_peaks_processed_topspin = process_peaks(PEAKS_FILES = "inst/extdata/meb_burn/peaks_topspin", METHOD = "topspin")
meb_peaks_processed_topspin_bins = assign_compound_classes_v2(meb_peaks_processed_topspin, BINSET = "Clemente")


# 3. compute relabund - by sample
meb_relabund_cores_auc = compute_relabund_cores(DAT = meb_peaks_processed_topspin_bins %>% rename(intensity = Intensity), METHOD = "AUC")
### `meb_relabund_cores_peaks_mult` does not work for topspin data because there is no "area" column
meb_relabund_cores_peaks_mult = compute_relabund_cores(DAT = meb_peaks_processed_topspin_bins %>% rename(Area = Intensity), METHOD = "peaks")

# 4. compute relabund - summary by treatment
meb_relabund_summary_auc = compute_relabund_treatments(RELABUND_CORES = meb_relabund_cores_auc, TREATMENTS = quos(burn_severity), COREKEY = "inst/extdata/meb_burn/corekey_burn_MEB.csv")
meb_relabund_summary_peaks_mult = compute_relabund_treatments(RELABUND_CORES = meb_relabund_cores_peaks_mult, TREATMENTS = quos(burn_severity), COREKEY = "inst/extdata/meb_burn/corekey_burn_MEB.csv")



########################


relabund_summary_auc %>%
  ggplot(aes(x = treatment, y = relabund_mean, fill = group))+
  geom_bar(position = "stack", stat = "identity")

relabund_summary_peaks_mult %>%
  ggplot(aes(x = treatment, y = relabund_mean, fill = group))+
  geom_bar(position = "stack", stat = "identity")



# misc testing ----
# a. joining bins
spectra1 = read.csv("inst/extdata/spectra_topspin/ascii-spec_W300_HB2_Topspin_Spectra.csv",
                    header=FALSE, col.names = c("x", "intensity", "y", "ppm"))
spectra1_bins = assign_compound_classes(spectra1, BINSET = "Clemente")
spectra1_bins %>% ggplot(aes(x = ppm, y = intensity))+ geom_point()
## this keeps only a subset of the data, we need even the non-matches!!!




y = merge(spectra1, bins_dat, by = c(start <= ppm & ppm <= stop))

library(data.table)
y = foverlaps(spectra1, bins_dat,
              by.x = "ppm", by.y = c("start", "stop"))



x = data.table(seq=c("Chr1", "Chr1", "Chr2", "Chr2", "Chr2"),
               start=c(5,10, 1, 25, 50), end=c(11,20,4,52,60))
y = data.table(chr=c("Chr1", "Chr1", "Chr2"), start=c(1, 15,1),
               end=c(4, 18, 55), geneid=letters[1:3])
setkey(y, chr, start, end)
foverlaps(x, y, by.x=c("seq", "start", "end"),
          type="any", which=TRUE)





#dat_y_stagger <-
dat %>%
  distinct(sampleID) %>%
  mutate(y_factor = row_number() / stagger_factor) %>%
  select(sampleID, y_factor)


# testing Andy SS-NMR data ----
library(tidyverse)
library(nmrrr)

data_wide = read.csv("old/buildmatrix.csv")
data_long =
  data_wide %>%
  pivot_longer(-ppm,
               names_to = "sampleID",
               values_to = "intensity") %>%
  filter(ppm >= 0 & ppm <= 250)


bin_ss_clemente = read.csv("old/ss_nmr_bins_clemente2012.csv")

data_long_bins = nmr_assign_bins(dat = data_long,
                                 binset = bin_ss_clemente)



nmr_plot_spectra(dat = data_long_bins %>% arrange(sampleID, ppm),
                 binset = bin_ss_clemente,
                 mapping = aes(x = ppm, y = intensity/20,
                               group = sampleID,
                               color = sampleID),
                 stagger = 20,
                 label_position = 3.5)+
  theme(axis.text.y = element_blank())+
  xlim(210, 0)

data_long_bins %>%
  filter(sampleID == "BSLE004") %>%
  ggplot(aes(x = ppm, y = intensity))+
  geom_line()


data_long_bins %>%
  filter(sampleID == "BSLE034") %>%
  ggplot(aes(x = ppm, y = intensity))+
  geom_line()

data_relabund = nmr_relabund(dat = data_long_bins %>%
                               filter(intensity >= 0),
                             method = "AUC")

data_relabund %>%
  ggplot(aes(x =  sampleID,
             y = relabund,
             fill = group))+
  geom_bar(stat = "identity")




devtools::install_github("bpbond/nmrrr")

# testing Andy SS-NMR data ----
library(tidyverse)
library(nmrrr)

data_wide = readxl::read_excel("old/plot_scaled_012023.xlsx", sheet = "Sheet1")
data_long =
  data_wide %>%
  pivot_longer(-ppm, names_to = "sampleID", values_to = "intensity")

bin_ss_clemente = read.csv("old/ss_nmr_bins_clemente2012.csv")
bin_ss_emsl = read.csv("old/ss_nmr_bins_emsl.csv")
bin_ss_dummy = read.csv("old/ss_nmr_bins_dummy.csv")

data_long_bins = nmr_assign_bins(dat = data_long,
                                 binset = bin_ss_clemente)



nmr_plot_spectra(dat = data_long_bins %>% arrange(sampleID, ppm),
                 binset = bin_ss_clemente,
                 mapping = aes(x = ppm, y = intensity, group = sampleID),
                 stagger = 5,
                 label_position = 2500)

data_long_bins %>%
  filter(sampleID == "BSLE004") %>%
  ggplot(aes(x = ppm, y = intensity))+
  geom_line()


data_long_bins %>%
  filter(sampleID == "BSLE034") %>%
  ggplot(aes(x = ppm, y = intensity))+
  geom_line()

data_relabund = nmr_relabund(dat = data_long_bins %>% filter(intensity >= 0),
                             method = "AUC")

data_relabund_wide =
  data_relabund %>%
  pivot_wider(names_from = "sampleID", values_from = "relabund")

data_relabund %>%
  ggplot(aes(x =  sampleID, y = relabund, fill = group))+
  geom_bar(stat = "identity")


# test MEB diss files ----
library(nmrrr)
library(tidyverse)
spectra = nmr_import_spectra("old/extdata_large_files/meb_dissertation/spectra", method = "mnova")
spr = read_tsv("old/extdata_large_files/meb_dissertation/spectra/SPR_1_0to5.txt")


spectra %>% ggplot(aes(x = ppm, y = intensity, group = sampleID))+geom_line()
spectra %>%
  nmr_plot_spectra(binset = bins_CadeMenum2015, label_position = 5, aes(x = ppm, y = intensity), stagger = 5)+
  ylim(0, 500)

library(scales)
nmr_plot_spectra(dat = spectra,
                 binset = bins_CadeMenum2015,
                 label_position = 6,
                 mapping = aes(x = ppm,
                               y = intensity/5e9,
                               group = sampleID,
                               color = sampleID),
                 stagger = 3e9) +
  # OPTIONAL PARAMETERS/LAYERS``
  labs(subtitle = "binset: CadeMenum2015")+
  scale_y_continuous(limits=c(-0.1, 8))+
  xlim(45,-30)
  ylim(0, 5.5)


peaks = nmr_import_peaks("old/extdata_large_files/meb_dissertation/peaks", method = "single column")
spectra_scaled <-
  spectra %>%
  group_by(sampleID) %>%
  dplyr::mutate(intensity = (intensity/max(intensity))*100)

nmr_plot_spectra(dat = spectra_scaled,
                 binset = bins_CadeMenum2015,
                 label_position = 8,
                 mapping = aes(x = ppm,
                               y = intensity,
                               group = sampleID,
                               color = sampleID),
                 stagger = 0.5) +
  # OPTIONAL PARAMETERS/LAYERS``
  labs(subtitle = "binset: CadeMenum2015")+
  scale_y_continuous(limits=c(-0.1, 10))+
  xlim(45,-30)


nmr_plot_spectra_2(dat = spectra,
                 binset = bins_CadeMenum2015,
                 label_position = 40e9,
                 mapping = aes(x = ppm,
                               y = intensity,
                               group = sampleID,
                               color = sampleID),
                 stagger = 3e9) +
  # OPTIONAL PARAMETERS/LAYERS``
  labs(subtitle = "binset: CadeMenum2015")+
  scale_y_continuous(limits=c(-5e8, 50e9))+
  xlim(45,-30)




spectra %>%
  filter(sampleID == "SPR_1_0to5") %>%
  ggplot(aes(x = ppm, y = intensity, group = sampleID))+geom_line()





nmr_plot_spectra_2 <- function(dat, binset, label_position, mapping, stagger) {

  if(!class(mapping) == "uneval") {
    stop("'mapping' must be a ggplot2::aes() output")
  }

  # Quiet R CMD CHECK notes
  start <- number <- sampleID <- newsource <- NULL

  # create spectra-base plot ----
  odds <- binset[seq(1, nrow(binset), by = 2),]
  evens <- binset[seq(2, nrow(binset), by = 2),]

  label_stagger = label_position/50
  spectra_base <-
    ggplot() +
    # stagger bracketing lines for odd vs. even rows
    geom_segment(
      data = evens,
      aes(x = start, xend = stop, y = label_position, yend = label_position),
      color = "black"
    ) +
    geom_segment(
      data = odds,
      aes(x = start, xend = stop, y = label_position - (2*label_stagger), yend = label_position - (2*label_stagger)),
      color = "black"
    ) +
    # stagger numbering like the lines
    geom_text(
      data = evens,
      aes(x = (start + stop) / 2, y = label_position + label_stagger, label = number)
    ) +
    geom_text(
      data = odds,
      aes(x = (start + stop) / 2, y = label_position - label_stagger, label = number)
    ) +
    scale_x_reverse() +
    xlab("shift, ppm") +
    ylab("intensity")

  # add staggering factor ----

  stagger_factor <- 1 / stagger
  dat_y_stagger <- weak_tibble(
    sampleID = unique(dat$sampleID),
    y_factor = (seq_along(sampleID) - 1) / stagger_factor
  )

  spectra_new <- merge(dat, dat_y_stagger, by = "sampleID")
  spectra_new$intensity <- spectra_new$intensity + spectra_new$y_factor

  # combined plot ----

  spectra_base +
    geom_path(data = spectra_new, mapping)
}



nmr_plot_spectra_2(dat = data_long_bins,
                   binset = bins_ss_Clemente2012,
                   label_position = 65,
                   mapping = aes(x = ppm,
                                 y = intensity,
                                 group = sampleID,
                                 color = sampleID),
                   stagger = 15) +
  # OPTIONAL PARAMETERS/LAYERS``
  labs(subtitle = "binset: Clemente2012")+
  scale_y_continuous(limits=c(-5e8, 60e9))+
  xlim(45,-30)


#
# notes from meeting with NMR folks ----

# plug in to NMR analysis
# bins: capture. type of sample for the bins. caveats: these are all putative shifts
# but some of the groups did multi-dimensional tests for these bins
# 2019-EMSL NMR group had a lot of discussion about it
# strength of this tool: it is a standardized
#
# strength of this tool: you're not constrained by the conventional bins.
# krnel based binning. instead of rectangular, do gaussian binning
#
# give context to the different bins. give disclaimer to users.
#
# DSS binds to DOM, so peak widening of the DSS peak.
# concentration-normalized spectra --> put in the manuscript.
#
# tutorial-style manuscript.
# the decisions you make at the beginning affect everything you do.

#
# load and save bins ----
bins_ss_Baldock2004 = read.csv("old/bins/ss_nmr_bins_Baldock2004.txt")
save(bins_ss_Baldock2004, file = "data/bins_ss_Baldock2004.rda")
load("data/bins_ss_Baldock2004.rda")

bins_ss_Preston2009 = read.csv("old/bins/ss_nmr_bins_Preston2009.txt")
save(bins_ss_Preston2009, file = "data/bins_ss_Preston2009.rda")

bins_ss_Clemente2012 = read.csv("old/bins/ss_nmr_bins_clemente2012.csv")
save(bins_ss_Clemente2012, file = "data/bins_ss_Clemente2012.rda")

bins_CadeMenun2015 = read.csv("old/bins/bins_CadeMenun2015_p.csv")
save(bins_CadeMenun2015, file = "data/bins_CadeMenun2015.rda")

