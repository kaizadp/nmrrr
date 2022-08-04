# update package descriptions
roxygen2::roxygenise()


#
# TESTING NMRRR WORKFLOW --------------------------------------------------
library(tidyverse)
source("R/1-functions_processing.R")
source("R/1-functions_relabund.R")

# testing with `kfp_hysteresis` data --------------------------------------

## data generated using MNova
## spectra files and multi-column peaks files

# 1. process spectra
#spectra_processed = import_nmr_spectra_data(SPECTRA_FILES = "inst/extdata/spectra") #old code
hyst_spectra_processed = import_nmr_spectra_data(SPECTRA_FILES = "inst/extdata/kfp_hysteresis/spectra_mnova", METHOD = "mnova") %>% filter(ppm >= 0 & ppm <= 10)

hyst_spectra_processed_bins = assign_compound_classes_v2(spectra_processed, BINSET = "Clemente") %>% filter(!is.na(group))
# hyst_spectra_processed_bins2 = assign_compound_classes(spectra_processed, BINSET = "Clemente")

# spectra graphs (not functions yet)
hyst_spectra_processed_bins %>%
  filter(!is.na(group)) %>% filter(group != "NA") %>%
  ggplot(aes(x = ppm, y = intensity, color = group))+ geom_line()+ facet_wrap(~sampleID) + ylim(0, 5)+
  scale_x_reverse()

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
# testing with `amp_tempest` data -----------------------------------------

## data generated using MNova
## single-column peaks files


# 1. process spectra (?)


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
meb_spectra_processed_topspin = import_nmr_spectra_data(SPECTRA_FILES = "inst/extdata/meb_incubation/spectra_topspin", METHOD = "topspin")

# 2. process peaks
meb_peaks_processed_topspin = process_peaks(PEAKS_FILES = "inst/extdata/meb_incubation/peaks_topspin", METHOD = "topspin")
meb_peaks_processed_topspin_bins = assign_compound_classes_v2(meb_peaks_processed_topspin, BINSET = "Clemente")


# 3. compute relabund - by sample
meb_relabund_cores_auc = compute_relabund_cores(DAT = meb_peaks_processed_topspin_bins %>% rename(intensity = Intensity), METHOD = "AUC")
### `meb_relabund_cores_peaks_mult` does not work for topspin data because there is no "area" column
meb_relabund_cores_peaks_mult = compute_relabund_cores(DAT = meb_peaks_processed_topspin_bins, METHOD = "peaks")

# 4. compute relabund - summary by treatment



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

