
#
# TESTING NMRRR WORKFLOW --------------------------------------------------
library(tidyverse)

# 1. process spectra
spectra_processed = import_nmr_spectra_data(SPECTRA_FILES = "inst/extdata/spectra")
spectra_processed_bins = assign_compound_classes(spectra_processed, BINSET = "Clemente")


# 2. process peaks
peaks_processed_mult = process_peaks(PEAKS_FILES = "inst/extdata/peaks", METHOD = "multiple columns", BINSET = "Clemente")
peaks_processed_sing = process_peaks(PEAKS_FILES = "inst/extdata/peaks2", METHOD = "single column", BINSET = "Hertkorn")
peaks_processed_topspin = process_peaks(PEAKS_FILES = "inst/extdata/peaks_topspin", METHOD = "topspin")
peaks_processed_topspin2 = assign_compound_classes(peaks_processed_topspin, BINSET = "Clemente")



# 3. compute relabund - by sample
relabund_cores_auc = compute_relabund_cores(DAT = spectra_processed_bins, METHOD = "AUC")
relabund_cores_peaks_mult = compute_relabund_cores(DAT = peaks_processed_mult, METHOD = "peaks")
relabund_cores_peaks_sing = compute_relabund_cores(DAT = peaks_processed_sing, METHOD = "peaks")

# 4. compute relabund - summary by treatment
COREKEY = "inst/extdata/corekey_hyst_KFP.csv"
TREATMENTS = quos(treatment)

relabund_summary_auc = compute_relabund_treatments(RELABUND_CORES = relabund_cores_auc, TREATMENTS, COREKEY)
relabund_summary_peaks_mult = compute_relabund_treatments(RELABUND_CORES = relabund_cores_peaks_mult, TREATMENTS, COREKEY)
relabund_summary_peaks_sing = compute_relabund_treatments(RELABUND_CORES = relabund_cores_peaks_sing, TREATMENTS, COREKEY)

relabund_summary_auc %>%
  ggplot(aes(x = treatment, y = relabund_mean, fill = group))+
  geom_bar(position = "stack", stat = "identity")

relabund_summary_peaks_mult %>%
  ggplot(aes(x = treatment, y = relabund_mean, fill = group))+
  geom_bar(position = "stack", stat = "identity")
