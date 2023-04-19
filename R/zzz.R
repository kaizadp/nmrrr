.onAttach <- function(libname, pkgname) {
  tip <- random_tip()
  packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
}

random_tip <- function() {
  # Idea from https://github.com/bpbond/cosore/blob/master/R/zzz.R
  # and https://github.com/tidyverse/ggplot2/blob/master/R/zzz.r
  tips <- c(
    "Willkommen, Bienvenue, Welcome!",
    "Use `citation('nmrrr') to cite this package"
  )

  sample(tips, 2)
}
