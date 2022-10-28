
#' NMR grouping bins from Clemente et al. (2012).
#'
#' NMR grouping bins from Clemente et al. (2012), using DMSO-D6 as solvent.
#' (1) aliphatic polymethylene and methyl groups (0.6–1.3 ppm, “aliphatic1”);
#' (2) aliphatic methyl and methylene near O and N (1.3–2.9 ppm, “aliphatic2”);
#' (3) O-alkyl, mainly from carbohydrates and lignin (2.9–4.1 ppm);
#' (4) α-proton of peptides (4.1–4.8 ppm);
#' (5) aromatic and phenolic (6.2–7.8 ppm); and
#' (6) amide, from proteins (7.8–8.4 ppm).
#'
#' @format A data frame with 6 rows and 5 variables:
#' \describe{
#'   \item{number}{Bin number}
#'   \item{group}{Name of bin group}
#'   \item{start}{ppm shift range, lower limit}
#'   \item{stop}{ppm shift range, upper limit}
#'   \item{description}{Description of the bin group}
#' }
#' @note The NMR spectrum can be split into several bins, based on
#' chemical shift (ppm). Binsets are specific to nuclei and solvents and
#' by definition are open on the left and closed on the right;
#' for example, a bin of (0,1) includes 1 but \emph{not} 0.
#' @source JS Clemente et al. 2012. “Comparison of Nuclear Magnetic Resonance
#' Methods for the Analysis of Organic Matter Composition from Soil Density and
#' Particle Fractions.” Environmental Chemistry
#' \doi{10.1071/EN11096}
#' @seealso \code{\link{bins_Lynch2019}}
#' \code{\link{bins_Mitchell2018}} \code{\link{bins_Hertkorn2013}}
"bins_Clemente2012"


#' NMR grouping bins from Lynch et al. (2019).
#'
#' NMR grouping bins from Lynch et al. (2019), using D2O as solvent.
#' (1) methyl, methylene, and methane bearing protons (0.6–1.6 ppm) ;
#' (2) unsaturated functional groups (1.6–3.2 ppm),
#' including ketone, benzylic, and alicyclic-bearing protons;
#' (3) unsaturated, heteroatomic compounds,
#' including O-bearing carbohydrates, ethers, and alcohols (3.2–4.5 ppm);
#' (4) conjugated, double bond functionalities,
#' including aromatic, amide, and phenolic structures (6.5–8.5 ppm).
#'
#' @format A data frame with 4 rows and 5 variables:
#' \describe{
#'   \item{number}{Bin number}
#'   \item{group}{Name of bin group}
#'   \item{start}{ppm shift range, lower limit}
#'   \item{stop}{ppm shift range, upper limit}
#'   \item{description}{Description of the bin group}
#' }
#' @note The NMR spectrum can be split into several bins, based on
#' chemical shift (ppm). Binsets are specific to nuclei and solvents and
#' by definition are open on the left and closed on the right;
#' for example, a bin of (0,1) includes 1 but \emph{not} 0.
#' @source LM Lynch et al. 2019. “Dissolved Organic Matter Chemistry and
#' Transport along an Arctic Tundra Hillslope.” Global Biogeochemical Cycles
#' \doi{10.1029/2018GB006030}
#' @seealso \code{\link{bins_Clemente2012}}
#' \code{\link{bins_Mitchell2018}} \code{\link{bins_Hertkorn2013}}
"bins_Lynch2019"


#' NMR grouping bins from Mitchell et al. (2018).
#'
#' NMR grouping bins from Mitchell et al. (2018), using DMSO-D6 as solvent.
#' (1) aliphatic polymethylene and methyl groups (0.6–1.3 ppm);
#' (2) N- and O-substituted aliphatic (1.3–2.9 ppm);
#' (3) O-alkyl (2.9–4.1 ppm);
#' (4) α-proton of peptides (4.1–4.8 ppm);
#' (5) anomeric proton of carbohydrates (4.8–5.2 ppm);
#' (6) aromatic and phenolic (6.2–7.8 ppm);
#' (7) amide (7.8–8.4 ppm).
#'
#' @format A data frame with 7 rows and 5 variables:
#' \describe{
#'   \item{number}{Bin number}
#'   \item{group}{Name of bin group}
#'   \item{start}{ppm shift range, lower limit}
#'   \item{stop}{ppm shift range, upper limit}
#'   \item{description}{Description of the bin group}
#' }
#' @note The NMR spectrum can be split into several bins, based on
#' chemical shift (ppm). Binsets are specific to nuclei and solvents and
#' by definition are open on the left and closed on the right;
#' for example, a bin of (0,1) includes 1 but \emph{not} 0.
#' @source P Mitchell et al. 2018. “Nuclear Magnetic Resonance Analysis of
#' Changes in Dissolved Organic Matter Composition with Successive Layering
#' on Clay Mineral Surfaces.” Soil Systems
#' \doi{10.3390/soils2010008}
#' @seealso \code{\link{bins_Clemente2012}} \code{\link{bins_Lynch2019}}
#' \code{\link{bins_Hertkorn2013}}
"bins_Mitchell2018"


#' NMR grouping bins from Hertkorn et al. (2013).
#'
#' NMR grouping bins from  Hertkorn et al. (2013), using MeOD as solvent.
#' (1) aliphatics, HCCC (0.0-1.9);
#' (2) acetate analogs and CRAM (carboxyl-rich alicyclic materials), HCX (1.9-3.1);
#' (3) carbohydrate-like and methoxy, HCO (3.1-4.9);
#' (4) olefinic HC=C (5.3-7.0);
#' (5) aromatic (7.0-10.0).
#' @format A data frame with 5 rows and 5 variables:
#' \describe{
#'   \item{number}{Bin number}
#'   \item{group}{Name of bin group}
#'   \item{start}{ppm shift range, lower limit}
#'   \item{stop}{ppm shift range, upper limit}
#'   \item{description}{Description of the bin group}
#' }
#' @note The NMR spectrum can be split into several bins, based on
#' chemical shift (ppm). Binsets are specific to nuclei and solvents and
#' by definition are open on the left and closed on the right;
#' for example, a bin of (0,1) includes 1 but \emph{not} 0.
#' @source N. Hertkorn et al. 2013. "High-field NMR spectroscopy and FTICR mass
#' spectrometry: powerful discovery tools for the molecular level
#' characterization of marine dissolved organic matter" Biogeosciences
#' \doi{10.5194/bg-10-1583-2013}
#' @seealso \code{\link{bins_Clemente2012}} \code{\link{bins_Lynch2019}}
#' \code{\link{bins_Mitchell2018}}
"bins_Hertkorn2013"
