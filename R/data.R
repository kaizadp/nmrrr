
#' NMR grouping bins from Clemente et al. (2012).
#'
#' NMR grouping bins from Clemente et al. (2012), using DMSO-D6 as solvent.
#' (1) aliphatic polymethylene and methyl groups (0.6–1.3 ppm, “aliphatic1”);
#' (2) aliphatic methyl and methylene near O and N (1.3–2.9 ppm, “aliphatic2”);
#' (3) O-alkyl, mainly from carbohydrates and lignin (2.9–4.1 ppm);
#' (4) alpha-proton of peptides (4.1–4.8 ppm);
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
#' (4) alpha-proton of peptides (4.1–4.8 ppm);
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

#' NMR grouping bins from Preston et al. (2009).
#'
#' NMR grouping bins from  Preston et al. (2009), for solid-state NMR.
#' (1) alkyl C (0-50);
#' (2) methoxyl C (50-60);
#' (3) O-alkyl C (60-93);
#' (4) di-O-alkyl C (93-112);
#' (5) aromatic C (112-140);
#' (6) phenolic C (140-165);
#' (7) carboxyl C (165-190)
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
#' @source C. Preston et al. 2009. "Chemical Changes During
#' 6 Years of Decomposition of 11 Litters in Some Canadian Forest Sites.
#' Part 1. Elemental Composition, Tannins, Phenolics, and Proximate Fractions". Ecosystems.
#' \doi{10.1007/s10021-009-9266-0}
#' @seealso \code{\link{bins_Clemente2012}} \code{\link{bins_Lynch2019}}
#' \code{\link{bins_Mitchell2018}}
"bins_ss_Preston2009"

#' NMR grouping bins from Baldock et al. (2004).
#'
#' NMR grouping bins from  Baldock et al. (2004), for solid-state NMR.
#' (1) alkyl C (0-45);
#' (2) methoxyl C and N-alkyl C (45-60);
#' (3) O-alkyl C (60-95);
#' (4) di-O-alkyl C (95-110);
#' (5) aromatic C (110-145);
#' (6) phenolic C (145-165);
#' (7) amide and carboxyl C (165-215)
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
#' @source J. Baldock et al. "Cycling and composition of organic matter in terrestrial and marine ecosystems". Marine Chemistry.
#' \doi{10.1016/j.marchem.2004.06.016}
#' @seealso \code{\link{bins_Clemente2012}} \code{\link{bins_Lynch2019}}
#' \code{\link{bins_Mitchell2018}}
"bins_ss_Baldock2004"

#' NMR grouping bins from Clemente et al. (2012) - ss.
#'
#' NMR grouping bins from  Clemente et al. (2012), for solid-state NMR.
#' (1) alkyl C (0-50);
#' (2) O-alkyl C (60-93);
#' (3) anomeric C (95-110);
#' (4) aromatic C (110-160);
#' (5) carboxyl-carbonyl C (160-200)
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
#' @source JS Clemente et al. 2012. “Comparison of Nuclear Magnetic Resonance
#' Methods for the Analysis of Organic Matter Composition from Soil Density and
#' Particle Fractions.” Environmental Chemistry
#' \doi{10.1071/EN11096}
#' @seealso \code{\link{bins_Clemente2012}} \code{\link{bins_ss_Preston2009}}
#' \code{\link{bins_Mitchell2018}}
"bins_ss_Clemente2012"


#' NMR grouping bins from Cade-Menun (2015).
#'
#' NMR grouping bins from  Cade-Menun (2015), for 31P, using D2O as a solvent.
#' (1) polyphosphate (-20 to -4);
#' (2) diester (-1.5 to 2.0);
#' (3) monoester (3.0 to 5.5);
#' (4) orthophosphate (5.5 to 9.0);
#' (5) phosphate (9.0 to 40.0)
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
#' @source B. Cade-Menun. "Improved peak identification in 31P-NMR spectra of environmental samples
#' with a standardized method and peak library". Geoderma.
#' \doi{10.1016/j.geoderma.2014.12.016}
#' @seealso \code{\link{bins_Clemente2012}} \code{\link{bins_Lynch2019}}
#' \code{\link{bins_Mitchell2018}}
"bins_CadeMenun2015"
