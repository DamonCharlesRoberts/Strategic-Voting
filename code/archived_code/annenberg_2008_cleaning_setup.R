#### 2008 Annenberg Cleaning: Setup ####

#### Notes: ####
  ### Description: Setup file for cleaning Annenberg dataset ###

#### Modules ####
#' @export annenberg2008

#### Files: ####
  ### In: 
  ### Out: 

#### Setup ####
box::use(
    here = here[here],
    readr = readr[read_delim],
    dplyr = dplyr[select, filter, mutate, vars, mutate_at, case_when],
    magrittr = magrittr[...],
    haven = haven[write_dta]
)
here()
annenberg2008 <- read_delim("Data/annenberg_2008_wave2/annenberg_2008_wave2.txt", delim = "\t")