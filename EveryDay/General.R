### This is a R file with useful functions I have developed, or forked from others, or
### adapted from others.

## Data management

# from https://github.com/opensafely/comparative-ve-research/blob/main/analysis/lib/utility_functions.R

fct_case_when <- function(...) {
    # uses dplyr::case_when but converts the output to a factor,
    # with factors ordered as they appear in the case_when's  ... argument
    args <- as.list(match.call())
    levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
    levels <- levels[!is.na(levels)]
    factor(dplyr::case_when(...), levels=levels)
}