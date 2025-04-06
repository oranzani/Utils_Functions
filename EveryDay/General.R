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


## count prop one variable
## optimized with DeepSeek

#' Calculate Proportions with Grouping Options
#'
#' @param data A data frame or tibble
#' @param var1 Primary grouping variable (unquoted)
#' @param var2 Optional secondary grouping variable (unquoted, NULL by default)
#' @param prop Calculate proportion (default = FALSE)
#' @param percent Calculate percentage (default = TRUE)
#' @param digits Number of decimal places (default = 1)
#' @param sort Sort order ("desc" [default], "asc", or FALSE)
#' @param na.rm Remove NA values (default = FALSE)
#' @param name Name for count column (default = "n")
#' @param group_by_var1 Calculate proportions within var1 groups (default = when var2 present)
#'
#' @return A tibble with counts and calculated proportions/percentages
#' @export
#'
#' @examples
#' # Single variable (like before)
#' mtcars %>% count_prop2(cyl)
#' 
#' # Two variables with within-group percentages
#' mtcars %>% count_prop2(gear, carb)
count_prop <- function(data, var1, var2 = NULL, 
                        prop = FALSE, 
                        percent = TRUE, 
                        digits = 1, 
                        sort = "desc", 
                        na.rm = FALSE, 
                        name = "n",
                        group_by_var1 = !missing(var2)) {
    
    # Input validation
    if (!is.data.frame(data)) stop("Input must be a data frame")
    
    var1 <- rlang::enquo(var1)
    var2 <- rlang::enquo(var2)
    
    # Count operation
    if (rlang::quo_is_null(var2)) {
        result <- data %>%
            dplyr::count(!!var1, name = name, na.rm = na.rm)
    } else {
        result <- data %>%
            dplyr::count(!!var1, !!var2, name = name, na.rm = na.rm)
    }
    
    # Calculate proportions
    if (prop || percent) {
        if (group_by_var1 && !rlang::quo_is_null(var2)) {
            result <- result %>%
                dplyr::group_by(!!var1)
        }
        
        total <- sum(result[[name]], na.rm = TRUE)
        
        if (prop) {
            result <- result %>%
                dplyr::mutate(prop = round(.data[[name]] / total, digits + 2))
        }
        
        if (percent) {
            result <- result %>%
                dplyr::mutate(pct = round(100 * .data[[name]] / total, digits))
        }
        
        if (group_by_var1) {
            result <- dplyr::ungroup(result)
        }
    }
    
    # Apply sorting
    if (sort == "desc") {
        result <- result %>% dplyr::arrange(-.data[[name]])
    } else if (sort == "asc") {
        result <- result %>% dplyr::arrange(.data[[name]])
    }
    
    return(result)
}


