### Brazilian regions, etc


#' Convert Brazilian state identifiers
#'
#' @param x A vector of state codes (2-digit numbers) or abbreviations (2-letter codes)
#' @param to Output format: "code", "abbreviation", or "region"
#' @param from Input format: "auto" (detect), "code", or "abbreviation"
#' @param language Language for region names: "en" (English, default) or "pt" (Portuguese)
#'
#' @return A vector of converted values
#' @export
#'
#' @examples
#' br_state(c(12, "SP", "21", "mg"), to = "abbreviation")
#' br_state(c("AC", "SP", "RS"), to = "region") # Returns English regions by default
br_state <- function(x, to = c("code", "abbreviation", "region"), 
                     from = c("auto", "code", "abbreviation"),
                     language = c("en", "pt")) {
    
    # Brazilian state database
    states <- data.frame(
        code = c("12", "27", "16", "13", "29", "23", "53", "32", "52", 
                 "21", "51", "50", "31", "15", "25", "41", "26", "22", 
                 "24", "43", "33", "11", "14", "42", "35", "28", "17"),
        abbreviation = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
                         "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
                         "RN", "RS", "RJ", "RO", "RR", "SC", "SP", "SE", "TO"),
        region_pt = c("Norte", "Nordeste", "Norte", "Norte", "Nordeste", "Nordeste",
                      "Centro-Oeste", "Sudeste", "Centro-Oeste", "Nordeste",
                      "Centro-Oeste", "Centro-Oeste", "Sudeste", "Norte", "Nordeste",
                      "Sul", "Nordeste", "Nordeste", "Nordeste", "Sul", "Sudeste",
                      "Norte", "Norte", "Sul", "Sudeste", "Nordeste", "Norte"),
        region_en = c("North", "Northeast", "North", "North", "Northeast", "Northeast",
                      "Central-West", "Southeast", "Central-West", "Northeast",
                      "Central-West", "Central-West", "Southeast", "North", "Northeast",
                      "South", "Northeast", "Northeast", "Northeast", "South", "Southeast",
                      "North", "North", "South", "Southeast", "Northeast", "North"),
        stringsAsFactors = FALSE
    )
    
    # Standardize inputs
    x <- as.character(x)
    to <- match.arg(to)
    from <- match.arg(from)
    language <- match.arg(language)
    
    # Auto-detect input type if needed
    if (from == "auto") {
        from <- ifelse(grepl("^[0-9]{2}$", x), "code", "abbreviation")
    }
    
    # Convert to uppercase for case-insensitive matching
    x_clean <- toupper(x)
    states$abbreviation_clean <- toupper(states$abbreviation)
    
    # Determine region column to use (English default)
    region_col <- ifelse(language == "pt", "region_pt", "region_en")
    
    # Perform conversion
    result <- character(length(x))
    
    for (i in seq_along(x)) {
        if (from == "code" && to == "abbreviation") {
            idx <- which(states$code == x[i])
            result[i] <- if (length(idx) > 0) states$abbreviation[idx] else NA
        } else if (from == "abbreviation" && to == "code") {
            idx <- which(states$abbreviation_clean == x_clean[i])
            result[i] <- if (length(idx) > 0) states$code[idx] else NA
        } else if (to == "region") {
            if (from == "code") {
                idx <- which(states$code == x[i])
            } else {
                idx <- which(states$abbreviation_clean == x_clean[i])
            }
            result[i] <- if (length(idx) > 0) states[[region_col]][idx] else NA
        } else {
            result[i] <- NA  # Unsupported conversion
        }
    }
    
    # Return appropriate type
    if (to == "code") {
        return(as.numeric(result))
    } else {
        return(result)
    }
}