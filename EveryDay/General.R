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


### Chapters from ICD-10
### adapted from this repo https://github.com/walterpedro/artigo-completitude-escolaridade-obito/tree/main


criar_capitulo_CID <- function(x) {
    
    capitulos_cid <- readxl::read_excel("Data/capitulos-cid-10.xlsx") %>%
        slice(-22) %>%
        mutate(capitulo_CID = paste0(capitulo, ". ", descricao))
    
    capitulos_cid2 <- readxl::read_excel("Data/causabas-capitulo_cid.xlsx")
    
    result <- data.frame(CAUSABAS = x) %>%
        mutate(CAUSABAS = as.character(CAUSABAS)) %>%
        left_join(
            capitulos_cid2,
            by = c("CAUSABAS")
        ) %>%
        with(capitulo_CID)
    
    factor(result, levels = capitulos_cid$capitulo_CID)
}




get_chapter_ICD10 <- function(x) {
    
    capitulos_cid <- readxl::read_excel("Data/chapter_icd_10.xlsx") %>%
        slice(-22) %>%
        mutate(capitulo_CID = paste0(capitulo, ". ", descricao))
    
    capitulos_cid2 <- readxl::read_excel("Data/underlyingcause_chapter_icd10.xlsx")
    
    result <- data.frame(CAUSABAS = x) %>%
        mutate(CAUSABAS = as.character(CAUSABAS)) %>%
        left_join(
            capitulos_cid2,
            by = c("CAUSABAS")
        ) %>%
        with(capitulo_CID)
    
    factor(result, levels = capitulos_cid$capitulo_CID)
}

## this from icd R package 
icd10_chapters <- list(
    "Certain infectious and parasitic diseases" = c(start = "A00", end = "B99"),
    "Neoplasms" = c(start = "C00", end = "D49"),
    "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism" =
        c(start = "D50", end = "D89"),
    "Endocrine, nutritional and metabolic diseases" = c(start = "E00", end = "E89"),
    "Mental, Behavioral and Neurodevelopmental disorders" = c(start = "F01", end = "F99"),
    "Diseases of the nervous system" = c(start = "G00", end = "G99"),
    "Diseases of the eye and adnexa" = c(start = "H00", end = "H59"),
    "Diseases of the ear and mastoid process" = c(start = "H60", end = "H95"),
    "Diseases of the circulatory system" = c(start = "I00", end = "I99"),
    "Diseases of the respiratory system" = c(start = "J00", end = "J99"),
    "Diseases of the digestive system" = c(start = "K00", end = "K95"),
    "Diseases of the skin and subcutaneous tissue" = c(start = "L00", end = "L99"),
    "Diseases of the musculoskeletal system and connective tissue" = c(start = "M00", end = "M99"),
    "Diseases of the genitourinary system" = c(start = "N00", end = "N99"),
    "Pregnancy, childbirth and the puerperium" = c(start = "O00", end = "O9A"),
    "Certain conditions originating in the perinatal period" = c(start = "P00", end = "P96"),
    "Congenital malformations, deformations and chromosomal abnormalities" = c(start = "Q00", end = "Q99"),
    "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" =
        c(start = "R00", end = "R99"),
    "Injury, poisoning and certain other consequences of external causes" = c(start = "S00", end = "T88"),
    "External causes of morbidity" = c(start = "V00", end = "Y99"),
    "Factors influencing health status and contact with health services" = c(start = "Z00", end = "Z99")
)

