## ==================================================================================
# NSS 55th Round (July 1999 to June 2000), Schedule 10 (with Schedule 10.1)
# harmonization within this round for women born between 1980 and 1984 (cohort 1)
#
# The column names, widths and file structure are taken from the portal's DDI:
# DDI-IND-MOSPI-NSSO-55Rnd-Sch10-and-10dot1-1999-2000.xml. The DDI does not include
# value labels, so the code lists below come from the Schedule 10 PDF for the
# 55th round. These codes differ for each round.
#
# The script saves a harmonized person-level file, a cohort 1 CSV, LFPR tables,
# a reconciliation table for all women aged 15+, and diagnostic output on screen. 
# Files saved to out_55/.
## ==================================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(haven); library(survey)
})
options(survey.lonely.psu = "adjust")

## ========================= Helper Functions ========================= 

# Estimates to 5 sf
# sf5() converts values to character strings with 5 sf + trailing zeros
sf5 <- function(x) vapply(x, function(v) {
  if (is.na(v)) return(NA_character_)
  if (v == 0)   return("0.0000")
  d <- max(0, 4 - floor(log10(abs(v))))
  formatC(round(v, d), format = "f", digits = d)
}, character(1))

# print5() displays selected cols in a df to 5 sf
print5 <- function(df, cols) {
  df <- as.data.frame(df); df[cols] <- lapply(df[cols], sf5); print(df, row.names = FALSE)
}

# ----------------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------------

CFG <- list(
  data_dir   = "nss_55_1999_00/data/Emp_Unemp_1999_2000_STATA",
  out_dir    = "out_55",
  layout_csv = "nss_55_1999_00/documentation/nss55_ddi_layout.csv",
  cohort_years = 1980:1984,
  
  # "interview_year" uses birth year as interview year (1999 for sub-rounds 1-2, 2000 for sub-rounds 3-4) minus age
  # so we have 15-19 in sub-rounds 1-2 and ages 16-20 in sub-rounds 3-4
  # "round_start" uses 1999 minus age, keeping age as 15-19 only
  cohort_rule  = "interview_year",
  
  # "FALSE" uses only the Schedule 10 files (596,686 people) and "TRUE" uses the combined Schedule 10 and 10.1 files (819,011 people)
  use_combined = TRUE,
  
  # "computed" uses the multiplier rule in How_to_Use_MULTIPLIERS_...pdf to Wgt and ss_replicate
  # "provided" uses the multiplier already supplied in the file (Wgt_SR_comb / Wgt_10_10pt1_SR_comb)
  weight_rule  = "computed",
  
  # For Schedule 10 files only, check if second-stage stratum 9 requires removal since it adds new households during a revisit
  drop_sss9    = FALSE,
  
  # For fixed-width files, set to TRUE - divides multipliers by 100
  fwf_implied_decimals = FALSE,
  sex_female   = 2,                                   
  sector_codes = c(`1` = "Rural", `2` = "Urban"),    
  educ_map_verified = TRUE,                           
  
  # If the same person key appears multiple times in Block 4/5.1, only first copy is kept
  # If the records differ, due to status code mismatch etc., "stop" halts and shows the entries
  # "keep_first" keeps the first record and displays a warning
  # If any part of a key is blank, "stop" halts and shows the entries
  # "drop" removes them and they have no status record hence excluded from LFPR calculations
  
  blank_key_policy = "drop", # can also be STOP to stop the run here - here only 32: Lakshadweep entries are dropped
  dup_policy   = "stop",
  inspect_only = FALSE    # set TRUE to display mapped layout
)

# ----------------------------------------------------------------------------
# File names and linking using identifiers
# ----------------------------------------------------------------------------

# household key constructed
KEY_HH <- c("sector", "state", "subround", "subsample", "fsu", "visit", "seg",
            "sss", "hh_no")

key_map <- function(hh_no = "Hhold_Slno")
  c(sector = "Sector", state = "State", subround = "Sub_round",
    subsample = "Sub_sample", fsu = "fsu_no", visit = "Visit_no",
    seg = "Seg_no", sss = "Stg2_stratm", hh_no = hh_no)

# Stata - .dta files downloaded
EXT <- ".dta" 
fn  <- function(title) paste0(title, EXT)

T10 <- list(
  hh  = "Block3-sch10--Household-Characteristics-records",
  b4  = "Block4-sch10-persons-demographic-migration-records",
  b51 = "Block51-sch10-Persons-usual-principal-activity-Records",
  b52 = "Block52-sch10-Persons-subsidiary-activity-Records")
TC  <- list(
  b4  = "Block4-sch10-and-10dot1-Records-combined",
  b51 = "Block51-sch10-and-10dot1-Records-combined",
  b52 = "Block52-sch10-and-10dot1-Records-combined")

W_COLS <- c(nss = "Nss", ss_rep = "ss_replicate", mult = "Wgt")

BLOCKS_10 <- list(
  # Block 3 contains 120,578 households
  # DDI has B3_q5 as MPCE but it is total household expenditure as per Schedule 10 
  # B3_q1 is not in DDI so MPCE is calculated using Block 4 household size
  
  hh  = list(title = T10$hh, file = fn(T10$hh),
             cols = c(key_map("Hhhold_Slno"), soc_group = "B3_q2",
                      religion = "B3_q3", hh_type = "B3_q4",
                      hh_exp_month = "B3_q5",   # total household expenditure in the previous month
                      survey_code = "Survey_code",
                      W_COLS, mult_comb = "Wgt_SR_comb")),
  b4  = list(title = T10$b4, file = fn(T10$b4),
             cols = c(key_map(), pid = "Prsn_Slno_B4_q1", rel = "B4_q3",
                      sex = "B4_q4", age = "B4_q5", marital = "B4_q6",
                      educ_raw = "B4_q7", tech = "B4_q8", attend = "B4_q9",
                      region = "Region", stratum = "Stratum", district = "District",
                      W_COLS, mult_comb = "Wgt_SR_Comb")),
  b51 = list(title = T10$b51, file = fn(T10$b51),
             cols = c(key_map(), pid = "Prsn_slno_B51_q1", ups = "B51_q3",
                      sub_flag = "B51_q7")),
  b52 = list(title = T10$b52, file = fn(T10$b52),
             cols = c(key_map(), pid = "Prsn_Slno_B52_q1", uss = "B52_q3"))
)
BLOCKS_COMB <- list(
  hh  = NULL,
  b4  = list(title = TC$b4, file = fn(TC$b4),
             cols = c(key_map(), pid = "B4_q1", rel = "B4_q3", sex = "B4_q4",
                      age = "B4_q5", marital = "B4_q6", educ_raw = "B4_q7",
                      tech = "B4_q8", attend = "B4_q9", region = "Region",
                      stratum = "Stratum", district = "District", W_COLS,
                      mult_comb = "Wgt_10_10pt1_SR_comb")),
  b51 = list(title = TC$b51, file = fn(TC$b51),
             cols = c(key_map(), pid = "B51_q1", ups = "B51_q3", sub_flag = "B51_q7")),
  b52 = list(title = TC$b52, file = fn(TC$b52),
             cols = c(key_map("samp_hholdno"), pid = "B52_q1", uss = "B52_q3"))
)

# 01-04 mean people who are not literate or have literacy through NFEC/AEC, TLC or other routes
# 05 = below primary, 06 = primary, 07 = middle,
# 08 = secondary, 09 = higher secondary, and 10-13 = graduate and above in agriculture, engineering/technology, medicine or other subjects

EDUC_MAP <- data.frame(
  code  = 1:13,
  educ5 = c(rep("1 Not literate / no formal schooling", 4),
            rep("2 Below middle (below primary, primary)", 2),
            "3 Middle",
            rep("4 Secondary / higher secondary", 2),
            rep("5 Graduate and above", 4)),
  stringsAsFactors = FALSE)

# 11-16 mean not currently attending (never attended, discontinued or dropped out)
# 21-35 mean currently attending (NFEC/AEC/TLC, pre-primary, primary, middle, secondary/higher secondary, graduate or diploma courses)
ATTEND_NOW <- 21:35
EXPECTED_ATTEND <- c(11:16, 21:35)

# ----------------------------------------------------------------------------
# Codes from State/Region code files
# ----------------------------------------------------------------------------

STATES <- data.frame(
  state = 2:33,
  state_name = c("Andhra Pradesh","Arunachal Pradesh","Assam","Bihar","Goa",
                 "Gujarat","Haryana","Himachal Pradesh","Jammu & Kashmir","Karnataka",
                 "Kerala","Madhya Pradesh","Maharashtra","Manipur","Meghalaya","Mizoram",
                 "Nagaland","Orissa","Punjab","Rajasthan","Sikkim","Tamil Nadu","Tripura",
                 "Uttar Pradesh","West Bengal","A & N Islands","Chandigarh",
                 "Dadra & Nagar Haveli","Daman & Diu","Delhi","Lakshadweep","Pondicherry"),
  stringsAsFactors = FALSE)

# The 1999-2000 did not have Telangana and Jharkhand
# Chhattisgarh and Uttarakhand had separated 
# The NSS region code is taken as state * 10 + Region code from DDI

# 22 = Telangana, 51 = Jharkhand, 131 = Chhattisgarh and 251 = Uttarakhand

modern_state_approx <- function(state, region3) {
  nm <- STATES$state_name[match(state, STATES$state)]
  nm[region3 %in% 22]  <- "Telangana (approx.)"
  nm[region3 %in% 51]  <- "Jharkhand (approx.)"
  nm[region3 %in% 131] <- "Chhattisgarh (approx.)"
  nm[region3 %in% 251] <- "Uttarakhand (approx.)"
  nm
}

# Usual-status codes in Block 5.1, column 3, Schedule 10: 
# 11 = own-account worker, 12 = employer, 21 = unpaid helper, 31 = regular wage/salaried worker,
# 41 = casual worker in public works and 51 = other casual worker, 81 = sought/available for work,
# 91 = student, 92-93 = domestic duties, 94 = rentier/pensioner, 95 = disabled, 
# 96 = beggar etc. and 97 = other 
# Block 5.2 only has 11-51

WORKER_CODES <- c(11, 12, 21, 31, 41, 51)
UNEMP_CODE   <- 81
STUDENT_CODE <- 91
# Code 99 is not listed in Schedule 10 but present in the data
# later, diagnostic 4.5c is used to verify it appears only for children under 5
EXPECTED_UPS <- c(WORKER_CODES, UNEMP_CODE, 91:97, 99)

# From DDI file
DDI_REF <- list(n_hh = 120578, mean_ss_rep = 2.7372157, mean_wgt = 12480.4751,
                mean_wgt_comb = 6250.1059)

## ========================= More Helper Functions ========================= 

to_num   <- function(x) suppressWarnings(as.numeric(trimws(as.character(x))))
norm_key <- function(x) {
  x <- trimws(as.character(x)); n <- to_num(x)
  ifelse(is.na(n), x, sprintf("%.0f", n))
}
clean_block <- function(x) {
  x <- x %>% mutate(across(everything(), ~ trimws(as.character(.x))))
  keys <- intersect(c(KEY_HH, "pid"), names(x))
  x %>% mutate(across(all_of(keys), norm_key))
}
LAYOUT <- if (file.exists(CFG$layout_csv))
  read_csv(CFG$layout_csv, show_col_types = FALSE) else NULL

var_labels <- function(x) vapply(x, function(v) {
  l <- attr(v, "label", exact = TRUE); if (is.null(l)) "" else as.character(l) }, "")
show_vars <- function(spec) {
  path <- file.path(CFG$data_dir, spec$file)
  if (grepl("\\.(txt|dat)$", path, ignore.case = TRUE))
    return(as.data.frame(LAYOUT[LAYOUT$file_title == spec$title,
                                c("pos", "name", "label", "start", "end")]))
  x <- if (grepl("\\.dta$", path, ignore.case = TRUE)) read_dta(path, n_max = 5)
  else read_csv(path, n_max = 5, col_types = cols(.default = "c"), progress = FALSE)
  data.frame(pos = seq_along(x), name = names(x), label = var_labels(x))
}

read_block <- function(spec, label) {
  path <- file.path(CFG$data_dir, spec$file)
  if (!file.exists(path)) stop("Missing file for ", label, ": ", path)
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("txt", "dat")) {
    if (is.null(LAYOUT)) stop(label, ": fixed-width needs ", CFG$layout_csv)
    lay <- LAYOUT[LAYOUT$file_title == spec$title, ]
    idx <- match(tolower(spec$cols), tolower(lay$name))
    if (anyNA(idx)) stop(label, ": not in layout: ", paste(spec$cols[is.na(idx)], collapse = ", "))
    x <- read_fwf(path, fwf_positions(start = lay$start[idx], end = lay$end[idx],
                                      col_names = names(spec$cols)),
                  col_types = cols(.default = "c"), progress = FALSE, trim_ws = TRUE)
    if (CFG$fwf_implied_decimals)
      for (nm in intersect(c("mult", "mult_comb"), names(x)))
        x[[nm]] <- as.character(to_num(x[[nm]]) / 100)
    return(clean_block(x))
  }
  x <- switch(ext,
              dta = zap_labels(read_dta(path)),
              csv = read_csv(path, col_types = cols(.default = "c"), progress = FALSE),
              stop(label, ": unsupported extension .", ext))
  idx <- match(tolower(spec$cols), tolower(names(x)))
  if (anyNA(idx))
    stop(label, ": columns not found in ", spec$file, ": ",
         paste(spec$cols[is.na(idx)], collapse = ", "),
         "\n  Available: ", paste(names(x), collapse = ", "))
  x <- x[, idx]; names(x) <- names(spec$cols)
  clean_block(x)
}

# Checks repeated person keys and removes identical entries
dedupe_block <- function(x, keys, label) {
  blank_mat <- as.matrix(sapply(x[keys], function(v) is.na(v) | v == ""))
  blank <- rowSums(blank_mat) > 0
  if (any(blank)) {
    warning(label, ": ", sum(blank), " rows have a blank key field - check the column map",
            call. = FALSE)
    cat("  blank rows per key column (the column with a count is the problem):\n")
    print(colSums(blank_mat))
    cat("  first blank rows:\n")
    print(as.data.frame(head(x[blank, ], 6)), row.names = FALSE)
    cat("  blank rows by state:\n"); print(table(state = x$state[blank]))
    if (CFG$blank_key_policy == "drop") {
      message(label, ": dropping ", sum(blank), " rows with a blank key (blank_key_policy = drop)")
      x <- x[!blank, ]
    } else stop(label, ": rows with blank key fields (see counts above). Fix the column map, or set ",
                "CFG$blank_key_policy = \"drop\" to set them aside.", call. = FALSE)
  }
  dup_any <- duplicated(x[keys]) | duplicated(x[keys], fromLast = TRUE)
  if (!any(dup_any)) return(x)
  n_extra <- sum(duplicated(x[keys]))
  d       <- x[dup_any, ]
  nonkey  <- setdiff(names(x), keys)
  d$.kk   <- do.call(paste, c(d[keys], sep = "|"))
  d$.sig  <- do.call(paste, c(d[nonkey], sep = "|"))
  nsig    <- tapply(d$.sig, d$.kk, function(z) length(unique(z)))
  n_conf  <- sum(nsig > 1)
  message(label, ": ", n_extra, " repeated rows on the person key (",
          length(nsig), " keys); ", length(nsig) - n_conf, " keys are identical copies, ",
          n_conf, " keys have copies that DISAGREE on ", paste(nonkey, collapse = "/"))
  show <- if (n_conf > 0) d[d$.kk %in% names(nsig)[nsig > 1], ] else d
  cat(if (n_conf > 0) "  examples of CONFLICTING repeated keys:\n" else "  examples of repeated keys:\n")
  print(as.data.frame(head(show[order(show$.kk), c(keys, nonkey)], 8)), row.names = FALSE)
  if (n_conf > 0) {
    if (CFG$dup_policy == "stop")
      stop(label, ": conflicting duplicate records (", n_conf, " keys). Inspect the examples ",
           "above; if a key column is blank/constant in this file the column map is wrong; ",
           "otherwise set CFG$dup_policy = \"keep_first\" to keep the first row of each key.",
           call. = FALSE)
    warning(label, ": ", n_conf, " conflicting keys - kept first row (dup_policy = keep_first)")
  }
  x[!duplicated(x[keys]), ]
}

# Check with DDI stats
ddi_check_block3 <- function(hh) {
  cat("\n---- 4.0 Block 3 vs DDI summary statistics ----\n")
  m <- function(x) mean(to_num(x), na.rm = TRUE)
  chk <- function(label, got, ref) cat(sprintf("  %-28s got %12s  DDI %12s  %s\n", label, sf5(got), sf5(ref),
                                               if (abs(got / ref - 1) < 1e-3) "OK" else if (abs(got / ref - 100) < 0.5)
                                                 "-> about 100x TOO LARGE: set fwf_implied_decimals = TRUE is wrong, or divide by 100"
                                               else if (abs(got * 100 / ref - 1) < 0.01) "-> 100x too small: set fwf_implied_decimals = FALSE"
                                               else "MISMATCH"))
  chk("households", nrow(hh), DDI_REF$n_hh)
  if ("ss_rep"    %in% names(hh)) chk("mean ss_replicate", m(hh$ss_rep), DDI_REF$mean_ss_rep)
  if ("mult"      %in% names(hh)) chk("mean Wgt",           m(hh$mult), DDI_REF$mean_wgt)
  if ("mult_comb" %in% names(hh)) chk("mean Wgt_SR_comb",   m(hh$mult_comb), DDI_REF$mean_wgt_comb)
  if ("survey_code" %in% names(hh)) {
    cat("  survey code (1 original, 2 substitute, 3 casualty):\n"); print(table(hh$survey_code))
  }
  if ("mult_comb" %in% names(hh))
    cat(sprintf("  weighted households (sum Wgt_SR_comb/4): %s million (India 1999-00 ~ 190 m)\n",
                sf5(sum(to_num(hh$mult_comb), na.rm = TRUE) / 4 / 1e6)))
}

build_persons <- function(B, schedule) {
  message("Reading schedule ", schedule, " ...")
  b4  <- read_block(B$b4,  "Block 4")
  b51 <- read_block(B$b51, "Block 5.1")
  b52 <- read_block(B$b52, "Block 5.2")
  hh  <- if (!is.null(B$hh)) read_block(B$hh, "Block 3") else NULL
  pk  <- c(KEY_HH, "pid")
  b4  <- dedupe_block(b4,  pk, "Block 4")
  b51 <- dedupe_block(b51, pk, "Block 5.1")
  if (!is.null(hh)) {
    ddi_check_block3(hh)
    hh <- distinct(hh, across(all_of(KEY_HH)), .keep_all = TRUE)
  }
  
  # Block 5.2 has multiple subsidiary-activity records for one person, so they are combined
  b52p <- b52 %>% mutate(uss_n = to_num(uss)) %>%
    group_by(across(all_of(pk))) %>%
    summarise(uss = first(uss), n_sub_rows = n(),
              sub_worker = any(uss_n %in% WORKER_CODES), .groups = "drop")
  
  hhs <- count(b4, across(all_of(KEY_HH)), name = "hh_size")
  p <- b4 %>%
    left_join(hhs, by = KEY_HH) %>%
    left_join(select(b51, all_of(pk), ups, sub_flag), by = pk) %>%
    left_join(b52p, by = pk)
  if (!is.null(hh))
    p <- left_join(p, select(hh, all_of(KEY_HH),
                             any_of(setdiff(names(hh), c(KEY_HH, names(p))))),
                   by = KEY_HH)
  p <- p %>%
    mutate(hh_key = do.call(paste, c(across(all_of(KEY_HH)), sep = "|")),
           schedule = schedule,
           sub_worker = coalesce(sub_worker, FALSE)) %>%
    mutate(across(any_of(c("sector","state","subround","subsample","visit","seg",
                           "sss","region","stratum","district","age","sex","marital",
                           "educ_raw","attend","tech","rel","ups","uss","sub_flag",
                           "nss","ss_rep","mult","mult_comb","n_sub_rows",
                           "soc_group","religion","hh_type","hh_exp_month","survey_code",
                           "hh_size")), to_num))
  message("  persons: ", nrow(p), " | no 5.1 status (age>=5): ",
          sum(is.na(p$ups) & p$age >= 5, na.rm = TRUE),
          " | no multiplier: ", sum(is.na(p$mult)))
  p
}

## ----------------------------------------------------------------------------
## Builds person-level file and calculates weights
## ----------------------------------------------------------------------------

dir.create(CFG$out_dir, showWarnings = FALSE, recursive = TRUE)
if (CFG$inspect_only) {
  for (B in list(BLOCKS_10, if (CFG$use_combined) BLOCKS_COMB))
    for (nm in names(B)) if (!is.null(B[[nm]])) {
      cat("\n####", B[[nm]]$file, "\n"); print(show_vars(B[[nm]]), row.names = FALSE)
    }
  if (!interactive()) quit(save = "no")
  stop("inspect_only = TRUE", call. = FALSE)
}
persons <- if (CFG$use_combined) build_persons(BLOCKS_COMB, "10+10.1") else
  build_persons(BLOCKS_10, "10")

if (!CFG$use_combined && CFG$drop_sss9) {
  n9 <- sum(persons$sss == 9, na.rm = TRUE)
  message("drop_sss9 = TRUE: dropping ", n9, " persons in 2nd-stage stratum 9")
  persons <- filter(persons, sss != 9 | is.na(sss))
}

# Multiplier instructions from How_to_Use_MULTIPLIERS_for_NSS_55_SCHEDULE_10_and_10pt1.pdf
# For Schedule 10 - posted multiplier is used if ss_replicate == 1
# if ss_replicate > 1, posted multiplier divided by ss_replicate
# For combined Schedule 10 and 10.1 - divide by ss_replicate in second-stage strata 1 and 2 then divide by 4
# Wgt_SR_comb is half of Wgt in Block 3

persons$w_sub_computed <- if (!CFG$use_combined)
  ifelse(persons$ss_rep == 1, persons$mult, persons$mult / 2) else
    ifelse(persons$sss == 9, persons$mult, persons$mult / persons$ss_rep)
persons$w_sub <- if (CFG$weight_rule == "provided") persons$mult_comb else
  persons$w_sub_computed
persons$w_final <- persons$w_sub / 4
if (anyNA(persons$w_final))
  warning(sum(is.na(persons$w_final)), " persons have no weight")

# ----------------------------------------------------------------------------
# Diagnostics
# ----------------------------------------------------------------------------

cat("\n==== DIAGNOSTICS ====\n")
tot <- sum(persons$w_final, na.rm = TRUE)
cat(sprintf("4.1 Weighted persons (all ages): %s million (NSS frame-based totals usually fall ~5-10%% short of demographic projections of ~1,000 m; LFPR is a ratio and unaffected)\n",
            sf5(tot / 1e6)))
cat(sprintf("    by sector (million): rural %s, urban %s   [NSS Report 455 1.1.2: survey estimates 687 rural + 234 urban = 921 million]\n",
            sf5(sum(persons$w_final[persons$sector %in% 1], na.rm = TRUE) / 1e6),
            sf5(sum(persons$w_final[persons$sector %in% 2], na.rm = TRUE) / 1e6)))
cat("4.2 Weighted persons by sub-round (should be similar):\n")
print5(persons %>% group_by(subround) %>%
         summarise(pop_m = sum(w_final, na.rm = TRUE) * 4 / 1e6, n = n(), .groups = "drop"), "pop_m")
cat("4.3 Distinct FSUs by sector (central-sample design: 6,208 rural, 4,176 urban)\n")
print(persons %>% distinct(sector, state, region, stratum, subsample, fsu) %>% count(sector))
if (!CFG$use_combined)
  cat(sprintf("4.3b Households %d / persons %d (DDI: 120,578 / 596,686 before any drop)\n",
              n_distinct(persons$hh_key), nrow(persons)))
cat("4.4 2nd-stage stratum x visit:\n"); print(table(sss = persons$sss, visit = persons$visit, useNA = "ifany"))
cat("4.5 Usual principal status codes outside the expected list:\n")
bad <- setdiff(unique(persons$ups[!is.na(persons$ups)]), EXPECTED_UPS)
if (length(bad)) print(table(persons$ups[persons$ups %in% bad])) else cat("    none\n")
cat("4.5b Education / attendance codes outside the Schedule 10 lists:\n")
bad_e <- setdiff(unique(persons$educ_raw[!is.na(persons$educ_raw)]), 1:13)
bad_a <- setdiff(unique(persons$attend[!is.na(persons$attend)]), EXPECTED_ATTEND)
if (length(bad_e) || length(bad_a))
  print(list(education = bad_e, attendance = bad_a)) else cat("    none\n")
cat("4.5c Status code 99 by age group (expected: only ages 0-4):\n")
print(table(age_grp = cut(persons$age, c(-1, 4, 14, 20, 120), labels = c("0-4", "5-14", "15-20", "21+")),
            ups_is_99 = persons$ups == 99, useNA = "ifany"))
cat("4.5d Persons age >= 5 with no principal status, by state (should be none):\n")
print(table(state = persons$state[is.na(persons$ups) & persons$age >= 5]))
cat("4.6 Sex and sector codes found:\n")
print(table(persons$sex, useNA = "ifany")); print(table(persons$sector, useNA = "ifany"))
if ("mult_comb" %in% names(persons)) {
  r <- abs(persons$w_sub_computed / persons$mult_comb - 1)
  cat(sprintf("4.7 Computed vs file's generated multiplier: max rel. diff %.4e; share >0.1%%: %s\n",
              max(r, na.rm = TRUE), sf5(mean(r > 1e-3, na.rm = TRUE))))
}

# ----------------------------------------------------------------------------
# Harmonized variables
# ----------------------------------------------------------------------------

for (nm in c("soc_group", "religion", "hh_exp_month"))
  if (!nm %in% names(persons)) persons[[nm]] <- NA_real_   # these will not be available if use_combined = TRUE as Block 3 is not included
persons <- persons %>%
  mutate(
    round        = 55L,
    interview_year = if (CFG$cohort_rule == "interview_year" && all(!is.na(subround)))
      ifelse(subround %in% 1:2, 1999L, 2000L) else 1999L,
    birth_year_hat = interview_year - age,
    cohort       = ifelse(birth_year_hat %in% CFG$cohort_years,
                          paste0(min(CFG$cohort_years), "-",
                                 substr(max(CFG$cohort_years), 3, 4)), NA_character_),
    female       = sex == CFG$sex_female,
    sector_lab   = unname(CFG$sector_codes[as.character(sector)]),
    state_name   = STATES$state_name[match(state, STATES$state)],
    region3      = state * 10 + region,
    state_modern_approx = modern_state_approx(state, region3),
    # UPS is principal activity only
    # UPSS is both principal and subsidiary activity
    # missing status becomes NA
    worker_ups   = ifelse(is.na(ups), NA, ups %in% WORKER_CODES),
    lf_ups       = ifelse(is.na(ups), NA, ups %in% c(WORKER_CODES, UNEMP_CODE)),
    worker_upss  = ifelse(sub_worker, TRUE, worker_ups),
    lf_upss      = ifelse(sub_worker, TRUE, lf_ups),
    student_ups  = ups == STUDENT_CODE,                 
    # principal usual status code 91 (student)
    # Attendance in column 9 - 0 value or blank is taken as NA
    student_attend = ifelse(attend %in% EXPECTED_ATTEND, attend %in% ATTEND_NOW, NA),
    social_group = c(`1` = "ST", `2` = "SC", `3` = "OBC", `9` = "Others")[as.character(soc_group)],
    religion_lab = c(`1` = "Hindu", `2` = "Muslim", `3` = "Christian", `4` = "Sikh",
                     `5` = "Jain", `6` = "Buddhist", `7` = "Zoroastrian",
                     `9` = "Others")[as.character(religion)],
    mpce_calc    = if ("hh_exp_month" %in% names(.)) hh_exp_month / hh_size else NA_real_,
    across(c(worker_ups, lf_ups, sub_worker, worker_upss, lf_upss), as.integer),
    psu_id   = paste(sector, state, region, stratum, subsample, fsu, sep = "|"),
    strat_id = paste(state, sector, region, stratum, sep = "|"))

if (CFG$educ_map_verified && nrow(EDUC_MAP) > 0) {
  persons$educ5 <- EDUC_MAP$educ5[match(persons$educ_raw, EDUC_MAP$code)]
  unm <- unique(persons$educ_raw[is.na(persons$educ5) & !is.na(persons$educ_raw)])
  if (length(unm)) warning("Unmapped education codes: ", paste(sort(unm), collapse = ","))
  persons$educ5[is.na(persons$educ5)] <- "Missing"
} else {
  persons$educ5 <- NA_character_
  message("Education bins skipped (fill EDUC_MAP and set educ_map_verified = TRUE)")
}

saveRDS(persons, file.path(CFG$out_dir, "nss55_persons_all.rds"))
cohort1 <- filter(persons, female, !is.na(cohort))
write_csv(select(cohort1, round, schedule, hh_key, pid, state, state_name,
                 state_modern_approx, sector, sector_lab, region3, stratum, fsu,
                 subround, subsample, visit, sss, age, interview_year,
                 birth_year_hat, cohort, marital, educ_raw, educ5, attend, ups,
                 uss, sub_worker, lf_ups, lf_upss, worker_upss, student_ups,
                 student_attend, any_of(c("social_group", "religion_lab",
                                          "hh_type", "hh_size", "mpce_calc")), w_final, psu_id, strat_id),
          file.path(CFG$out_dir, "nss55_cohort1.csv"))
cat("\nCohort 1 (women born ", min(CFG$cohort_years), "-", max(CFG$cohort_years),
    "): ", nrow(cohort1), " records; age range ",
    paste(range(cohort1$age), collapse = "-"), "\n", sep = "")

# ----------------------------------------------------------------------------
# LFPR tables - Builds survey design on all people before taking subsets to get accurate SE
# ----------------------------------------------------------------------------

des_all <- svydesign(ids = ~psu_id, strata = ~strat_id, weights = ~w_final,
                     data = filter(persons, !is.na(w_final)), nest = TRUE)
des_c   <- subset(des_all, female & !is.na(cohort))

lfpr <- function(des, by, outcome, label) {
  if (is.null(by)) {
    r <- svymean(as.formula(paste("~", outcome)), des, na.rm = TRUE)
    out <- data.frame(group = "All", est = as.numeric(r), se = as.numeric(SE(r)),
                      n = as.numeric(unwtd.count(as.formula(paste("~", outcome)), des, na.rm = TRUE)[1]))
  } else {
    r <- svyby(as.formula(paste("~", outcome)), as.formula(paste("~", by)),
               des, svymean, na.rm = TRUE, na.rm.by = TRUE, vartype = "se")   # omit groups with missing values
    rn <- svyby(as.formula(paste("~", outcome)), as.formula(paste("~", by)),
                des, unwtd.count, na.rm = TRUE, na.rm.by = TRUE)   # counts the women by group
    out <- setNames(r[, 1:3], c("group", "est", "se"))
    out$n <- rn[[2]]
    out$group <- as.character(out$group)
  }
  out$lo <- pmax(0, out$est - 1.96 * out$se); out$hi <- pmin(1, out$est + 1.96 * out$se)
  out$measure <- label
  out
}
nstud <- subset(des_c, !student_attend)   # keeps women who are not currently attending
tabs <- bind_rows(
  lfpr(des_c, NULL, "lf_upss", "UPSS, all"),
  lfpr(des_c, "sector_lab", "lf_upss", "UPSS, all"),
  lfpr(des_c, NULL, "lf_ups",  "UPS, all"),
  lfpr(des_c, "sector_lab", "lf_ups", "UPS, all"),
  lfpr(nstud, NULL, "lf_upss", "UPSS, not attending school/college"),
  lfpr(nstud, "sector_lab", "lf_upss", "UPSS, not attending school/college"),
  lfpr(des_c, "age", "lf_upss", "UPSS by single year of age"),
  lfpr(des_c, "student_attend", "lf_upss", "UPSS by currently attending (TRUE/FALSE)"))
if (CFG$educ_map_verified)
  tabs <- bind_rows(tabs, lfpr(nstud, "educ5", "lf_upss", "UPSS, not attending, by education"))
n_tab <- cohort1 %>% count(sector_lab, name = "n_unweighted")
tabs$cohort <- paste0(min(CFG$cohort_years), "-", substr(max(CFG$cohort_years), 3, 4))
tabs <- tabs[, c("cohort", setdiff(names(tabs), "cohort"))]
tabs$est_pct <- 100 * tabs$est; tabs$se_pct <- 100 * tabs$se
tabs$lo_pct  <- 100 * tabs$lo;  tabs$hi_pct <- 100 * tabs$hi
tabs[c("est", "se", "lo", "hi", "est_pct", "se_pct", "lo_pct", "hi_pct")] <-
  lapply(tabs[c("est", "se", "lo", "hi", "est_pct", "se_pct", "lo_pct", "hi_pct")], signif, 5)
write_csv(tabs,  file.path(CFG$out_dir, "nss55_cohort1_lfpr.csv"))
write_csv(n_tab, file.path(CFG$out_dir, "nss55_cohort1_cell_sizes.csv"))
cat("\n==== COHORT 1 LFPR (%) ====\n")
print5(tabs[, c("measure", "group", "est_pct", "se_pct", "lo_pct", "hi_pct")],
       c("est_pct", "se_pct", "lo_pct", "hi_pct"))
cat("\nUnweighted cohort records by sector:\n"); print(as.data.frame(n_tab))

# ----------------------------------------------------------------------------
# women aged 15+ calculated vs published - NSS Report 458
# ----------------------------------------------------------------------------

w15 <- subset(des_all, female & age >= 15)
recon <- bind_rows(lfpr(w15, "sector_lab", "lf_upss", "All women 15+, UPSS"),
                   lfpr(w15, "sector_lab", "lf_ups",  "All women 15+, UPS"))
recon$est_pct <- 100 * recon$est; recon$se_pct <- 100 * recon$se
recon$n <- NULL   # the unweighted count is included only in the cohort table
recon[c("est", "se", "lo", "hi", "est_pct", "se_pct")] <-
  lapply(recon[c("est", "se", "lo", "hi", "est_pct", "se_pct")], signif, 5)
write_csv(recon, file.path(CFG$out_dir, "nss55_recon_women15plus.csv"))
cat("\n==== RECONCILIATION: all women 15+ (%), compare with Report 458 ====\n")
print5(recon[, c("measure", "group", "est_pct", "se_pct")], c("est_pct", "se_pct"))

# ----------------------------------------------------------------------------
# Reconciliation using alternative age bases
# LFPR and WPR for UPS and UPSS, by sex and sector out of 100
# ----------------------------------------------------------------------------

recon_bases <- local({
  v <- des_all$variables
  ok <- !is.na(v$age)
  age_ok <- list(`all ages` = ok, `5+` = ok & v$age >= 5, `15+` = ok & v$age >= 15,
                 `15-59` = ok & v$age >= 15 & v$age <= 59)
  meas <- c(`LFPR UPSS` = "lf_upss", `LFPR UPS` = "lf_ups",
            `WPR UPSS` = "worker_upss", `WPR UPS` = "worker_ups")
  out <- list()
  for (bn in names(age_ok)) {
    keep <- age_ok[[bn]]
    db <- subset(des_all, keep)
    for (mn in names(meas)) {
      f <- as.formula(paste("~", meas[[mn]]))
      for (sx in c("female", "male")) {
        dd <- if (sx == "female") subset(db, female) else subset(db, !female)
        rs <- svyby(f, ~sector_lab, dd, svymean, na.rm = TRUE, vartype = "se")
        ra <- svymean(f, dd, na.rm = TRUE)
        out[[length(out) + 1]] <- data.frame(
          basis = bn, measure = mn, sex = sx,
          sector = c(as.character(rs$sector_lab), "All India"),
          est_pct = 100 * c(rs[[2]], as.numeric(ra)),
          se_pct  = 100 * c(rs[[3]], as.numeric(SE(ra))), stringsAsFactors = FALSE)
      }
    }
  }
  bind_rows(out)
})
recon_bases$est_pct <- signif(recon_bases$est_pct, 5); recon_bases$se_pct <- signif(recon_bases$se_pct, 5)
write_csv(recon_bases, file.path(CFG$out_dir, "nss55_recon_bases.csv"))
rb_wide <- reshape(recon_bases[, c("sex", "sector", "measure", "basis", "est_pct")],
                   idvar = c("sex", "sector", "measure"), timevar = "basis", direction = "wide")
names(rb_wide) <- sub("^est_pct\\.", "", names(rb_wide))
rb_wide <- rb_wide[order(rb_wide$measure, rb_wide$sex, match(rb_wide$sector, c("Rural", "Urban", "All India"))), ]
cat("\n==== RECONCILIATION ON ALTERNATIVE BASES (%; x10 = per 1,000) ====\n")
print5(rb_wide, c("all ages", "5+", "15+", "15-59"))

# ----------------------------------------------------------------------------
# Compares published LFPRs (per 1,000 people)
# Table 2 of NSS Report 455 and from Report 515 for the 55th round
# combined Schedule 10 + 10.1 sample, run with use_combined = TRUE
# Published rates are rounded to whole numbers or one decimal place
# ----------------------------------------------------------------------------

BAND_LAB <- c("5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
              "50-54", "55-59", "60 & above")
band_of <- function(a) cut(a, breaks = c(-Inf, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, Inf),
                           labels = c("0-4", BAND_LAB))
pub_vs_ours <- function(des, outcome, pub, label) {
  des <- update(des, band = band_of(age))
  f <- as.formula(paste("~", outcome)); rows <- list()
  for (sx in c("male", "female")) {
    dsx <- if (sx == "male") subset(des, !female) else subset(des, female)
    for (sc in c("Rural", "Urban")) {
      dd <- subset(dsx, sector_lab == sc)
      r  <- svyby(f, ~band, dd, svymean, na.rm = TRUE, vartype = "se")
      ra <- svymean(f, dd, na.rm = TRUE)
      rows[[length(rows) + 1]] <- data.frame(
        sex = sx, sector = sc, band = c(as.character(r$band), "all (0 +)"),
        ours = 1000 * c(r[[2]], as.numeric(ra)), se = 1000 * c(r[[3]], as.numeric(SE(ra))),
        stringsAsFactors = FALSE)
    }
  }
  o <- merge(bind_rows(rows), pub, by = c("sex", "sector", "band"))
  o$diff <- o$ours - o$published
  o$check <- ifelse(abs(o$diff) <= 1, "OK", "CHECK"); o$measure <- label
  o[order(o$sex, o$sector, match(o$band, c(BAND_LAB, "all (0 +)"))), ]
}
show_pub <- function(cmp, title) {
  cat("\n====", title, "====\n")
  cat(sprintf("  %d of %d cells within 1 per 1,000 of the published value; largest |difference| %s\n",
              sum(cmp$check == "OK"), nrow(cmp), sf5(max(abs(cmp$diff)))))
  print5(cmp[, c("sex", "sector", "band", "ours", "published", "diff", "check")], c("ours", "diff"))
}

PUB_55_PSSS <- data.frame(
  sex       = c("male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female", "male", "female"),
  sector    = c("Rural", "Rural", "Urban", "Urban", "Rural", "Rural", "Urban", "Urban", "Rural", "Rural", "Urban", "Urban", "Rural", "Rural", "Urban", "Urban", "Rural", "Rural", "Urban", "Urban", "Rural", "Rural", "Urban", "Urban", "Rural", "Rural", "Urban", "Urban", "Rural", "Rural", "Urban", "Urban", "Rural", "Rural", "Urban", "Urban", "Rural", "Rural", "Urban", "Urban", "Rural", "Rural", "Urban", "Urban", "Rural", "Rural", "Urban", "Urban", "Rural", "Rural", "Urban", "Urban"),
  band      = c("5-9", "5-9", "5-9", "5-9", "10-14", "10-14", "10-14", "10-14", "15-19", "15-19", "15-19", "15-19", "20-24", "20-24", "20-24", "20-24", "25-29", "25-29", "25-29", "25-29", "30-34", "30-34", "30-34", "30-34", "35-39", "35-39", "35-39", "35-39", "40-44", "40-44", "40-44", "40-44", "45-49", "45-49", "45-49", "45-49", "50-54", "50-54", "50-54", "50-54", "55-59", "55-59", "55-59", "55-59", "60 & above", "60 & above", "60 & above", "60 & above", "all (0 +)", "all (0 +)", "all (0 +)", "all (0 +)"),
  published = c(7, 7, 3, 2, 93, 96, 52, 37, 532, 314, 366, 121, 889, 425, 755, 191, 975, 498, 951, 214, 987, 557, 980, 245, 986, 579, 986, 289, 984, 586, 980, 285, 980, 566, 974, 269, 953, 515, 939, 264, 930, 450, 811, 208, 640, 218, 402, 94, 540, 302, 542, 147),
  stringsAsFactors = FALSE)

cmp_pub55 <- pub_vs_ours(des_all, "lf_upss", PUB_55_PSSS, "UPSS (ps+ss), 55th")
write_csv(cmp_pub55, file.path(CFG$out_dir, "nss55_vs_published_age_specific.csv"))
show_pub(cmp_pub55, paste0("UPSS (ps+ss) LFPR per 1,000 vs NSS 55th report (this run: use_combined = ", CFG$use_combined, ")"))

# ----------------------------------------------------------------------------
# Results vs published figures for the 55th round
# Source: Sarkar (2008), ILO Asia-Pacific Working Paper,
# "Trends and patterns of labour supply and unemployment in India" 
# (Tables 2, 10, 12 and 14).
# Checks the weights, status codes and labour-force definition
# ----------------------------------------------------------------------------

wm <- function(x, w) sum(x * w, na.rm = TRUE)
wf <- persons %>% filter(female, !is.na(w_final), !is.na(ups))

cat("\n(a) Subsidiary share of the female UPSS labour force, all ages, %",
    "  [ILO Table 10: rural 22.3, urban 14.4]\n")
print5(wf %>% group_by(sector_lab) %>%
         summarise(sub_share_pct = 100 * (wm(lf_upss, w_final) - wm(lf_ups, w_final)) /
                     wm(lf_upss, w_final)), "sub_share_pct")

cat("\n(b) Female unemployment rate per 1,000 in the labour force",
    "\n    [ILO Table 14, 15-19 / 20-24 / 25-29: rural UPS 42/49/23, UPSS 31/36/15;",
    "urban UPS 156/228/118, UPSS 129/191/94]\n")
print5(wf %>% filter(age >= 15, age <= 29) %>%
         mutate(band = cut(age, c(14, 19, 24, 29), labels = c("15-19", "20-24", "25-29"))) %>%
         group_by(sector_lab, band) %>%
         summarise(UPS_per1000  = 1000 * wm(ups == 81, w_final) / wm(lf_ups, w_final),
                   UPSS_per1000 = 1000 * wm(ups == 81 & sub_worker == 0, w_final) /
                     wm(lf_upss, w_final), .groups = "drop"),
       c("UPS_per1000", "UPSS_per1000"))

cat("\n(c) Female UPSS LFPR by social group, all ages, %",
    "\n    [ILO Table 12: rural ST 44.0 SC 32.7 Others 27.0; urban ST 21.0 SC 19.1 Others 13.7]\n")
print5(wf %>%
         mutate(grp = case_when(soc_group == 1 ~ "ST", soc_group == 2 ~ "SC",
                                soc_group %in% c(3, 9) ~ "Others (incl. OBC)")) %>%
         filter(!is.na(grp)) %>% group_by(sector_lab, grp) %>%
         summarise(UPSS_LFPR_pct = 100 * wm(lf_upss, w_final) / sum(w_final),
                   .groups = "drop"), "UPSS_LFPR_pct")

cat(sprintf("\n(d) Weighted UPSS labour force, all persons: %s million   [ILO Table 2 'actual': 402.4 million.  NSS Report 455 (1.1.2): survey population estimates are lower than census projections and absolute numbers should come from applying the ratios to projected population - so a gap is expected; judge the weights by (a)-(c) and 7c]\n",
            sf5(sum(persons$w_final * persons$lf_upss, na.rm = TRUE) / 1e6)))
