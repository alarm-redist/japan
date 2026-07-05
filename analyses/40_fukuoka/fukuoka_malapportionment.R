library(redist)
library(geomander)
library(tidyverse)
library(gt)
library(extrafontdb)
library(Rttf2pt1)

pref_code <- 40
pref_name <- "fukuoka"

load(here::here(paste("data-out/pref/",
                      as.character(pref_code),
                      "_",
                      as.character(pref_name),
                      "_data",
                      ".Rdata",
                      sep = "")))

optimal_max_to_min <- results_sample[which(results_sample$index == optimal),]$max_to_min

optimal_matrix <- redist::get_plans_matrix(sim_smc_pref_sample %>% filter(draw == optimal))
colnames(optimal_matrix) <- "district"
optimal_plan <- pref_map %>%
  mutate(district = as.integer(optimal_matrix[, "district"]))

# --- Add Japanese place names for `code` and `sub_code` -----------------------
# Pull 市区町村名 (X9) and 小地域名 (X10) from the 2020 Census
# (same source/URL as `download_pop_2020()`)
census_file <- tempfile(fileext = ".csv")
download.file(
  paste("https://www.e-stat.go.jp/stat-search/file-download?statInfId=00003216",
        as.character(3094 + 15 * (pref_code - 1)), "&fileKind=1", sep = ""),
  census_file)
census_2020 <- read_csv(census_file, locale = locale(encoding = "cp932"),
                        col_names = FALSE, skip = 5, show_col_types = FALSE)

# Municipality names keyed by `code`
mun_names <- census_2020 %>%
  transmute(code = as.numeric(X2), mun_name = X9) %>%
  distinct(code, .keep_all = TRUE) %>%
  # merged 郡 (gun) units
  bind_rows(tibble::tribble(
    ~code,  ~mun_name,
    40340, "糟屋郡", 40380, "遠賀郡", 40400, "鞍手郡", 40420, "嘉穂郡",
    40440, "朝倉郡", 40500, "三井郡", 40520, "三潴郡", 40540, "八女郡",
    40600, "田川郡", 40620, "京都郡", 40640, "築上郡"))

# Sub-area (小地域) names keyed by `code` + `sub_code`
sub_names <- census_2020 %>%
  filter(!(as.numeric(X4) %in% c(1, 4))) %>%   # keep 小地域 level, as in the prep
  transmute(code = as.numeric(X2),
            sub_code = as.numeric(X3),
            sub_name = X10) %>%
  distinct(code, sub_code, .keep_all = TRUE)

optimal_plan <- optimal_plan %>%
  left_join(mun_names, by = "code") %>%
  left_join(sub_names, by = c("code", "sub_code"))


# Save the optimal plan (a redist_map with the added `district` column)
saveRDS(
  optimal_plan,
  here::here(paste("data-out/pref/",
                   as.character(pref_code),
                   "_",
                   as.character(pref_name),
                   "_optimal_plan",
                   ".Rds",
                   sep = "")))


csv <- optimal_plan %>%
  sf::st_drop_geometry() %>%
  as.tibble() %>%
  select(-adj) %>%
  group_by(code, mun_name, district) %>%
  summarise(pop = sum(pop)) %>%
  # Fix manually for Fukuoka Minami ku
  mutate(mun_name = case_when(
    mun_name == "福岡市南区" & pop == 217000 ~ "福岡市南区（旧2区）",
    mun_name == "福岡市南区" & pop == 43154 ~ "福岡市南区（旧5区）",
    .default = mun_name)
  )

write_csv(
  csv,
  here::here(paste("data-out/pref/",
                   as.character(pref_code),
                   "_",
                   as.character(pref_name),
                   "_optimal_plan",
                   ".csv",
                   sep = "")))



