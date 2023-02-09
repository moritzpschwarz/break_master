# Country groupings

cty_sample <- readRDS(here("data/out/country_sample.RDS"))

EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg", 
          "Netherlands", "Greece", "Portugal", "Sweden")

EU31 <- c("Austria", "Croatia", "Belgium", "Bulgaria", "Cyprus", 
          "Czech Republic", "Germany", "Denmark", "Spain", "Estonia", "Finland",
          "France", "United Kingdom", "Greece", "Hungary", "Ireland", "Italy",
          "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland",
          "Portugal", "Romania", "Slovak Republic", "Slovenia", "Sweden", 
          "Switzerland", "Iceland", "Norway")


# Developing vs Developed: 4 x AC6 & AC1
# Source: https://www.oecd-ilibrary.org/sites/f0773d55-en/1/4/3/index.html?itemId=/content/publication/f0773d55-en&_csp_=5026909c969925715cde6ea16f4854ee&itemIGO=oecd&itemContentType=book
AC6 <- read_excel(here("data/raw/country_groupings.xlsx"), sheet = 2)
AC1 <- read_excel(here("data/raw/country_groupings.xlsx")) %>% pull(Developed) %>% unlist
for(i in 1:nrow(AC6)){
  temp <- AC6 %>% slice(i) %>% pull(countries) %>% strsplit(., ", ") %>% unlist
  mv(from = "temp", to = paste0("AC6_", AC6$cat_abbrev[i]))
}
AC6_HICs <- c(AC6_HICs, "Russia")

# AC1 -> Eastern Europe vs. Rest
east_euro <- setdiff(EU31[!(EU31 %in% c("Switzerland", "Iceland", "Norway", "Cyprus", "Malta"))], EU15)
euro_main <- setdiff(EU31[EU31 != "Cyprus"], east_euro)

# OECD
OECD <- readRDS(here("data/out/oecd_countries.rds")) %>% 
  replace(. == "Korea", "South Korea")

# Emerging
emerging <- c("Argentina", "Brazil", "China", "Chile", "Costa Rica", "Colombia", 
              "India", "Indonesia", "Peru", "Russia", "Mexico", "South Africa", 
              "Turkey")

# Combine
samples <- mget(c("EU15", "EU31", "AC1", "AC6_HICs", "AC6_UMICs", "AC6_LMICs", "east_euro", "euro_main", "OECD", "emerging"))

#### Name standardisation
country_dict <- readRDS(here("data/out/countrydict.RDS"))
#saveRDS(country_dict, "data/out/countrydict.RDS")

getcountry <- country_dict$country.name.en
names(getcountry) <- country_dict$country.name.alt

get_country <- function(c_name){
  if(!(any(country_dict == c_name))){
    return(paste(c_name, "not in country_dict. Please add."))}
  else if(c_name %in% names(getcountry)){
    c_new <- unname(getcountry[c_name])
    return(c_new)
  }else{return(c_name)}
  }


# Checks completeness of data frames in cleaning/combining master_cleaning.Rmd
test_complete <- function(df){
  stopifnot(
    # Balanced
    is.pbalanced(df),
    # 51 distinct countries
    n_distinct(df$country) == 51,
    # All cty_sample countries represented
    length(cty_sample[!(cty_sample %in% df$country)]) == 0,
    # At least 20 observations per country
    df %>% count(country) %>% filter(n < 20) %>% nrow(.) == 0,
    # 51 country observations per year
    df %>% count(year) %>% filter(n != 51) %>% nrow(.) == 0)
  print("Passed.")
  
  # Complete cases check
  tmp <- df %>% filter(year >= 2000 & !complete.cases(.))
  if(nrow(tmp) == 0){"No missing cases."}else{
    print("Missing cases:")
          print(tmp)
          }
  
}
