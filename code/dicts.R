EU15 <- c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
          "France", "United Kingdom", "Ireland", "Italy", "Luxembourg", 
          "Netherlands", "Greece", "Portugal", "Sweden")

EU31 <- c("Austria", "Croatia", "Belgium", "Bulgaria", "Cyprus", 
          "Czech Republic", "Germany", "Denmark", "Spain", "Estonia", "Finland",
          "France", "United Kingdom", "Greece", "Hungary", "Ireland", "Italy",
          "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland",
          "Portugal", "Romania", "Slovak Republic", "Slovenia", "Sweden", 
          "Switzerland", "Iceland", "Norway")

OECD <- readRDS(here("data/out/oecd_countries.rds"))

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

test_diff <- function(df1 = data.frame, df2 = data.frame, vars = list){
  for(i in 1:length(vars)){
    a <- df1 %>% pull(vars[i])
    b <- df2 %>% pull(vars[i])
    if(length(setdiff(a,b) != 0)) {
      print(paste(deparse(substitute(df1)),"has extra:",setdiff(a,b)))
    }else{print(paste(vars[i],"match"))}
    if(length(setdiff(b,a) != 0)) {
      print(paste(deparse(substitute(df2)),"has extra:",setdiff(b,a)))
    }else{print(paste(vars[i],"match"))}
  }
}

# test_complete <- function(df){
#   is.pbalanced(df) %>% print
#   as.logical(n_distinct(df$country) == 51)
#   df %>% count(country) %>% filter(n != 52) %>% nrow(.) == 0
#   df %>% count(year) %>% filter(n != 51) %>% nrow(.) == 0
#   cty_sample[!(cty_sample %in% df$country)] %>% length == 0
# }

# Heat map generation
gen_country <- function(df){
  country_p <- df %>% 
    pivot_longer(cols = !c(country, year), values_drop_na = TRUE) %>% 
    filter(value > 0)  %>% 
    group_by(name, year) %>% 
    summarise(obs = n(), n_OECD = sum(country %in% union(EU31, OECD))) %>% 
    ggplot(., aes(x = year)) + 
    geom_col(aes(y = obs, fill = "blue")) +
    geom_col(aes(y = n_OECD, fill = "green")) +
    geom_hline(yintercept = 43, linetype = "dashed") +
    facet_wrap(~name) +
    theme(legend.position = "none")
  return(country_p)
}

gen_year <- function(df){
  year_p <- df %>% 
    pivot_longer(cols = !c(country, year), values_drop_na = TRUE) %>% 
    filter(value > 0)  %>% 
    group_by(name) %>% 
    summarise(start = as.numeric(min(year)), end = as.numeric(max(year))) %>% 
    ggplot() +
    aes(y = name) +
    geom_segment(aes(x = start, 
                     xend = end,
                     yend = name,
                     color = name), 
                 size = 5,
                 show.legend = FALSE) +
    geom_text(aes(x = start, label = start), 
              nudge_x = 0,
              size = 3) +
    geom_text(aes(x = end, label = end), 
              nudge_x = 0,
              size = 3)
  return(year_p)
}


# Functions for results overview
get_model <- function(ct_sample, yr_range, p.val, l_iis, b_sz){
  m <- models %>% filter(country_sample == ct_sample & 
                           year_range == yr_range & 
                           p_val == p.val & 
                           iis == l_iis & 
                           b_size == b_sz)
  mod <- m %>% pull(is) %>% first()
  p <- mod %>% plot(zero_line = FALSE)
  p_grid <- mod %>% plot_grid
  mt <- m$main_title
  st <- m$sub_title
  return(list(results = mod, plot = p, plot_grid = p_grid, main_title = mt, sub_title = st))
}