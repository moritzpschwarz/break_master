# Plotting functions

f <- function(k) {
  step <- k
  function(y) seq(floor(min(y)), ceiling(max(y)), by = step)
}

# Arranges result plots (plot and plot_grid) and prints model results
# Option to suppress model results using results = FALSE is required for automatic tabsetting.
gen_p <- function(df, results = TRUE, auto = FALSE){
  if(nrow(df) == 0){return("Empty data frame. Model likely not run.")}else{
    for(i in 1:nrow(df)){
      mt <- paste0(df$country_sample[i], " (", df$year_range[i],")", "; p.value: ",df$p_val[i],  "; AR: ",df$ar[i])
      st <- paste0("Formula: ", df$source[i])
      
      res <- df %>% slice(i) %>% pull(is) %>% first
      
      pl <- res %>% 
        plot(zero_line = FALSE) +
        ggtitle(label = mt, subtitle = st) +
        scale_x_continuous(breaks = f(10))
      
      pg <- res %>%
        plot_grid() +
        ggtitle(label = mt, subtitle = st)
      grid.arrange(pl, pg, ncol = 2)
      
      if(results == TRUE & auto == FALSE){
        print(st)
        print(mt)
        print(res)
      }else if(results == TRUE & auto == TRUE){
        p <- invisible(capture.output(res$isatpanel.result))
        # Spacing included to trick knitr into reading as verbatim code.
        cat(c("                         \n", 
              "                         \n", 
              paste("                  ", st,'     \n'),
              paste("                  ", mt,'     \n'),
              paste("                  ", p,'     \n')))
      }
    }
  }
}

# Data manipulation function: extracts plot_grid input data from getspanel
# mod = df of models as created above. Minimum requirement is 2 columns (is =  isatpanel object, model = model name)
# na.rm removes countries for which NO model reveals a break/effect
convert <- function(mod){
  if(nrow(mod) == 0){
    print("No models to plot.")
  }else{
    c_mods <- tibble()
    for(m in 1:nrow(mod)){
      mod_name <- mod %>% slice(m) %>% pull(model)
      # Currently, this extracts the data used to build the plot_grid in isatpanel; not ideal
      grid_dat <- mod %>% slice(m) %>% pull(is) %>% first %>% plot_grid %>% ggplot_build
      grid_dat <- grid_dat$plot$data
      grid_dat$model <- mod_name
      c_mods <- rbind(c_mods, grid_dat)
    }
  }
  return(c_mods)
}

plot_comp <- function(mod, panel = "country", na.rm = TRUE, sign = NULL){
  tmp <- convert(mod)
  
  if(panel == "model"){
    tmp <- tmp %>% rename(id = model, model = id)
  }else if(!(panel == "country")){
    print("Error")
    break}else{}
  
  if(na.rm == TRUE){
    tmp <- tmp %>% group_by(id) %>% filter(!all(is.na(effect)))
  }
  if(sign == "pos"){
    tmp <- tmp %>% group_by(id) %>% filter(any(effect > 0))
    
  }else if(sign == "neg") { tmp <- tmp %>% group_by(id) %>% filter(any(effect < 0))}
  
  p <- tmp %>% ggplot(aes(x = time, y = model)) +
    geom_tile(aes(fill = effect), na.rm = NA) +
    scale_fill_gradient2(na.value = NA, name = "Effect")+
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0), limits = rev) +
    facet_grid(id~.) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(fill = NA),
          strip.background = element_blank(),
          axis.text = element_text(size = 12, color = "black"),
          strip.text.y = element_text(size = 14, angle = 0)) +
    labs(x = NULL, y = NULL,title= "Model Overview")
  
  print(p)
}

# Similar function as convert but organises w/ respect to multiple outcome variables
convert_mult <- function(mod){
  if(nrow(mod) == 0){
    print("No models to plot.")
  }else{
    c_mods <- tibble()
    for(m in 1:nrow(mod)){
      mod_name <- mod %>% slice(m) %>% pull(model)
      dep_name <- mod %>% slice(m) %>% pull(dep)
      # Currently, this extracts the data used to build the plot_grid in isatpanel
      grid_dat <- mod %>% slice(m) %>% pull(is) %>% first %>% plot_grid %>% ggplot_build
      grid_dat <- grid_dat$plot$data
      grid_dat$model <- mod_name
      grid_dat$dep <- dep_name
      c_mods <- rbind(c_mods, grid_dat)
    }
  }
  return(c_mods)
}

# Pass converted data frame (convert_mult)

plot_country <- function(cmod, country, bs = list(), na.rm = FALSE){
  cmod <- cmod %>% filter(id == country)
  
  if(na.rm == TRUE){
    cmod <- cmod %>% group_by(dep) %>% filter(!all(is.na(effect)))
  }
  if(nrow(cmod)== 0){return()}
  # Replace NAs with 0 value so that cases in which only one model reveals an effect are displayed correctly (limitation of facet_grid)
  cmod %>% mutate(effect = ifelse(is.na(effect), 0, effect)) %>%
    group_by(dep) %>%
    do(gg = {ggplot(., aes(x = time, y = model)) +
        geom_tile(aes(fill = effect)) +
        scale_fill_gradient2(na.value = NA, name = "Effect", oob = scales::squish) +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_discrete(expand = c(0,0), limits = rev) +
        geom_vline(xintercept = bs) +
        facet_grid(dep~.) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "bottom",
              strip.background = element_blank(),
              axis.text = element_blank(), #element_text(size = 12, color = "black"),
              strip.text.y = element_text(size = 12)) +
        labs(x = NULL, y = NULL)
    }) %>%
    .$gg %>% arrangeGrob(grobs = ., ncol = 1) %>% grid.arrange()
}

get_breaks <- function(mods, sign = "all"){
  breaks <- c()
  for(s in 1:nrow(mods)){
    breaks <- mods %>% slice(s) %>% pull(is) %>% first %>% break_uncertainty(.) %>% rbind(breaks, .)}
  if(sign == "neg"){
    neg_b <- breaks %>% filter(coef < 0) %>% pull(id) %>% unique
    return(neg_b)
  }else if(sign == "pos"){
    pos_b <- breaks %>% filter(coef > 0) %>% pull(id) %>% unique
    return(pos_b)
  }else if(sign == "all"){
    all_b <- breaks %>% pull(id) %>% unique
    return(all_b)}
}