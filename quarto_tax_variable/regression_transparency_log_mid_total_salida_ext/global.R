####################
# Global functions #
####################
# Author: Alex Bajaña

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(shiny)
library(shiny.semantic)
library(texreg)
library(gt)
library(ggrepel)
# 
# datos <- read_tsv("20240304/Tablas/params/dep_var_tasa_ir_joint.txt")
# gdatos <- read_tsv("20231212/Tablas/performance/dep_var_pff_p_joint.txt")


save_result <- function(result) {
  # Accede al environment global y actualiza el contador
  results_envir$counter <- results_envir$counter + 1
  
  # Construye el nombre del nuevo elemento
  name <- paste("result", results_envir$counter, sep = "_")
  
  # Asigna el resultado al environment con el nombre construido
  assign(name, result, envir = results_envir)
}

save_perfor <- function(result) {
  # Accede al environment global y actualiza el contador
  perfor_envir$counter <- perfor_envir$counter + 1
  
  # Construye el nombre del nuevo elemento
  name <- paste("perfor", perfor_envir$counter, sep = "_")
  
  # Asigna el resultado al environment con el nombre construido
  assign(name, result, envir = perfor_envir)
}


# Graficos para los coeficientes ------------------------------------------


coef_simple_table <- function(tidy_tems,
                              title_plot,
                              subtitle_plot,
                              type = "lm"){
  
  
  if(type == "lm"){
    # browser()
    plot_table <- tidy_tems %>% 
      mutate(term = str_replace(term,"(:?)Post",":"),
             term = str_to_sentence(term)
      )
  }else{
    
    
    
    plot_table <- tidy_tems %>% 
      filter(str_detect(term,"treatment")) %>% 
      mutate(
        term = str_remove_all(term,"i\\(.*\\)"),
        term = str_remove_all(term,"anio_fiscal::"),
        term = str_remove_all(term,":treatment"),
        term = as.numeric(term)) %>% 
      filter(!is.na(term)) %>% 
      bind_rows(tibble(term = rep(2014,11),
                       estimate = rep(0,11),
                       conf.low = rep(0,11),
                       conf.high = rep(0,11),
                       population  = c("all",
                                       "cit_m22",
                                       "treat_a",
                                       "treat_b",
                                       "treat_c",
                                       "cit_m0" ,
                                       "main_group_0" ,
                                       "main_group_1",
                                       "main_group_50", 
                                       "minor_group_50", 
                                       "domestic_50" ))) %>% 
      right_join(tidy_tems %>% distinct(population)) 
  }
  
  
  
  plot_table <- plot_table %>% 
    mutate(
      sample = population,
      population = case_when(
      population == "all" ~ "All firms",
      population == "cit_m22" ~ "CIT >= Statutory",
      population == "treat_a" ~ "Treatment A",
      population == "treat_b" ~ "Treatment B",
      population == "treat_c" ~ "Treatment C",
      population == "cit_m0" ~ "CIT > 0",        
      population == "main_group_0" ~ "Mean p. main group  post == 0%",  
      population == "main_group_1"  ~ "Mean p. main group  post > 0%",
      population == "main_group_50" ~ "Mean p. main group  post >= 50%", 
      population == "minor_group_50" ~ "Mean p. minor group  post >= 50%", 
      population == "domestic_50" ~ "Mean p. Domestic >= 50%"
    ),
    population = factor(population,
                        levels = c("All firms",
                                   "CIT >= Statutory",
                                   "Treatment A",
                                   "Treatment B",
                                   "Treatment C",
                                   "CIT > 0",        
                                   "Mean p. main group  post == 0%",  
                                   "Mean p. main group  post > 0%",
                                   "Mean p. main group  post >= 50%", 
                                   "Mean p. minor group  post >= 50%", 
                                   "Mean p. Domestic >= 50%")),
    population = fct_drop(population)
    )   %>%
    filter(!is.na(population))
  
  save_result(plot_table)
  
  return(plot_table)
}

coef_plot_plus <- function(tidy_tems,
                           title_plot,
                           subtitle_plot,
                           type = "lm"){
 
  plot_table <- coef_simple_table(tidy_tems,
                                  title_plot,
                                  subtitle_plot,
                                  type) %>% 
    mutate(population = fct_relabel(population, function(x) str_wrap(x, width = 20)))
  
  # browser()
  
  plot_out <- plot_table %>% 
    ggplot(aes(x = term, 
               y = estimate,
               color = population,
               group = population)) +
    geom_point(size = 3)  +
    geom_linerange(aes(x = term,
                       ymin = conf.low, 
                       ymax = conf.high,
               group = population )) +
    geom_line(aes(x = term,
                  y = estimate,
               group = population),
              alpha = 0.7) +
    geom_hline(aes(yintercept = 0),linetype = "dashed") +
    geom_vline(aes(xintercept = 2014.5),linetype = "dashed") +
    theme_light() +
    theme(axis.title= element_blank(),text = element_text(size = 9),
          plot.margin = margin(t = 0,r = 0,b = 0.1,l = 0, unit = "cm"),
          legend.position = "bottom",legend.text = element_text(size = 8), 
          title = element_text(size = 12)) +
    labs(title = title_plot,
         subtitle = subtitle_plot,
         color = "Population") + 
    guides(color = guide_legend(nrow = 2))
  
  
  
  if(type == "es"){
    
    # browser()
    
    plot_out <- plot_out +
      geom_line(aes(x = term, 
                    y = estimate,
                    color = population,
               group = population
                    ),
              
                alpha = 0.7) +
      geom_vline(aes(xintercept = 2014.5),linetype = "dashed") +
      geom_hline(aes(yintercept = 0),linetype = "dashed") +
      scale_x_continuous(breaks = 2007:2019,
                         labels = c(2007:2019))  +
      theme_light() +
      theme(axis.title= element_blank(),text = element_text(size = 13),
            plot.margin = margin(t = 0,r = 0,b = 0.1,l = 0, unit = "cm"),
            legend.position = "bottom",legend.text = element_text(size = 8), 
            title = element_text(size = 15)) +
      labs(title = title_plot,
           subtitle = subtitle_plot,
           color = "Population") + 
      guides(color = guide_legend(nrow = 2))
  }
  
  return(plot_out)
  
}

# coef_plot_plus(model_df_0 %>% filter(model == "Event study"),
#                title_plot  = str_c("Event Study Design: ",params$var_lab),
#                subtitle_plot  = "Unsaturated model with firm clustered standard errors",
#                type = "es")

# coef_simple_table(datos %>% filter(model == "Event study assets weighted with fixed effects"),
#                "Tasa IR",
#                "T-Maj + T-Min vs C-Maj + C-Min",
#                type = "es")
# 
# coef_plot_plus(datos %>% filter(model == "Event study assets weighted with fixed effects"),
#                "Tasa IR",
#                "T-Maj + T-Min vs C-Maj + C-Min",
#                type = "es")


# Función para poner los modelos en modo tabla ----------------------------

extract_broom <- function(tidy_model, glance_model, type = "lm"){
  
  # browser()
  
  
  if(type == "lm"){
    # browser()
    plot_table <- tidy_model %>% 
      mutate(term = str_replace(term,"(:?)Post",":"),
             term = str_to_sentence(term)
      )
  }
  
  # get estimates/standard errors from tidy
  
  coef <- tidy_model$estimate
  
  ci_low <- tidy_model$conf.low
  ci_up <- tidy_model$conf.high
  
  coef.names <- as.character(tidy_model$term)
  
  se <- tidy_model$std.error
  
  pvalues <- tidy_model$p.value
  
  # get goodness-of-fit statistics from glance
  
  glance_transposed <- glance_model %>% 
    select(-model) %>% 
    pivot_longer(everything()) %>% 
    filter(!is.na(value))
  
  # as_tibble(cbind(name = names(glance_model), value = t(glance_model)))
  
  gof.names <- as.character(glance_transposed$name)
  
  gof <- as.double(glance_transposed$value)
  
  gof.decimal <- gof %% 1 > 0
  
  # browser()
  
  tr_object <- texreg::createTexreg(coef.names = coef.names,
                                    coef = coef,
                                    se = se,
                                    pvalues = pvalues,
                                    gof.names = gof.names,
                                    gof = gof,
                                    gof.decimal = gof.decimal)
  return(tr_object)
}


# extract_broom (datos %>% 
#                  filter(model == "Event study assets weighted",
#                         population == "all") %>% 
#                  select(-population),
#                gdatos %>% 
#                  filter(model == "Event study assets weighted",
#                         population == "all") %>% 
#                  select(-population),type = "lm")

make_multi_model <- function(model_df_0,
                             model_df_0_p,
                             modelo,
                             tipo,
                             treatment){
  
  
  # browser()
  
  model_df <- model_df_0 %>% 
    filter(model == modelo) 
  
  model_df_p <- model_df_0_p %>% 
    filter(model == modelo)
  
  # browser()
  elemnts <- list(model_df,
                  model_df_p) %>% 
    map(~.x %>% 
          mutate(
            sample_spec = population,
            treatment_spec = treatment,
            population = case_when(
            population == "all" ~ "All firms",
            population == "cit_m22" ~ "CIT >= Statutory",
            population == "treat_a" ~ "Treatment A",
            population == "treat_b" ~ "Treatment B",
            population == "treat_c" ~ "Treatment C",
            population == "cit_m0" ~ "CIT > 0",        
            population == "main_group_0" ~ "Mean p. main group  post == 0%",  
            population == "main_group_1"  ~ "Mean p. main group  post > 0%",
            population == "main_group_50" ~ "Mean p. main group  post >= 50%", 
            population == "minor_group_50" ~ "Mean p. minor group  post >= 50%", 
            population == "domestic_50" ~ "Mean p. Domestic >= 50%"
          ),
          population = factor(population,
                              levels = c("All firms",
                                         "CIT >= Statutory",
                                         "Treatment A",
                                         "Treatment B",
                                         "Treatment C",
                                         "CIT > 0",        
                                         "Mean p. main group  post == 0%",  
                                         "Mean p. main group  post > 0%",
                                         "Mean p. main group  post >= 50%", 
                                         "Mean p. minor group  post >= 50%", 
                                         "Mean p. Domestic >= 50%")),
          population = fct_drop(population)
          )  ) 
  
  
  save_perfor(elemnts[[2]])
  
  elemnts <- elemnts %>% 
    map(~ .x %>% split(.$population)) %>% 
    transpose()
  
  
  
  lista <- map(.x = elemnts,
               ~{ 
                 
                 tabla <- map(.x,select,-population,-sample_spec,-treatment_spec)
                 
                 extract_broom(tidy_model = tabla[[1]],
                               glance_model = tabla[[2]],
                               type = tipo)
               })
  
  screenreg(lista)

}

# make_multi_model(model_df_0,
#                  model_df_0_p,
#                  "Event study assets weighted",
#                  "lm")

# make_multi_model(model_df_0,
#                  model_df_0_p,
#                  "Event study",
#                  "es")

# Objetos extra -----------------------------------------------------------

my_grid_template <- grid_template(
  default = list(
    areas = 
      rbind(
        c("sidebar","title","title","title","title","title"),
        c("sidebar","control","blank1" ,"modelo_tipo","modelo_design" ,"blank2" ),
        c("sidebar","main" ,"main" ,"main","main","table" ),
        c("sidebar","footer","footer" ,"footer" ,"footer" ,"footer" )),
    cols_width = c("22.5%","15%","5%","15%" ,"17.5%","25%"),
    rows_height = c("5%","5%", "80", "10%")
  ))


choices_grupo <- c("T-Maj + T-Min vs C-Maj + C-Min" = "joint",
                   "T-Maj vs C-Maj" = "majors",
                   "T-Min vs C-Min" = "minors")

choices_variable <- c("Tax haven participation" = "pff_p",
                      "Foreign participation" = "ext_p",
                      "Log amount of assets attributable to TH" = "log_assets_attr_pff",
                      "Log amount of assets attributable to non TH" = "log_assets_attr_ext",
                      "Log(CIT liability)" = "log_cit_liability",
                      "Log(Profits)"  = "log_utility",
                      "Log(Taxable profits)" = "log_taxable_profits",
                      "Prominent participation in group" = "prominent",
                      "Amount of assets atributables in dominant group"  = "log_assets_prominent")

choices_model <- c("Saturarated model" = "satu",
                   "Fixed effect model" = "fe")

choices_design <- c("Diff-in-diff design" = "lm",
                    "Event study design"  = "es")


# coef_plot_plus(model_df,
#                title_plot  = str_c("Event Study Design: ",params$var_lab),
#                subtitle_plot  = "Unsaturated model with firm clustered standard errors",
#                type = "es")
