#' # Helper functions used throughout {.tabset .tabset-sticky}
#' documentation on the functions is interspersed through code comments
#'
#' ## set some options
#' dont show messages when loading libraries
library = function(...) suppressMessages(base::library(...))
#' never set strings as factors automatically (google for reason why)
options(stringsAsFactors = FALSE)
#' show four significant digits tops
options(digits = 4)
#' tend not to show scientific notation, because we're just psychologists
options(scipen = 7)
#' make output a bit wider
options(width = 110)
#' set a seed to make analyses depending on random number generation reproducible
set.seed(1710) # if you use your significant other's birthday make sure you stay together for the sake of reproducibility


#' ## Load packages
#' generate the site
library(rmarkdown)
#' set options for chunks
library(knitr)
#' my formr utility package to generate e.g. the bibliography
library(formr)
#' pretty-printed output
library(pander)
#' tidyverse date times
library(lubridate)
#' tidyverse strings
library(stringr)
#' extractor functions for models
library(broom)
#' grammar of graphics plots
library(ggplot2)
#' svg graphs
# library(svglite);
library(ggthemes)
library(codebook)

#' tidyverse: has a lot of naming conflicts, so always load last
library(tidyverse)

#' some packages may be needed without being loaded
fool_packrat = function() {
  # needed to install formr package
  library(devtools)
  # needed to actually run rmarkdown in RStudio, but for some reason not in its dependencies
  library(formatR)
  library(cmdstanr)
}

#' ## Spin R files
#' R scripts can be documented in markdown using Roxygen comments, as demonstrated here
#' This function turns all R files (that don't have an Rmd file of the same name and that don't start with an underscore _) into HTML pages
spin_R_files_to_site_html = function() {
  library(knitr)
  all_Rs = c(list.files(pattern = "^[^_].+\\.R$"), ".Rprofile")
  component_Rmds = list.files(pattern = "^_.+\\.Rmd$")
  temporary_Rmds = c()
  for (i in seq_along(all_Rs)) {
    if(all_Rs[i] == ".Rprofile") {
      Rmd_file = ".Rprofile.Rmd"
    } else {
      Rmd_file = paste0(all_Rs[i], "md")
    }
    if (!file.exists(Rmd_file)) {
      next_document = length(temporary_Rmds) + 1
      if(file.exists(all_Rs[i])) {
        temporary_Rmds[next_document] = spin(all_Rs[i], knit = FALSE, envir = new.env(), format = "Rmd")
        prepended_yaml = paste0(c("---
                                  output:
                                  html_document:
                                  code_folding: 'show'
                                  ---
                                  
                                  ", readLines(temporary_Rmds[next_document])), collapse = "\n")
        cat(prepended_yaml, file = temporary_Rmds[next_document])
      }
    }
    }
  components_and_scripts = c(temporary_Rmds, component_Rmds)
  for (i in seq_along(components_and_scripts)) {
    opts_chunk$set(eval = FALSE, cache = FALSE)
    # if we call render_site on the .R file directly it adds a header I don't like
    rmarkdown::render_site(components_and_scripts[i], quiet = TRUE)
  }
  opts_chunk$set(eval = TRUE, cache = TRUE)
  unlink(temporary_Rmds)
    }

#' ## Output options
#' use pander to pretty-print objects (if possible)
opts_chunk$set(
  dev = "png"
)

#' don't split tables, scroll horizontally
panderOptions("table.split.table", Inf)


#' ##### we use this function to automatically get nice tables
pander_handler = function(x, ...) {
  anyS3method = function(x) {
    classes = class(x)
    any(sapply(classes, FUN = function(classes) { !is.null(getS3method('pander',classes,TRUE)) } ))
  }
  if ("knit_asis" %in% class(x)) {
    x # obj is knit_asis already, don't touch it 
    # (useful if e.g. pander is called with options in the doc)
  } else if (anyS3method(x)) {
    pander(x, row.names = F, ...) # if method available, pander
  } else if (isS4(x)) {
    show(x)
  } else {
    print(x)
  }
}




#' ## curve plot
#' plot residuals/raw values over reverse cycle days relative to ovulation
plot_curve = function(obj, diary, caption_x = "Days until next menstruation") {
  options <- list(
    fig.path = paste0(knitr::opts_chunk$get("fig.path"), digest::digest(list(obj, diary))),
    cache.path = paste0(knitr::opts_chunk$get("cache.path"),  digest::digest(list(obj, diary)))
  )
  asis_knit_child("_plot_curve.Rmd",
                  options = options)
}


##### robustness analyses

plot_triptych = function(obj, x.var = 'fertile_fab', multiline=F, partial.residuals = F, xlevels = 3, term = NULL, panel_rows = 2) {
  library(effects)
  if(length(attributes(obj)$effects) == 0) {
    attributes(obj)$effects = allEffects(obj, xlevels = xlevels, partial.residuals = partial.residuals)
  }
  if(is.null(term)) {
    interaction = attributes(obj)$effects[
      which.max(stringr::str_length(names(attributes(obj)$effects)))
      ]
  } else {
    interaction = attributes(obj)$effects[ names(attributes(obj)$effects) == term ]
  }
  if (multiline) {
    panel_rows = 1
    colors = c("red","black")
  } else {
    colors = "black"
  }
  layout = c(xlevels,panel_rows,1)
  plot(interaction,x.var = x.var, multiline = multiline, layout = layout, colors = colors, rug = F)
  invisible(obj)
}


do_model = function(model, diary, model_prefix) {
  rmdpartials::partial("_robustness_model.Rmd", model = model, diary = diary, 
                       model_prefix = model_prefix)
}

##### counting excluded participants

n_excluded = function(x) {
  excluded_new = sum(is.na(x) | x == FALSE,na.rm = T)
  if (is.null(excluded_old)) {
    excluded = excluded_new
  } else {
    excluded = excluded_new - excluded_old
  }
  cat(excluded, "excluded\n")
  excluded_old <<- excluded_new
  excluded
}

### counting excluded days 

n_excluded_days = function(x) {
  excluded_new_days = sum(is.na(x) | x == FALSE,na.rm = T)
  if (is.null(excluded_old_days)) {
    excluded = excluded_new_days
  } else {
    excluded = excluded_new_days - excluded_old_days
  }
  cat(excluded, "excluded\n")
  excluded_old_days <<- excluded_new_days
  excluded
}

### counting excluded days in social diary

n_excluded_social = function(x) {
  excluded_new_social = sum(is.na(x) | x == FALSE,na.rm = T)
  if (is.null(excluded_old_social)) {
    excluded = excluded_new_social
  } else {
    excluded = excluded_new_social - excluded_old_social
  }
  cat(excluded, "excluded\n")
  excluded_old_social <<- excluded_new_social
  excluded
}

### counting excluded participants in social network

n_excluded_network = function(x) {
  excluded_new_network = sum(is.na(x) | x == FALSE,na.rm = T)
  if (is.null(excluded_old_network)) {
    excluded = excluded_new_network
  } else {
    excluded = excluded_new_network - excluded_old_network
  }
  cat(excluded, "excluded\n")
  excluded_old_network <<- excluded_new_network
  excluded
}

###### strict exclusion
n_excluded_network_strict = function(x) {
  excluded_new_network_strict = sum(is.na(x) | x == FALSE,na.rm = T)
  if (is.null(excluded_old_network_strict)) {
    excluded = excluded_new_network_strict
  } else {
    excluded = excluded_new_network_strict - excluded_old_network_strict
  }
  cat(excluded, "excluded\n")
  excluded_old_network_strict <<- excluded_new_network_strict
  excluded
}

###### bar plot 

bar_count = function(data, variable, na.rm = FALSE) {
  varname = deparse(substitute(variable))
  var = data %>% select_(varname) %>% .[[1]]
  if (na.rm == T) {
    var = var %>% na.omit()
  }
  var = factor(var, exclude = NULL)
  data$var = var
  
  ggplot(data, aes(x = var)) +
    geom_bar() +
    stat_count(aes(label = paste(..count.., "\n", scales::percent(round(..count../sum(count),2)))), hjust = -0.1, geom = "text", position = "identity", na.rm = T) +
    scale_y_continuous(expand = c(0.1, 0)) +
    xlab(varname) +
    coord_flip()
}

theme_set(theme_tufte(base_size = 20, base_family='Helvetica Neue'))

plot_interaction = function(model, nr = 3) {
  ef = allEffects(model, xlevels = 2)
  # ef = allEffects(model, x.var = "fertile_fab", xlevels = 2)
  int = names(ef)[nr]
  int_ef = data.frame(ef[[int]])
  int_parts = stringr::str_split(int, ":")[[1]]
  int_ef[, int_parts[2]] = factor(int_ef[, int_parts[2]])
  ggplot(int_ef, 
         aes_string(x = int_parts[1], y = "fit", ymin = "lower", ymax = "upper")) + 
    geom_smooth(aes_string(colour = int_parts[2],
                           fill = int_parts[2]),
                stat = 'identity') + 
    facet_wrap(as.formula(paste0("~ ", int_parts[3])), labeller = labeller(hormonal_contraception = c(`FALSE` = 'cycling', `TRUE` = 'hormonal contraception'))) + 
    ggtitle(paste0("Moderation by ", str_replace_all(int_parts[2], "_", " "))) +
    scale_color_solarized(guide = F) +
    scale_fill_solarized(guide = F) + 
    scale_x_continuous("Conception risk estimate", breaks = c(0, 0.3, 0.5))
  
}

plot_2way_interaction = function(model, nr = 3) {
  ef = allEffects(model, x.var = "fertile_fab", xlevels = 2)
  int = names(ef)[nr]
  int_ef = data.frame(ef[[int]])
  int_parts = stringr::str_split(int, ":")[[1]]
  int_ef[, int_parts[2]] = factor(int_ef[, int_parts[2]])
  ggplot(int_ef, 
         aes_string(x = int_parts[1], y = "fit", ymin = "lower", ymax = "upper")) + 
    geom_smooth(aes_string(colour = int_parts[2],
                           fill = int_parts[2]),
                stat = 'identity') + 
    ggtitle(paste0("Moderation by ", str_replace_all(int_parts[2], "_", " "))) +
    scale_color_solarized(guide = F) +
    scale_fill_solarized(guide = F) + 
    scale_x_continuous("Conception risk estimate", breaks = c(0, 0.3, 0.5))
  
}

multi_rel = function(diary, lme = T, lmer = T) { 
  mrel = diary %>%
    group_by(short) %>% 
    filter(!is.na(risk_taking)) %>% 
    filter(day_number <= 70) %>%
    gather(variable, value, -short, -day_number) %>%
    psych::multilevel.reliability(., "short", "day_number", lme = lme, lmer = lmer, items = "variable", values = "value", long = T, aov = F)
  mrel
}


robust_rowmeans <- function(x) { 
  y <- rowMeans(x, na.rm = TRUE)
  y[is.nan(y)] <- NA_real_
  y
}


cortest_stretch <- function(d) {
  var_pairs <- t(combn(names(d), 2)) %>%
    as_data_frame() %>% 
    setNames(c("x", "y"))
  
  p_values <- var_pairs %>% 
    dplyr::mutate(r.test = purrr::map2(x, y, ~ stats::cor.test(d[[.x]], d[[.y]])),
                  r.test = purrr::map(r.test, broom::tidy)) %>%
    tidyr::unnest(r.test)
  p_values
}


cut_common_stem <- function(x) {
  i = 1
  while (i <= max(stringr::str_length(x)) & dplyr::n_distinct(stringr::str_sub(x, 1, i)) == 1) {
    i = i + 1
  }
  stringr::str_sub(x, i)
}



#' # Send job to cluster {.tabset .tabset-sticky}
#' This function is used to send jobs to a computing cluster.
#' It accomplishes the following needs:
#' - wait for errors during building the model
#' - close the SSH connections to the cluster if building the model worked
#'    - this is useful so I don't open hundreds of SSH tunnels to send over
#'      models
#' - if the model has been built already, check if LOO has been computed, if not, rerun
#' - if we're not in an interactive session, load the model (this allows me to use
#' the same code to retrieve the model that I use to send it off)
#'
send_job_to_cluster <- function(expr,
                                job_name = 'desire', queue = "default",
                                walltime = "10:0:0", memory = 10, ncpus = 6,
                                interactive_call = interactive(),
                                wait_till_error = 180,
                                template = 'torque-lido.tmpl') {
  file <- paste0(job_name, ".rds")
  file_there <- file.exists(file)
  file_and_loo_there <- FALSE
  if (file_there) {
    mod <- readRDS(file)
    file_and_loo_there <- !is.null(mod$loo)
  }
  if (file_and_loo_there) {
    if (interactive_call) {
      "done"
    } else {
      mod
    }
  } else {
    if (file.exists(template)) { 
      qsub <- tweak(batchtools_torque, template = template,
                    # workers = "export LSF_ENVDIR=/opt/lsf/conf",
                    resources = list(job.name = job_name,
                                     queue = queue,
                                     walltime = walltime,
                                     memory = memory,
                                     ncpus = ncpus))
      plan(qsub)
    }
    expr <- substitute(expr)
    job <- future(expr, substitute = FALSE,
                  conditions = character(0),
                  stdout = NA)
    i <- 0
    while (i < wait_till_error) {
      if (resolved(job)) {
        break
      } else {
        Sys.sleep(3)
        i <- i + 3
      }
    }
    if (resolved(job)) {
      value(job)
    } else {
      "sent"
    }
  }
}
model_time <- function(model) {
  rstan::get_elapsed_time(model$fit) %>% rowSums() %>% max() %>% {./60/60}
}


curve_plot <- function(model, outcome) {
  rmdpartials::partial("_model_based_curve.Rmd", model = model, outcome = outcome)
}

custom_forest <- function(model, pars) {
  coefs <- coef(model, probs = c(0.1, 0.9), pars = pars)
  coefs_df <- as_tibble(coefs$person[,,"fertile_fab"], rownames = "person") %>%     left_join(model$data %>% select(person, hormonal_contraception) %>% mutate(person = as.character(person)))
  coefs_df %>% arrange(Estimate) %>% 
    mutate(person = fct_inorder(person)) %>% 
    ggplot(aes(person, Estimate, ymin = `Q10`, ymax = `Q90`, color = hormonal_contraception)) + 
    geom_pointrange(size = 0.5, fatten = 0.1) +
    scale_color_viridis_d("Contraception", begin = 0.4, end = 0.9, labels = c("TRUE" = "Hormonal", "FALSE" = "Non-hormonal"), breaks = c("TRUE", "FALSE")) +
    coord_flip()
}

summary.brmsfit <- function(...) {
  brms:::summary.brmsfit(..., prob = 0.99)
}
