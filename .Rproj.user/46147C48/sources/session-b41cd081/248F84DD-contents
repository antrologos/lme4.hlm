
get_terms_from_char = function(x){
        x <- stringr::str_split_1(x, pattern = "[~]")
        x <- unlist(lapply(x, FUN = \(x) stringr::str_split_1(x, pattern = "[+]")))
        x <- lapply(x, FUN = \(x) stringr::str_split_1(x, pattern = "[*]"))
        x <- lapply(x, FUN = \(x) stringr::str_trim(x))
}


get_parameter_type = function(formula_terms, parameter){

        formula_sub = formula_terms[which(sapply(formula_terms, \(x) parameter %in% x, simplify = T))]
        formula_sub = unlist(formula_sub)

        if(length(formula_sub) == 1){
                "Intercept"
        }else if(length(formula_sub) > 1){
                "var_coef"
        }else{
                stop(paste("Invalid parameter type:", parameter))
        }
}


#' HLM formula to lmer formula
#'
#' Converts a character vector representing a Multilevel/Hierarchical Linear Model formula to a mixed model formula for using with lmer (package lme4)
#'
#' @param formula_char .
#' @param correlated_ranef .
#' @export
formula_hlm_to_lmer = function(formula_char,
                               correlated_ranef = T){


        formula_split = unlist(stringr::str_split(formula_char, pattern = "\n"))
        formula_split = formula_split[nchar(formula_split) > 0]

        level1_start        = grep(x = formula_split, pattern = "Level 1", ignore.case = T)
        formula_level1_char = formula_split[level1_start + 1]
        level_1_terms       = get_terms_from_char(formula_level1_char)
        level_1_vars        = unlist(lapply(level_1_terms, \(x) x[-1]))

        level2_start         = grep(x = formula_split, pattern = "Level 2", ignore.case = T)
        formula_level2_char  = formula_split[(level2_start + 1):length(formula_split)]

        #
        parameters_df = tibble::tibble(var   = c("(Intercept)" ,level_1_vars),
                                       par   = sapply(level_1_terms[-1], \(x) x[1], simplify = T),
                                       level = 1)

        formula_mixed = formula_level1_char
        fixed_effects = level_1_vars
        random_effects_df = tibble::tibble()
        #k = 1
        for(k in 1:length(formula_level2_char)){

                formula_level2_k = formula_level2_char[k]
                level2_terms_k    = get_terms_from_char(formula_level2_k)

                parameter_k    = as.character(level2_terms_k[[1]])
                parameter_type = get_parameter_type(level_1_terms, parameter_k)

                if(parameter_type == "Intercept"){
                        var_k = "1"
                }else{
                        possible_var = stringr::str_split_1(string = formula_level1_char, pattern = "[~]|[+]")
                        possible_var = grep(x = possible_var, pattern = parameter_k, value = T)
                        possible_var = stringr::str_remove(string = possible_var, pattern = paste0(parameter_k, "[*]"))
                        possible_var = stringr::str_trim(string = possible_var)

                        var_k = possible_var
                }

                level_2_vars = unlist(lapply(level2_terms_k, \(x) x[-1]))

                if(parameter_type == "Intercept"){
                        level_2_fixed_effects_k = level_2_vars
                }else{
                        if(length(level_2_vars) > 0){
                                level_2_fixed_effects_k = paste0(var_k, ":", level_2_vars)
                        }else{
                                level_2_fixed_effects_k = NULL
                        }
                }

                fixed_effects = c(fixed_effects, level_2_fixed_effects_k)

                random_effects_part_k = stringr::str_extract(string  = formula_level2_k,
                                                             pattern = "r\\(.+\\)")

                random_effects_k = unlist(stringr::str_split(string = random_effects_part_k, pattern = "[+]"))
                random_effects_k = stringr::str_trim(random_effects_k)
                random_effects_k = stringr::str_remove_all(string = random_effects_k, pattern = "^r\\(|\\)$")

                random_effects_df_k = tibble::tibble(variable      = var_k,
                                                     random_effect = random_effects_k)

                random_effects_df = dplyr::bind_rows(random_effects_df, random_effects_df_k)

                parameters_df_k = tibble::tibble(var       = level2_terms_k[[1]],
                                                 par       = sapply(level2_terms_k[-1], \(x) x[1], simplify = T),
                                                 level_var = sapply(level2_terms_k[-1], \(x)
                                                                    ifelse(length(x[-1])>0, x[-1], NA_character_),
                                                                    simplify = T),
                                                 level = 2)

                parameters_df_k = parameters_df_k[ !(stringr::str_detect(parameters_df_k$par, pattern = "^r\\(|\\)$")), ]
                parameters_df_k = dplyr::mutate(.data = parameters_df_k,
                                                i = 1:nrow(parameters_df_k),
                                                level_var = ifelse(i == 1 & is.na(level_var), "(Intercept)", level_var))
                parameters_df_k = dplyr::select(.data = parameters_df_k, -i)

                parameters_df = dplyr::bind_rows(parameters_df, parameters_df_k)

        }

        random_effects_df <- dplyr::filter(.data = random_effects_df, !is.na(random_effect))

        random_effects = unique(random_effects_df$random_effect)

        random_effects_part = NULL
        for(re in random_effects){

                variables = dplyr::filter(.data = random_effects_df, random_effect == re)
                variables = dplyr::pull(.data = variables, variable)

                if(correlated_ranef == T){
                        ranef_k = paste0("(", paste(variables, collapse = " + "), "|", re, ")")
                }else{
                        ranef_k = paste0("(", variables, "|", re, ")")
                }

                random_effects_part = c(random_effects_part, ranef_k)
        }

        dep_var = level_1_terms[[1]]

        formula_lmer = paste(dep_var, "~", paste(c(fixed_effects, random_effects_part), collapse = " + "))


        parameters_df = dplyr::bind_rows(dplyr::filter(.data = parameters_df, level == 1),

                                         dplyr::left_join(x = dplyr::filter(.data = parameters_df, level == 2),

                                                          y = dplyr::select(.data = dplyr::filter(.data = parameters_df,
                                                                                                  level == 1,
                                                                                                  var != "(Intercept)"),
                                                                            var = par,
                                                                            interaction = var),
                                                          by = "var"))

        parameters_df <- dplyr::mutate(.data = parameters_df,
                                       term = dplyr::case_when(level == 1 ~ var,
                                                               level == 2 & is.na(interaction) ~ level_var,
                                                               level == 2 & !is.na(interaction) & level_var == "(Intercept)" ~ interaction,
                                                               level == 2 & !is.na(interaction) & level_var != "(Intercept)" ~ paste0(interaction, ":", level_var)))

        formula = as.formula(formula_lmer)

        attr(formula, "parameters_df") <- parameters_df

        formula
}

#' Tidy HLM summary from a lmer object
#'
#' Extracts and organizes the results from a lmer model into a HLM summary
#'
#' @param model A model estimated with lmer (package lme4), using a HLM formula converted using formula_hlm_to_lmer.
#' @export
tidy_hlm = function(model){

        formula_str   = formula(model)
        parameters_df = attr(formula_str, "parameters_df")

        effects_df = broom.mixed::tidy(model)

        effects_df$p.value = 2*(1 - pnorm(abs(effects_df$statistic)))

        level1 = dplyr::filter(.data = parameters_df, level == 1)
        level1 = dplyr::left_join(x = level1, y = effects_df, by = "term")
        level1 = dplyr::select(.data = level1, -level_var, -level, -interaction, - effect, -group, - term)
        level1 = dplyr::select(.data = level1, par, var, dplyr::everything())

        level2 = dplyr::filter(.data = parameters_df, level == 2)
        level2 = dplyr::left_join(x = level2, y = effects_df, by = "term")
        level2 = dplyr::select(.data = level2, -term, -interaction, - effect, -group)
        level2 = dplyr::select(.data = level2, var, par, dplyr::everything())
        level2 = split(x = level2, f = level2$var)
        level2 = lapply(X = level2, FUN = \(x) dplyr::select(.data = x, -var, - level))

        random = dplyr::filter(.data = effects_df, effect == "ran_pars")
        random = dplyr::select(.data = random, group, term, estimate)
        random_eff = dplyr::filter(.data = random, stringr::str_detect(string = term, pattern = "^sd_"))

        random_cor = dplyr::filter(.data = random, stringr::str_detect(string = term, pattern = "^cor_"))
        random_cor = dplyr::mutate(.data = random_cor, term = stringr::str_remove(term, "cor__"))
        random_cor = tidyr::separate(data = random_cor, col = term, into = c("var1", "var2"), sep = "[.]")
        random_cor = tidyr::pivot_wider(data = random_cor, names_from = var2, values_from = estimate)

        ICC = performance::icc(model)
        diagnostics = performance::performance(model)

        diagnostics$ICC <- NULL

        all_effects <- list(level1 = level1,
                            level2 = level2,
                            random_eff = random_eff,
                            random_cor = random_cor,
                            ICC = ICC,
                            diagnostics = diagnostics)

        all_effects
}



