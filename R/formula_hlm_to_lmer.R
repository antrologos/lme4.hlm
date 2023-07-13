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
