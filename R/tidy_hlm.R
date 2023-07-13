#' Extracts a Tidy list of summary objects from a lmer object
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
