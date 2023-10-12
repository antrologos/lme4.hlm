#' Tidy HLM Summary for a lmer object
#'
#' @param model A model estimated with lmer (package lme4), using a HLM formula converted using formula_hlm_to_lmer.
#' @export
summary_hlm = function(model){

        tables = tidy_hlm(model)

        f = formula(model)
        parameters_df = attr(f, "parameters_df")

        level2_deps = names(tables[["level2"]])
        level2_deps = dplyr::filter(.data = parameters_df,
                                    var %in% level2_deps)
        level2_deps = dplyr::summarise(dplyr::group_by(.data = level2_deps, var), Variables = unique(interaction))
        level2_deps = dplyr::rename(level2_deps, Parameters = var)
        level2_deps = dplyr::mutate(level2_deps, Variables = ifelse(is.na(Variables), "(Intercept)", Variables))
        level2_deps <- dplyr::mutate(level2_deps,
                                     Parameters = stringr::str_replace(Parameters,
                                                                       "^b",
                                                                       "\\&beta;<sub>"),
                                     Parameters = paste0(Parameters, "</sub>"),
                              headers = paste0("<b>Level 2</b>: ", Parameters, ", ", Variables))


        level1 = lme4.hlm:::prep_table(tables[[1]], letter = "b", greek_letter = "beta")
        level2 = lapply(tables[[2]], lme4.hlm:::prep_table, letter = "g", greek_letter = "gamma")

        random = dplyr::mutate(tables[[3]],
                               term = stringr::str_replace(term,
                                                           "^sd__",
                                                           "\\&tau;<sub>"),
                               term = paste0(term, "</sub>"),

                               term = ifelse(term == last(term), "&sigma;<sup>2</sup>", term),
                               estimate = round(estimate^2, 3))

        random = dplyr::rename(random, Value = estimate)
        random = htmlTable::htmlTable(random, rnames = F)

        correlations = lapply(split(tables[[4]], tables[[4]]$group),
                              \(x){
                                      htmlTable::htmlTable(dplyr::mutate_if(.tbl = x,
                                                                            .predicate = is.numeric,
                                                                            .funs = \(v) round(v, 3)))
                              })

        ICC = htmlTable::htmlTable(dplyr::mutate_if(as.data.frame(tables[[5]]),
                                                    is.numeric,
                                                    \(v) round(v, 3)),
                                   rnames = F)

        diagnostics = htmlTable::htmlTable(dplyr::mutate_if(as.data.frame(tables[[6]]),
                                                            is.numeric,
                                                            \(v) round(v, 3)),
                                           rnames = F)

        htmlTable::concatHtmlTables(tables  = c(level1, level2, random, correlations, ICC, diagnostics),
                                    headers = c("<b>Level 1</b>",
                                                level2_deps$headers,
                                                "<b>Variances of the Random Effects</b>",
                                                "<b>Correlation among Random Effects</b>",
                                                "<b>Intra-class Correlation Coefficient (ICC)</b>",
                                                "<b>Diagnostics</b>"))

}

