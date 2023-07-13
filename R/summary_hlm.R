#' Tidy HLM Summary for a lmer object
#'
#' @param model A model estimated with lmer (package lme4), using a HLM formula converted using formula_hlm_to_lmer.
#' @export
summary_hlm = function(model){

        tables = tidy_hlm(model)

        level1 = prep_table(tables[[1]], letter = "b", greek_letter = "beta")
        level2 = lapply(tables[[2]], prep_table, letter = "g", greek_letter = "gamma")

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

        htmlTable::concatHtmlTables(tables  = c(level1, level2, random, correlations, ICC, diagnostics))

}

