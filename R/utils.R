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

prep_table = function(t, letter, greek_letter){
        t = dplyr::mutate_if(.tbl = t, .predicate = is.numeric, .funs = \(x) round(x, 3))
        t = dplyr::mutate(.data = t,
                          p.value = ifelse(p.value == 0, "< 0.001", p.value),
                          par = paste0(stringr::str_replace(par,
                                                            paste0("^", letter),
                                                            paste0("\\&", greek_letter, ";<sub>")),
                                       "</sub>")
        )
        t

        t = htmlTable::htmlTable(x = t, rnames = F)
        t
}
