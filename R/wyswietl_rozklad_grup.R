#' @title Przetwarzanie danych z wynikami egzaminow
#' @description
#' Drukuje na konsolę rozkład przypisania uczniów do grup wyróżnianych
#' w procesie skalowania.
#' @param dane ramka danych z wynikami egzaminu - musi zawierać kolumnę *grupa*
#' @return ramka danych z rozkładem
#' @seealso [okresl_grupe()], [przygotuj_dane_do_ewd_bk()]
#' @importFrom dplyr %>% .data count mutate select
wyswietl_rozklad_grup = function(dane) {
  stopifnot(is.data.frame(dane), "grupa" %in% names(dane))
  dane %>%
    count(.data$grupa, name = "n") %>%
    mutate("%" = paste0(format(100 * .data$n / sum(.data$n),
                               digits = 1, nsmall = 1),
                        "%"),
           n = format(.data$n, big.mark = "'")) %>%
    select("grupa", "n", "%") %>%
    as.data.frame(check.names = FALSE) %>%
    print()
}
