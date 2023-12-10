#' @importFrom dplyr %>% .data all_of any_of distinct left_join select
#' @importFrom tidyr pivot_longer
dodaj_wybory_tematow <- function(dane, czesciEgzaminow, echo = TRUE) {
  kryteriaRozprawki = filter(czesciEgzaminow, !is.na(.data$temat))
  zdajacyTematy = dane %>%
    select("id_obserwacji", all_of(kryteriaRozprawki$kryterium)) %>%
    pivot_longer(-"id_obserwacji", names_to = "kryterium",
                 values_to = "punktacja", values_drop_na = TRUE) %>%
    left_join(kryteriaRozprawki %>%
                select("kryterium", "czesc_egzaminu", "rok", "czy_sf", "temat",
                       "zmienna_temat"),
              by = "kryterium") %>%
    select("id_obserwacji", "czesc_egzaminu", "rok", "czy_sf", "temat",
           "zmienna_temat") %>%
    distinct()
  # aby w podziale na części egzaminu utworzyć zmienne opisujące wybór tematów
  zdajacyTematy = zdajacyTematy %>%
    split(zdajacyTematy %>%
            select(any_of(c("czesc_egzaminu", "rok", "czy_sf"))) %>%
            as.list()) %>%
    lapply(przygotuj_zmienne_tematy_w_ramach_czesci_egzaminu, echo = echo)
  # i przyłączyć je do do danych
  for (i in seq_along(zdajacyTematy)) {
    dane = dane %>%
      left_join(zdajacyTematy[[i]],
                by = c("id_obserwacji", "rok"))
  }
  return(dane)
}
#' @importFrom dplyr %>% .data all_of arrange count desc left_join mutate select
#' @importFrom tidyr pivot_wider
przygotuj_zmienne_tematy_w_ramach_czesci_egzaminu = function(x, echo = TRUE) {
  zadaniaTematy = x %>%
    count(.data$czesc_egzaminu, .data$czy_sf, .data$temat, .data$zmienna_temat) %>%
    arrange(desc(.data$n))
  x = x %>%
    mutate(wybor = 1L) %>%
    left_join(zadaniaTematy %>% select("zmienna_temat", "n"),
              by = "zmienna_temat") %>%
    arrange(.data$czesc_egzaminu, desc(.data$n)) %>%
    select("id_obserwacji", "rok", "zmienna_temat", "wybor") %>%
    pivot_wider(names_from = "zmienna_temat",
                values_from = "wybor",
                values_fill = 0L) %>%
    select(-all_of(zadaniaTematy$zmienna_temat[1]))
  if (echo) {
    message("W części egzaminu '", zadaniaTematy$czesc_egzaminu[1],
            "' utworzono ", nrow(zadaniaTematy) - 1, " zmienne/ych ",
            "opisujące/ych wybór tematu wypracowania.\n",
            "Rozkład wybieralności tematów w danych:")
    zadaniaTematy %>%
      mutate("%" = paste0(format(100 * .data$n / sum(.data$n),
                                 digits = 1, nsmall = 1),
                          "%"),
             n = format(.data$n, big.mark = "'")) %>%
      select("temat", "n", "%") %>%
      as.data.frame(check.names = FALSE) %>%
      print()
  }
  return(x)
}
