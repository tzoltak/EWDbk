#' @title Przetwarzanie danych z wynikami egzaminow
#' @description
#' Funkcja dodaje do danych z wynikami egzaminu zmienne opisujące wybór tematów
#' rozprawki (wypracowania).
#' @inheritParams usun_wyniki_laureatow
#' @param echo wartość logiczna - czy wyświetlić na konsoli tabelę z rozkładami
#' wyboru tematów wypracowania
#' @return ramka danych przekazana argumentem `dane`, z dodanymi (jeśli w danej
#' skali występuje rozprawka/rozprawki) zmiennymi opisującymi wybór tematów
#' rozprawek (wypracowań)
#' @seealso [przygotuj_zmienne_tematy_w_ramach_czesci_egzaminu()],
#' [przygotuj_dane_do_skalowania()]
#' @importFrom dplyr %>% .data all_of any_of distinct left_join select
#' @importFrom tidyr pivot_longer
dodaj_wybory_tematow = function(dane, czesciEgzaminow, echo = TRUE) {
  stopifnot(is.data.frame(dane),
            is.data.frame(czesciEgzaminow),
            is.logical(echo), length(echo) == 1, echo %in% c(TRUE, FALSE))
  stopifnot(all(c("id_obserwacji", "rok") %in% names(dane)),
            all(c("kryterium", "czesc_egzaminu", "rok", "czy_sf", "temat",
                  "zmienna_temat") %in% names(czesciEgzaminow)),
            all(grep("^[kp]_", names(dane), value = TRUE) %in%
                  czesciEgzaminow$kryterium))
  kryteriaRozprawki = filter(czesciEgzaminow, !is.na(.data$temat))
  if (nrow(kryteriaRozprawki) == 0) return(dane)
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
#' @title Przetwarzanie danych z wynikami egzaminow
#' @description
#' *Koń roboczy* [dodaj_wybory_tematow()].
#' @param x ramka danych zawierające wyniki z pojedynczej części egzaminu
#' (z pojedynczego rok, w konkretnej - *starej*/*nowej* formule)
#' @inheritParams dodaj_wybory_tematow
#' @return ramka danych przekazana argumentem `x` z dodaną (jeśli w danej części
#' egzaminu występuje rozprawka) zmienną opisującą wybór tematu rozprawki
#' (wypracowania)
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
