#' @title Przygotowywanie danych do obliczenia wskaznikow EWD
#' @description
#' Określa, którym ze zmiennych opisujących wybór tematów rozprawki należy
#' ustawić wartość 1 dla laureatów w poszczególnych częściach egzaminu (tak aby
#' skutkowało to maksymalizacją oszacowań umiejętności laureatów).
#' @param parametry ramka danych z wartościami parametrów modelu, zwrócona przez
#' [ZPD::pobierz_parametry()]
#' @details
#' Funkcja robi najlepsze, co może, mając dostęp tylko do parametrów modelu.
#' Zwraca ramkę danych o 0 wierszach, jeśli w parametrach nie widzi żadnych
#' tematów, ale może też zwrócić pusty ciąg znaków w kolumnie `temat`, jeśli
#' w ramach danej części egzaminu należy wybrać "temat odniesienia" (a więc ten,
#' który "nie ma swojej zmiennej") lub temat, którego "zmienna selekcyjna"
#' została usunięta z modelu skalowania.
#' @return ramka danych z przypisaniem nazwy zmiennej opisującej temat
#' rozprawki, której wartość należy laureatom ustawić na 1, do części egzaminów
#' (p. sekcja *details* w celu zapoznania się ze sposobem obsługi szczególnych
#' przypadków)
#' @seealso [pobierz_parametry_egzaminow()],
#' [przygotuj_mapowanie_kryteriow_na_czesci_egzaminu()]
#' @importFrom dplyr %>% .data arrange desc filter group_by mutate select slice ungroup
#' @importFrom tidyr pivot_wider
wybierz_tematy_dla_laureatow = function(parametry) {
  stopifnot(is.data.frame(parametry),
            "parametr_uwagi" %in% names(parametry),
            "parametr" %in% names(parametry),
            "wartosc" %in% names(parametry))
  parametry = parametry %>%
    filter(grepl("^t[[:digit:]]+_", .data$parametr_uwagi)) %>%
    select(zmienna_temat = "parametr_uwagi", "parametr", "wartosc")
  if (nrow(parametry) == 0) {
    parametry %>%
      mutate(czesc_egzaminu = vector(mode = "character", length = 0),
             czy_sf = vector(mode = "logical", length = 0)) %>%
      select("czesc_egzaminu", "czy_sf", "zmienna_temat") %>%
      return()
  } else {
    parametry %>%
      pivot_wider(names_from = "parametr", values_from = "wartosc") %>%
      mutate(czesc_egzaminu = sub("^t[[:digit:]]_", "", .data$zmienna_temat),
             # uwaga, ten kod zależy od zachowania przygotuj_mapowanie_na_czesci_egzaminu()
             czy_sf = !grepl("^t[[:digit:]]+nf_.*$", .data$zmienna_temat)) %>%
      arrange(desc(.data$a), .data$b) %>%
      group_by(.data$czesc_egzaminu, .data$czy_sf) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(zmienna_temat = ifelse(.data$a > 0, .data$zmienna_temat, "")) %>%
      select("czesc_egzaminu", "czy_sf", "zmienna_temat") %>%
      return()
  }
}
