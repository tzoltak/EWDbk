#' @title Przetwarzanie danych z wynikami egzaminow
#' @description
#' Funkcja pozwala usunąć z danych wyniki laureatów - wszystkie albo tylko
#' w (pseudo)kryteriach oceny odnoszących się do rozprawki (wypracowania)
#' @param dane ramka danych z wynikami egzaminu
#' @param czesciEgzaminow ramka danych z charakterystykami (pseudo)kryteriów
#' oceny skali (występujących w wynikach przekazanych argumentem `dane`),
#' zwrócona przez [przygotuj_mapowanie_kryteriow_na_czesci_egzaminu())]
#' @param tylkoRozprawki wartość logiczna - czy usunąć tylko wyniki
#' (pseud)kryteriów oceny związanych z rozprawką (wypracowaniem)?
#' @param usunPusteWiersze wartość logiczna - czy usunąć z `dane` wiersze,
#' które po usunięciu wyników zawierają same braki danych?
#' @details
#' (Pseudo)kryteria oceny rozprawek (wypracowań) są w kontekście laureatów
#' szczególnie problematyczne, gdyż dla laureatów wybór tematu jest
#' nieokreślony.
#' @return ramka danych przekazana argumentem `dane`, z niektórymi wartościami
#' zamienionymi na braki danych i ew., jeśli `usunPusteWiersze=TRUE`, usuniętymi
#' niektórymi wierszami
#' @seealso [przygotuj_dane_do_skalowania()]
#' @importFrom dplyr .data across all_of cur_data filter matches mutate select
usun_wyniki_laureatow = function(dane, czesciEgzaminow, tylkoRozprawki = FALSE,
                                 usunPusteWiersze = TRUE) {
  stopifnot(is.data.frame(dane),
            is.data.frame(czesciEgzaminow),
            is.logical(tylkoRozprawki), length(tylkoRozprawki) == 1,
            tylkoRozprawki %in% c(TRUE, FALSE),
            is.logical(usunPusteWiersze), length(usunPusteWiersze) == 1,
            usunPusteWiersze %in% c(TRUE, FALSE))
  stopifnot(all(c("kryterium", "temat", "prefiks") %in% names(czesciEgzaminow)),
            all(grep("^[kp]_", names(dane), value = TRUE) %in%
                  czesciEgzaminow$kryterium))
  zmienneLaureaci =
    data.frame(zmienna = grep("^laur_", names(dane), value = TRUE)) %>%
    mutate(prefiks = sub("^laur_", "", .data$zmienna))
  if (tylkoRozprawki) {
    zmienneLaureaci = zmienneLaureaci %>%
      filter(.data$prefiks %in% filter(czesciEgzaminow, !is.na(.data$temat))$prefiks)
  } else {
    zmienneLaureaci = zmienneLaureaci %>%
      filter(.data$prefiks %in% czesciEgzaminow$prefiks)
  }
  for (i in 1:nrow(zmienneLaureaci)) {
    dane = dane %>%
      mutate(across(all_of(filter(czesciEgzaminow,
                                  .data$prefiks == zmienneLaureaci$prefiks[i])$kryterium),
                    ~ifelse(.data[[zmienneLaureaci$zmienna[i]]],
                            NA_real_, .)))
  }
  if (usunPusteWiersze) {
    dane = dane %>%
      filter(rowSums(!is.na(select(cur_data(), matches("^[kp]_")))) > 0)
  }
  return(dane)
}
