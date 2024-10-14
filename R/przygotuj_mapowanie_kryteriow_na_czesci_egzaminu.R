#' @title Przetwarzanie danych z wynikami egzaminow
#' @description
#' Funkcja na podstawie informacji z bazy danych zwraca (pseudo)kryteria oceny
#' skali o podanym id z dopisanymi informacjami o tym, z jakiej części egzaminu
#' i z jakiego roku pochodzą, prefiks związany z tą częścią egzaminu, numer
#' pytania, typ pytania oraz, w przypadku kryteriów oceny wypracowania, którego
#' tematu dane kryterium dotyczy (a także zmienną rozróżniającą części egzaminu
#' ze *starej* i *nowej* formuły matury, w sensie zmian z lat 2023/2034).
#' @param idSkali liczba (całkowita) - id skali w bazie danych
#' @param dodajRokDoNazwTematow wartość logiczna - czy do tworzonych
#' nazw zmiennych opisujących wybór tematów ma zostać dopisany rok
#' przeprowadzenia egzaminu? można podać wartość `NA`, która oznacza, że rok
#' zostanie dodany tylko w sytuacji, gdy kryteria danej skali pochodzą
#' z egzaminów przeprowadzonych w kilku różnych latach
#' @inheritParams skaluj_matura_bk
#' @seealso [ZPD::pobierz_kryteria_oceny()], [ZPD::pobierz_testy()],
#' [przygotuj_dane_do_skalowania()], [wybierz_tematy_dla_laureatow()]
#' @return ramka danych, w której wiersze stanowią (pseudo)kryteria oceny
#' @importFrom dplyr %>% .data arrange collect distinct filter left_join mutate select
przygotuj_mapowanie_kryteriow_na_czesci_egzaminu = function(idSkali,
                                                            dodajRokDoNazwTematow = FALSE,
                                                            src = NULL) {
  stopifnot(is.numeric(idSkali), length(idSkali) == 1, !anyNA(idSkali),
            as.integer(idSkali) == idSkali,
            is.logical(dodajRokDoNazwTematow),
            length(dodajRokDoNazwTematow) == 1)
  if (is.null(src)) {
    src = ZPD::polacz()
    on.exit(ZPD::rozlacz(src))
  }
  mapowanieNr = c("0" = 0, "I" = 1, "II" = 2, "III" = 3, "IV" = 4,
                  "V" = 5, "VI" = 6, "VII" = 7, "VIII" = 8, "IX" = 9)
  results = suppressMessages(ZPD::pobierz_kryteria_oceny(src, skale = TRUE)) %>%
    filter(.data$id_skali == idSkali) %>%
    select("id_skali", kolejnosc = "kolejnosc_w_skali", "kryterium", "id_testu",
           "typ_pytania", "numer_pytania", "numer_kryterium") %>%
    left_join(ZPD::pobierz_testy(src),
              by = "id_testu") %>%
    filter(.data$czy_egzamin) %>%
    select("id_skali", "rok", "kolejnosc", "kryterium", "czesc_egzaminu",
           "typ_pytania", "prefiks", "arkusz", "numer_pytania",
           "numer_kryterium") %>%
    collect() %>%
    mutate(czy_sf = !(substr(.data$arkusz, 7, 7) %in% c("X", "Y", "Z")),
           # uwaga, ew. grzebanie tutaj wymaga uzgodnienia kodu w wybierz_temat_dla_lauratow()
           sufiksNF = ifelse(all(c(FALSE, TRUE) %in% .data$czy_sf), "nf", "")) %>%
    select(-"arkusz") %>%
    distinct() %>%
    mutate(czyTemat = gsub("^(0|I|II|III|IV|V|VI|VII|VIII|IX)_.*$", "\\1",
                           .data$numer_pytania) %in% names(mapowanieNr),
           numer_pytania = ifelse(.data$czyTemat,
                                  sub("^0*_", "0_", .data$numer_pytania),
                                  as.character(.data$numer_pytania)),
           temat = ifelse(.data$czyTemat,
                          gsub("^(0|I|II|III|IV|V|VI|VII|VIII|IX)_.*$", "\\1",
                               .data$numer_pytania),
                          NA_character_),
           zmienna_temat = ifelse(.data$czyTemat,
                                  paste0("t", mapowanieNr[.data$temat],
                                         ifelse(.data$czy_sf, "", .data$sufiksNF),
                                         "_", sub("^m_", "", .data$prefiks)),
                                  NA_character_)) %>%
    select(-"czyTemat", -"sufiksNF") %>%
    arrange(.data$kolejnosc)
  if (nrow(results) == 0) warning("W bazie nie ma skali o podanym id lub nie są do niej przypisane żadne (psudo)kryteria oceny.")

  if (is.na(dodajRokDoNazwTematow)) {
    dodajRokDoNazwTematow = length(unique(results$rok)) > 1
  }
  if (dodajRokDoNazwTematow) {
    results = results %>%
      mutate(zmienna_temat = ifelse(is.na(.data$zmienna_temat),
                                    .data$zmienna_temat,
                                    paste0(.data$zmienna_temat, "_r", .data$rok)))
  }
  return(results)
}
