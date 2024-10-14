#' @title Wyszukiwanie skal
#' @description
#' Funkcja wyszukuje skale na podstawie porównując ich opisy do podanego
#' wyrażenia regularnego (lub znajduje skale o podanych *id_skali*) i zwraca
#' informacje o największych numerach skalowania powiązanych z wyszukanymi
#' skalami.
#' @param skale wektor ciągów znaków z wyrażeniami regularnymi
#' identyfikującymi skale (po kolumnie `opis` w bazie danych) lub wektor
#' liczbowy z `id_skali` - wskazuje skale, które mają zostać wykorzystane do
#' skalowania; domyślna wartość powinna działać poprawnie (tworząc wyrażenie
#' regularne na podstawie wartości argumentu `rokEWD`) w latach nie
#' zahaczających (zakresem danych) o zmiany formuły egzaminów
#' @param doPrezentacji wartość logiczna (domyślnie `NA``) - czy szukać tylko
#' skal oznaczonych w bazie danych jako przeznaczone do prezentacji? domyślna
#' wartość `NA` wskazuje, aby szukać wszystkich skal, bez względu na to, czy
#' są one oznaczone jako przeznaczone do prezentacji, czy nie
#' @param skalowanie numer skalowania (w ramach każdej ze skal
#' zidentyfikowanych na podstawie argumentu `skale`), parametry z którego
#' mają zostać wykorzystane do skalowania wyników egzaminu;
#' domyślna wartość -1 oznacza wybór (oddzielnie w ramach każdej skali)
#' największego numeru skalowania spośród tych, które nie opisują wyników
#' działania *pvreg* (tzn. pochodzą ze skalowania wyników egzaminu *uirt*-em,
#' a nie powstały w wyniku obliczania latentnych wskaźników EWD)
#' @inheritParams skaluj_matura_bk
#' @return ramka danych zawierająca id i opisy skal oraz informacje
#' o największym numerze skalowania przypisanym do skali i największym numerze
#' skalowania nie będącym skalowaniem związanym z estymacją EWD przy pomocy
#' *pvreg* przypisanym do bazy
#' @seealso [skaluj_matura_bk()], [skaluj_we_bk()], [oblicz_ewd_bk()]
#' @importFrom dplyr %>% .data collect filter group_by summarise
znajdz_skale = function(skale, doPrezentacji = NA, skalowanie = -1L, src = NULL) {
  stopifnot(is.numeric(skale) | is.character(skale), length(skale) > 0,
            is.logical(doPrezentacji), length(doPrezentacji) == 1,
            is.numeric(skalowanie), length(skalowanie) == 1,
            skalowanie > 0 || skalowanie == -1L,
            dplyr::is.src(src) | is.null(src))
  stopifnot(as.integer(skalowanie) == skalowanie)
  nrSkalowania = skalowanie
  if (is.null(src)) {
    src = ZPD::polacz()
    on.exit(ZPD::rozlacz(src))
  }

  if (is.numeric(skale)) {
    skale = ZPD::pobierz_skale(src, doPrezentacji = doPrezentacji,
                               skalowania = TRUE, czyKtt = FALSE) %>%
      filter(.data$id_skali %in% skale) %>%
      collect() %>%
      group_by(.data$id_skali, .data$opis_skali, .data$rodzaj_skali,
               .data$skala_do_prezentacji, .data$rodzaj_egzaminu, .data$rok)
  } else {
    skale = ZPD::pobierz_skale(src, doPrezentacji = doPrezentacji,
                               skalowania = TRUE, czyKtt = FALSE) %>%
      group_by(.data$id_skali, .data$opis_skali, .data$rodzaj_skali,
               .data$skala_do_prezentacji, .data$rodzaj_egzaminu, .data$rok) %>%
      collect() %>%
      filter(grepl(skale, .data$opis_skali))
  }
  skale %>%
    summarise(max_skalowanie = max(.data$skalowanie),
              max_skalowanie_bez_pvreg =
                suppressWarnings(
                  max(.data$skalowanie[!grepl(";pvreg;", .data$opis_skalowania)])),
              skalowanie = ifelse(nrSkalowania %in% .data$skalowanie,
                                  nrSkalowania, NA_integer_),
              .groups = "drop") %>%
    return()
}
