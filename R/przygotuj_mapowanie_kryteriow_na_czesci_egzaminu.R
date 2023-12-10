#' @importFrom dplyr %>% .data arrange collect distinct filter left_join mutate select
przygotuj_mapowanie_kryteriow_na_czesci_egzaminu = function(idSkali,
                                                            dodajRokDoNazwTematow = FALSE,
                                                            src = NULL) {
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

  if (dodajRokDoNazwTematow) {
    results = results %>%
      mutate(zmienna_temat = ifelse(is.na(.data$zmienna_temat),
                                    .data$zmienna_temat,
                                    paste0(.data$zmienna_temat, "_r", .data$rok)))
  }
  return(results)
}
