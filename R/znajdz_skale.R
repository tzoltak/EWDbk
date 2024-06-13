#' @importFrom dplyr %>% .data collect filter group_by summarise
znajdz_skale = function(skale, doPrezentacji = NA, skalowanie = -1L, src = NULL) {
  stopifnot(is.numeric(skale) | is.character(skale), length(skale) > 0,
            is.logical(doPrezentacji), length(doPrezentacji) == 1,
            is.numeric(skalowanie), length(skalowanie) == 1, skalowanie > 0 || skalowanie == -1L,
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
