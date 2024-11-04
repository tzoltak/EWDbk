#' @title Przygotowywanie danych do obliczenia wskaznikow EWD
#' @description
#' Dla podanych skal pobiera z bazy zestawienie wartości parametrów modeli
#' skalowania i ich kowariancji.
#' @param skale ramka danych o jednym wierszu zawierająca kolumny
#' *id_skali_matura*, *skalowanie_matura*, *id_skali_we*, *skalowanie_we*
#' (pojedynczy wiersz połączonych ze sobą i przekształconych wywołań
#' [znajdz_skale()] dla skal matury i skal egzaminu na wejściu)
#' @inheritParams oblicz_ewd_bk
#' @return lista ramek danych z elementami: *parametryMatura*,
#' *kowariancjeMatura*, *tematyLaureatowMatura*, *parametryWejscie*,
#' *kowariancjeWejscie*, *tematyLaureatowWejscie*
#' @seealso [ZPD::pobierz_parametry()],
#' [ZPD::pobierz_skalowania_elementy_kowariancje()],
#' [wybierz_tematy_dla_laureatow()] [oblicz_ewd_bk()]
#' @importFrom dplyr %>% .data collect filter
pobierz_parametry_egzaminow = function(skale, src) {
  stopifnot(is.data.frame(skale), nrow(skale) == 1,
            "id_skali_matura" %in% names(skale),
            "skalowanie_matura" %in% names(skale),
            "id_skali_we" %in% names(skale),
            "skalowanie_we" %in% names(skale),
            is.numeric(skale$id_skali_matura), !is.na(skale$id_skali_matura),
            is.numeric(skale$skalowanie_matura), !is.na(skale$skalowanie_matura),
            is.numeric(skale$id_skali_we), !is.na(skale$id_skali_we),
            is.numeric(skale$skalowanie_we), !is.na(skale$skalowanie_we))
  if (is.null(src)) {
    src = ZPD::polacz()
    on.exit(ZPD::rozlacz(src))
  }

  parametryMatura = ZPD::pobierz_parametry(src) %>%
    filter(.data$id_skali %in% local(skale$id_skali_matura),
           .data$skalowanie %in% local(skale$skalowanie_matura)) %>%
    collect()
  stopifnot("id_elementu" %in% names(parametryMatura))
  kowariancjeMatura = ZPD::pobierz_skalowania_elementy_kowariancje(src) %>%
    filter(.data$id_elementu1 %in% local(parametryMatura$id_elementu) |
             .data$id_elementu2 %in% local(parametryMatura$id_elementu)) %>%
    collect()
  tematyLaureatowMatura = wybierz_tematy_dla_laureatow(parametryMatura)

  parametryWejscie = ZPD::pobierz_parametry(src) %>%
    filter(.data$id_skali %in% local(skale$id_skali_we),
           .data$skalowanie %in% local(skale$skalowanie_we)) %>%
    collect()
  stopifnot("id_elementu" %in% names(parametryWejscie))
  kowariancjeWejscie = ZPD::pobierz_skalowania_elementy_kowariancje(src) %>%
    filter(.data$id_elementu1 %in% local(parametryWejscie$id_elementu) |
             .data$id_elementu2 %in% local(parametryWejscie$id_elementu)) %>%
    collect()
  tematyLaureatowWejscie = wybierz_tematy_dla_laureatow(parametryWejscie)

  stopifnot(nrow(parametryMatura) > 0L,
            nrow(parametryWejscie) > 0L)
  return(list(parametryMatura = parametryMatura,
              kowariancjeMatura = kowariancjeMatura,
              tematyLaureatowMatura = tematyLaureatowMatura,
              parametryWejscie = parametryWejscie,
              kowariancjeWejscie = kowariancjeWejscie,
              tematyLaureatowWejscie = tematyLaureatowWejscie))
}
