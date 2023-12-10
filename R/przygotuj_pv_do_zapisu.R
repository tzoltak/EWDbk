#' @importFrom dplyr %>% .data bind_rows filter left_join mutate rename select
przygotuj_pv_do_zapisu = function(pv, skale, parametry, czesciEgzaminu, grupy,
                                  nazwaWskaznika, rokEWD) {
  stopifnot(is.data.frame(skale), nrow(skale) == 1)

  wyniki = list(skalowania = data.frame(id_skali = skale$id_skali,
                                        skalowanie = skale$skalowanie_zapisz,
                                        opis = paste0(skale$opis_skali,
                                                      ";pvreg;",
                                                      nazwaWskaznika, rokEWD),
                                        estymacja = "MML (UIRT)/pvreg",
                                        do_prezentacji = FALSE,
                                        data = Sys.Date()),
                skalowania_grupy = data.frame(id_skali = skale$id_skali,
                                              skalowanie = skale$skalowanie_zapisz,
                                              grupa = grupy),
                skalowania_elementy = parametry$parametry %>%
                  filter(is.na(.data$grupa) |
                           .data$grupa %in% grupy) %>%
                  mutate(skalowanie = skale$skalowanie_zapisz) %>%
                  mutate(idTemp = .data$id_elementu,
                         id_elementu = NA_integer_,
                         grupowy = !is.na(.data$grupa)) %>%
                  left_join(czesciEgzaminu %>%
                              select("id_skali", "kryterium", "kolejnosc"),
                            by = c("id_skali", "kryterium")) %>%
                  select("id_skali", "kolejnosc", "skalowanie", "parametr",
                         "model", "wartosc", uwagi = "parametr_uwagi", "bs",
                         "id_elementu", "grupowy", "grupa", "idTemp"),
                skalowania_elementy_kowariancje =
                  parametry$kowariancje %>%
                  rename(idTemp = "id_elementu1",
                         idTemp2 = "id_elementu2"),
                skalowania_obserwacje = pv %>%
                  select(-"id_szkoly") %>%
                  mutate(id_skali = skale$id_skali,
                         skalowanie = skale$skalowanie_zapisz) %>%
                  select("id_skali", "skalowanie", "id_obserwacji", "id_testu",
                         "estymacja", "nr_pv", "wynik", "bs", "grupa"),
                usunieteKryteria = NULL)
  class(wyniki) = c("wynikiSkalowania", class(wyniki))
  return(wyniki)
}
