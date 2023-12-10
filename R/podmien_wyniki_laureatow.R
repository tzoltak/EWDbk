#' @importFrom dplyr %>% .data arrange count desc distinct group_by left_join mutate select
#' @importFrom tidyr pivot_longer
# Jeśli dla danej części egzaminu (w której były tematy) nie ma wskazanej nazwy
# zmiennej kodującej wybór tematu, który należy przypisać laureatom, to
# przypisuje im maksymalne wyniki z najczęściej wybieranego tematu spośród tych,
# dla których w modelu skalowania nie ma zmiennych opisujących ich wybór.
podmien_wyniki_laureatow = function(dane, tematyLaureatow, zmienneTematy) {
  if (nrow(tematyLaureatow) == 0) {
    return(dane)
  }
  czesciEgzaminu = attributes(dane)$czesciEgzaminu
  kryteriaRozprawki = filter(czesciEgzaminu, !is.na(.data$temat))
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
  zmienneTematy = zdajacyTematy %>%
    count(.data$czesc_egzaminu, .data$czy_sf, .data$zmienna_temat, name = "freq") %>%
    arrange(.data$czesc_egzaminu, .data$czy_sf, desc(.data$freq)) %>%
    group_by(.data$czesc_egzaminu, .data$czy_sf) %>%
    mutate(freq = 1L:n()) %>%
    left_join(tematyLaureatow,
              by = c("czesc_egzaminu", "czy_sf", "zmienna_temat")) %>%
    left_join(tematyLaureatow %>%
                select(-"czesc_egzaminu") %>%
                mutate(dla_laur = TRUE),
              by = c("zmienna_temat", "czy_sf")) %>%
    group_by(.data$czesc_egzaminu, .data$czy_sf) %>%
    mutate(dla_laur =
             ifelse(rep(any(.data$dla_laur %in% TRUE), n()),
                    ifelse(is.na(.data$dla_laur), FALSE, .data$dla_laur),
                    ifelse(
                      .data$freq == min(.data$freq[!(.data$zmienna_temat %in% zmienneTematy)]),
                      TRUE, FALSE))) %>%
    left_join(kryteriaRozprawki %>%
                select("prefiks", "zmienna_temat") %>%
                distinct(),
              by = "zmienna_temat")
  kryteriaRozprawki = kryteriaRozprawki %>%
    left_join(zmienneTematy,
              by = c("czesc_egzaminu", "czy_sf", "zmienna_temat"))
  # jak już wiemy, co chcemy zrobić z którymi kryteriami, to trzeba to zrobić
  for (k in 1:nrow(kryteriaRozprawki)) {
    kryterium = kryteriaRozprawki$kryterium[k]
    laureaci = paste0("laur_", kryteriaRozprawki$prefiks[k])
    dane[[kryterium]][dane[[laureaci]]] =
      ifelse(kryteriaRozprawki$dla_laur[k],
             max(dane[[kryterium]], na.rm = TRUE),
             NA_real_)
  }
  # tak samo z tematami
  for (z in 1:nrow(zmienneTematy)) {
    kryterium = zmienneTematy$zmienna_temat[z]
    if (kryterium %in% names(dane)) {
      laureaci = paste0("laur_", zmienneTematy$prefiks[z])
      dane[[kryterium]][dane[[laureaci]]] =
        ifelse(zmienneTematy$dla_laur[z], 1L, 0L)
    }
  }
  # koniec
  return(dane)
}
