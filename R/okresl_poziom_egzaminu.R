#' @title Przetwarzanie danych z wynikami egzaminow
#' @description
#' W odniesieniu do wyników egzaminu maturalnego, tworzy zmienne opisujące
#' poziom(y) poszczególnych przedmiotów zdawane przez poszczególnych uczniów.
#' @inheritParams usun_wyniki_laureatow
#' @param zmienNazweGdyTylkoJednaCzesc wartość logiczna - p. sekcja *details*
#' @details
#' Jeśli przekazane dane obejmują kilka różnych przedmiotów (w sensie różnych
#' wartości kolumny `czesc_egzaminu` ramki danych przekazanej do funkcji
#' argumentem `czesciEgzaminow` - p. [przygotuj_mapowanie_kryteriow_na_czesci_egzaminu]),
#' a także zawsze wtedy, kiedy argument `zmienNazweGdyTylkoJednaCzesc=FALSE`,
#' nazwy zmiennych opisujących poziomy, na których poszczególni uczniowie
#' zdawali poszczególne przedmioty brane są z połączenia wartości kolumn
#' `czesc_egzaminu` i `poziom` ramki danych przekazanej do funkcji argumentem
#' `czesciEgzaminow` Jednak jeśli przekazane dane obejmują tylko jeden przedmiot
#' i `zmienNazweGdyTylkoJednaCzesc=TRUE` (wartość domyślna), nazwa kolumny
#' opisującej poziom zdawanego przedmiotu zostanie ustawiona na *poziom*.#'
#' @return ramka danych przekazana argumentem `dane`, z dodanymi zmiennymi
#' opisującymi poziom(y), na których poszczególni uczniowie zdawali poszczególne
#' przedmioty matury
#' @seealso [przygotuj_dane_do_skalowania()]
#' @importFrom dplyr %>% .data arrange distinct group_by inner_join left_join matches mutate select summarise
#' @importFrom tidyr pivot_longer pivot_wider
okresl_poziom_egzaminu = function(dane, czesciEgzaminow,
                                  zmienNazweGdyTylkoJednaCzesc = TRUE) {
  stopifnot(is.data.frame(dane),
            is.data.frame(czesciEgzaminow),
            is.logical(zmienNazweGdyTylkoJednaCzesc),
            length(zmienNazweGdyTylkoJednaCzesc) == 1,
            zmienNazweGdyTylkoJednaCzesc %in% c(TRUE, FALSE))
  stopifnot(all(c("id_obserwacji", "rok") %in% names(dane)),
            all(c("kryterium", "czesc_egzaminu", "prefiks") %in%
                  names(czesciEgzaminow)),
            all(grep("^[kp]_", names(dane), value = TRUE) %in%
                  czesciEgzaminow$kryterium))
  poziomy = dane %>%
    select("id_obserwacji", "rok", matches("^[kp]_")) %>%
    pivot_longer(matches("^[kp]_"), names_to = "kryterium",
                 values_to = "wynik", values_drop_na = TRUE) %>%
    inner_join(czesciEgzaminow %>%
                 mutate(poziom = sub("^.*_([pr])$", "\\1",
                                     .data$prefiks),
                        czesc_egzaminu = sub("_[pr]$", "",
                                             .data$prefiks)) %>%
                 select("kryterium", "czesc_egzaminu", "poziom"),
               by = "kryterium") %>%
    select("id_obserwacji", "rok", "czesc_egzaminu", "poziom") %>%
    distinct() %>%
    arrange(.data$id_obserwacji, .data$rok,
            .data$czesc_egzaminu, .data$poziom) %>%
    group_by(.data$id_obserwacji, .data$rok, .data$czesc_egzaminu) %>%
    summarise(poziom = paste(c("p" = "PP", "r" = "PR")[.data$poziom],
                             collapse = " i "),
              .groups = "drop") %>%
    pivot_wider(names_from = "czesc_egzaminu", values_from = "poziom")
  if (zmienNazweGdyTylkoJednaCzesc & ncol(poziomy) == 3) {
    names(poziomy)[3] = "poziom"
  }
  dane %>%
    left_join(poziomy,
              by = c("id_obserwacji", "rok")) %>%
    return()
}
