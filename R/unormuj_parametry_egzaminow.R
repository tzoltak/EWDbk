#' @title Przygotowywanie danych do obliczenia wskaznikow EWD
#' @description
#' Unormowuje parametry egzaminów w taki sposób, aby w populacji objętych
#' obliczaniem wskaźników EWD rozkład umiejętności zarówno na wejściu jak i na
#' wyjściu miał wartość oczekiwaną 0 i odchylenie standardowe 1.
#' @inheritParams estymuj_pvreg
#' @param parametry lista ramek danych zwrócona przez
#' [pobierz_parametry_egzaminow()]
#' @return lista czterech ramek danych z elementami *parametryMatura*,
#' *kowariancjeMatura*, *parametryWejscie* i *kowariancjeWejscie*
#' @seealso [unormuj_parametry_egzaminu()], [oblicz_ewd_bk()]
unormuj_parametry_egzaminow = function(parametry, dane) {
  parametryMatura =
    unormuj_parametry_egzaminu(parametry = parametry$parametryMatura,
                               kowariancje = parametry$kowariancjeMatura,
                               dane = dane$matura)
  parametryWejscie =
    unormuj_parametry_egzaminu(parametry = parametry$parametryWejscie,
                               kowariancje = parametry$kowariancjeWejscie,
                               dane = dane$wejscie)
  return(list(parametryMatura = parametryMatura$parametry,
              kowariancjeMatura = parametryMatura$kowariancje,
              parametryWejscie = parametryWejscie$parametry,
              kowariancjeWejscie = parametryWejscie$kowariancje))
}
#' @title Przygotowywanie danych do obliczenia wskaznikow EWD
#' @description
#' *Koń roboczy* [unormuj_parametry_egzaminow]. Unormowuje parametry danego
#' egzaminu w taki sposób, aby w populacji objętych obliczaniem wskaźników EWD
#' rozkład umiejętności  miał wartość oczekiwaną 0 i odchylenie standardowe 1.
#' @param parametry ramka danych z wartościami parametrów modelu skalowania
#' egzaminu
#' @param kowariancje ramka danych z kowariancjami parametrów modelu skalowania
#' egzaminu
#' @param dane ramka danych z wynikami egzaminu (używana do obliczenia rozkładu
#' częstości grup w populacji objętych obliczaniem wskaźników EWD)
#' @return lista dwóch ramek danych z elementami *parametry* i *kowariancje*
#' @importFrom dplyr %>% %>% .data case_when count filter inner_join mutate pull summarise
unormuj_parametry_egzaminu = function(parametry, kowariancje, dane) {
  czestosciGrup = dane %>%
    count(.data$grupa, name = "w") %>%
    mutate(w = .data$w / sum(.data$w))
  parametryGrup = parametry %>%
    filter(!is.na(.data$grupa),
           .data$parametr %in% c("group_mean", "group_sd")) %>%
    inner_join(czestosciGrup,
               by = "grupa")

  srCalosc = parametryGrup %>%
    filter(.data$parametr %in% "group_mean") %>%
    summarise(wartosc = sum(.data$wartosc * .data$w)) %>%
    pull("wartosc")
  sredniaWariancjiCalosc = parametryGrup %>%
    filter(.data$parametr %in% "group_sd") %>%
    summarise(wartosc = sum(.data$wartosc^2 * .data$w)) %>%
    pull("wartosc")
  wariancjaSrednichCalosc = parametryGrup %>%
    filter(.data$parametr %in% "group_mean") %>%
    summarise(wartosc = sum((.data$wartosc - srCalosc)^2 * .data$w)) %>%
    pull("wartosc")
  odchStdCalosc = (sredniaWariancjiCalosc + wariancjaSrednichCalosc)^0.5

  parametry = parametry %>%
    filter(.data$parametr %in% c("group_mean", "group_sd", "a", "c") |
             grepl("^b(|[[:digit:]]+)", .data$parametr),
           .data$grupa %in% c(parametryGrup$grupa, NA_character_)) %>%
    mutate(wartosc =
             case_when(.data$parametr == "group_mean" ~
                         (.data$wartosc - srCalosc) / odchStdCalosc,
                       .data$parametr == "group_sd" ~
                         .data$wartosc / odchStdCalosc,
                       .data$parametr == "a" ~ .data$wartosc * odchStdCalosc,
                       grepl("^b(|[[:digit:]]+)", .data$parametr) ~
                         (.data$wartosc - srCalosc) / odchStdCalosc,
                       .data$parametr == "c" ~ .data$wartosc),
           bs = .data$bs / odchStdCalosc)
  kowariancje = kowariancje %>%
    filter(.data$id_elementu1 %in% parametry$id_elementu,
           .data$id_elementu2 %in% parametry$id_elementu) %>%
    mutate(kowariancja = .data$kowariancja / odchStdCalosc^2)

  return(list(parametry = parametry,
              kowariancje = kowariancje))
}
