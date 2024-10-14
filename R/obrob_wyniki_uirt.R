#' @title Skalowanie wynikow egzaminow w Stacie przy pomocy pakietu uirt
#' @description
#' Przetwarza wyniki skalowania z formy zapisanej ze Staty do kompatybilnej
#' ze strukturami danych, w jakich są one zapisywane w bazie danych.
#' @param parametry ramka danych z wyestymowanymi parametrami modelu
#' (wczytanymi wprost z pliku CSV zapisanego przez Statę)
#' @inheritParams skaluj_uirt
#' @return lista z wynikami skalowania zawierająca następujące elementy:
#' *grupy* (ramka danych), *parametry* (ramka danych), *kowariancje*
#' (ramka danych)
#' @seealso [skaluj_uirt()]
#' @importFrom dplyr %>% .data count filter left_join mutate select
#' @importFrom tidyr pivot_longer pivot_wider
obrob_wyniki_uirt = function(parametry, dane) {
  grupy = parametry %>%
    filter(grepl("^grupa_", .data$var)) %>%
    select(nrGrupy = "var", "par", "est") %>%
    mutate(par = sub("_theta$", "", .data$par)) %>%
    pivot_wider(names_from = "par", values_from = "est") %>%
    left_join(parametry %>%
                filter(grepl("^grupa_", .data$var),
                       .data$par == "mean_theta") %>%
                select(nrGrupy = "var", N = "groupN_itemCATS"),
              by = "nrGrupy") %>%
    mutate(nrGrupy = as.integer(sub("^grupa_", "", .data$nrGrupy))) %>%
    left_join(dane %>% count(.data$grupa, name = "N"),
              by = "N") %>%
    select("nrGrupy", "grupa", "N", "mean", "sd")
  kowariancje = parametry %>%
    mutate(idTemp = paste0(.data$var, "_", .data$par)) %>%
    select(-"var", -"par", -"est", -"groupN_itemCATS")
  parametry = parametry %>%
    select(kryterium = "var", parametr = "par", wartosc = "est") %>%
    mutate(idTemp = paste0(.data$kryterium, "_", .data$parametr),
           grupowy = grepl("^grupa_[[:digit:]]+$", .data$kryterium),
           nrGrupy = as.integer(ifelse(.data$grupowy,
                                       sub("^grupa_", "", .data$kryterium),
                                       "")),
           kryterium = ifelse(.data$grupowy,
                              NA_character_, .data$kryterium),
           model = ifelse(.data$grupowy,
                          "n.d.",
                          sub("PLM", "PL",
                              toupper(sub("_.*$", "", .data$parametr)))),
           parametr = ifelse(.data$grupowy,
                             paste0("group_",
                                    tolower(sub("_.*$", "", .data$parametr))),
                             sub("^.*_", "", .data$parametr)),
           uwagi = ifelse(grepl("^t[[:digit:]]+_", .data$kryterium),
                          .data$kryterium,
                          NA_character_),
           id_elementu = NA_integer_) %>%
    left_join(grupy %>%
                select("nrGrupy", "grupa"),
              by = "nrGrupy") %>%
    left_join(data.frame(idTemp = kowariancje$idTemp,
                         bs = sqrt(diag(as.matrix(select(kowariancje,
                                                         -"idTemp"))))),
              by = "idTemp") %>%
    mutate() %>%
    select("kryterium", "parametr", "model", "wartosc", "uwagi", "bs",
           "id_elementu", "grupowy", "grupa", "idTemp")
  kowariancje = kowariancje %>%
    pivot_longer(-"idTemp", names_to = "idTemp2", values_to = "kowariancja") %>%
    filter(as.vector(lower.tri(kowariancje %>%
                                 select(-"idTemp"))))
  return(list(grupy = grupy,
              parametry = parametry,
              kowariancje = kowariancje))
}
