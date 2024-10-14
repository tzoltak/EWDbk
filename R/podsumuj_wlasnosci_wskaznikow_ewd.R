#' @title Podsumowanie wlasnosci obliczonych wskaznikow EWD
#' @description
#' Przygotowuje proste zestawienie najważniejszych własności wskaźników EWD
#' zapisanych w obiekcie klasy *listaWskaznikowEWD*.
#' @param ewd obiekt klasy *listaWskaznikowEWD*, w szczególności zwrócony przez
#' [oblicz_ewd_bk()] lub [agreguj_ewd()]
#' @param gamma poziom ufności dla obszarów ufności szkół, przy sprawdzaniu, czy
#' przecinają one osie układu współrzędnych wykresu (100, 0)
#' @return ramka danych
#' @importFrom stats qchisq weighted.mean
#' @importFrom dplyr %>% bind_rows case_match count group_by n select summarise
#' @importFrom tidyr pivot_wider
#' @export
podsumuj_wlasnosci_wskaznikow_ewd = function(ewd, gamma = 0.95) {
  stopifnot(is.list(ewd), inherits(ewd, "listaWskaznikowEWD"),
            is.numeric(gamma), length(gamma) == 1, gamma > 0, gamma < 1)
  wartosciEWD = mapply(
    function(x, w) {
      names(x)[names(x) == w] = "sr_wy"
      names(x)[names(x) == paste0("bs_", w)] = "bs_sr_wy"
      names(x) = sub(paste0("_", w, "$"), "", names(x))
      return(x)
    }, ewd, names(ewd), SIMPLIFY = FALSE) %>%
    bind_rows(.id = "wskaznik")
  l = qchisq(gamma, 2)^0.5

  attributes(ewd)$wskazniki %>%
    mutate(typ_szkoly = droplevels(factor(case_match(substring(.data$wskaznik, 2, 2),
                                                     "l" ~ "LO",
                                                     "t" ~ "T",
                                                     .default = "nieznany"),
                                          c("LO", "T", "nieznany"))),
           przedmiot = droplevels(factor(case_match(substring(.data$wskaznik, 3),
                                                    "b" ~ "biologia",
                                                    "c" ~ "chemia",
                                                    "f" ~ "fizyka",
                                                    "g" ~ "geografia",
                                                    "h" ~ "historia",
                                                    "i" ~ "informatyka",
                                                    "ja" ~ "j. angielski",
                                                    "jp" ~ "j. polski",
                                                    "m" ~ "matematyka",
                                                    "w" ~ "WOS",
                                                    .default = "nieznany"),
                                         c("matematyka", "biologia", "chemia",
                                           "fizyka", "geografia", "informatyka",
                                           "j. polski", "historia", "WOS",
                                           "j. angielski", "nieznany")))) %>%
    select(-"rodzaj_wsk") %>%
    inner_join(wartosciEWD %>%
                 group_by(.data$wskaznik, .data$rok_do) %>%
                 summarise(l_szk = n(),
                           l_uczn = sum(.data$lu_ewd),
                           sr_l_uczn_szk = mean(.data$lu_ewd),
                           .groups = "drop"),
               by = c("wskaznik", "rok_do")) %>%
    inner_join(wartosciEWD %>%
                 count(.data$wskaznik, .data$rok_do, .data$kategoria) %>%
                 pivot_wider(names_from = .data$kategoria, names_prefix = "kat",
                             values_from = .data$n, values_fill = 0L),
               by = c("wskaznik", "rok_do")) %>%
    inner_join(wartosciEWD %>%
                 group_by(.data$wskaznik, .data$rok_do) %>%
                 summarise(across(c(.data$ewd, .data$sr_we, .data$sr_wy),
                                  list(wtmean = ~weighted.mean(., lu_ewd),
                                       wtsd = ~cov.wt(matrix(., ncol = 1),
                                                      lu_ewd)$cov[1, 1]^0.5,
                                       mean = ~mean(.),
                                       sd = ~sd(.))),
                           ods_ewdIstDodatnie =
                             sum((.data$ewd - l*.data$bs_ewd) > 0) / n(),
                           ods_ewdIstUjemne =
                             sum((.data$ewd + l*.data$bs_ewd) < 0) / n(),
                           ods_sr_wyIstPowyzej100 =
                             sum((.data$sr_wy - l*.data$bs_sr_wy) > 100, na.rm = TRUE) / n(),
                           ods_sr_wyIstPonizej100 =
                             sum((.data$sr_wy + l*.data$bs_sr_wy) < 100, na.rm = TRUE) / n(),
                           .groups = "drop"),
               by = c("wskaznik", "rok_do"))
}
