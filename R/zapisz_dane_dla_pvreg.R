#' @title Obliczanie latentnych wskaznikow EWD
#' @description
#' Zapisuje pliki z danymi konieczne do obliczenia wskaźników EWD przez pakiet
#' *pvreg* Pythona.
#' @inheritParams estymuj_pvreg
#' @return ciąg znaków z nazwą pliku sterującego (w formacie JSON) z parametrami
#' wywołania *pvreg*
#' @seealso [przygotuj_zbior_z_parametrami_dla_pvreg()], [estymuj_pvreg()]
#' @importFrom utils write.csv
#' @importFrom dplyr %>% .data count matches mutate n_distinct select
zapisz_dane_dla_pvreg = function(dane, parametry, nazwa, nPV = 3L,
                                 nWatkow = 3L) {
  stopifnot(is.list(dane),
            all(c("wejscie", "matura", "warunkujace") %in% names(dane)),
            is.list(parametry),
            all(c("parametryWejscie", "kowariancjeWejscie",
                  "parametryMatura", "kowariancjeMatura") %in% names(parametry)),
            is.character(nazwa), length(nazwa) == 1,
            grepl("^[[:alpha:][:digit:]_-]+$", nazwa),
            is.numeric(nPV), length(nPV) == 1, nPV >= 0,
            is.numeric(nWatkow), length(nWatkow) == 1, nWatkow > 0)
  dane <- dane[c("wejscie", "matura", "warunkujace")]
  stopifnot(n_distinct(sapply(dane, nrow)) == 1,
            "grupa" %in% names(dane$wejscie), is.factor(dane$wejscie$grupa),
            "grupa" %in% names(dane$matura), is.factor(dane$matura$grupa),
            "id_obserwacji" %in% names(dane$wejscie),
            "id_obserwacji" %in% names(dane$matura),
            "id_obserwacji" %in% names(dane$warunkujace),
            "id_szkoly" %in% names(dane$warunkujace),
            "plec" %in% names(dane$warunkujace),
            "dysleksja_gimnazjum" %in% names(dane$warunkujace),
            "dysleksja_matura" %in% names(dane$warunkujace),
            as.integer(nPV) == nPV,
            as.integer(nWatkow) == nWatkow)

  dane$wejscie$grupa = droplevels(dane$wejscie$grupa)
  dane$matura$grupa = droplevels(dane$matura$grupa)
  grupyMatura = dane$matura %>%
    count(.data$grupa, name = "groupN_itemCATS") %>%
    mutate(grupaNr = as.integer(.data$grupa))
  grupyWejscie = dane$wejscie %>%
    count(.data$grupa, name = "groupN_itemCATS") %>%
    mutate(grupaNr = as.integer(.data$grupa))

  plikiDoNadpisania = intersect(list.files(paste0(nazwa, "/")),
                                paste0("ewd_", nazwa,
                                       c("_in_responses.csv", "_out_responses.csv",
                                         "_student_exog.csv",
                                         "_in_item_cats.csv", "_out_item_cats.csv",
                                         "_in_estimates.csv", "_out_estimates.csv")))
  if (length(plikiDoNadpisania) > 0) {
    warning("W katalogu '", nazwa, "' istnieją już pliki z danymi dla pvreg.py: '",
            paste(plikiDoNadpisania, collapse = "', '"), "'.",
            "\nZostaną one nadpisane.", immediate. = TRUE)
  }

  dane$wejscie %>%
    select("id_obserwacji", "grupa", matches("^([kp]|t[[:digit:]]+)_")) %>%
    mutate(grupa = as.integer(.data$grupa)) %>%
    write.csv(paste0(nazwa, "/ewd_", nazwa, "_in_responses.csv"),
              row.names = FALSE, na = "", quote = FALSE)
  dane$matura %>%
    select("id_obserwacji", "grupa", matches("^([kp]|t[[:digit:]]+)_")) %>%
    mutate(grupa = as.integer(.data$grupa)) %>%
    write.csv(paste0(nazwa, "/ewd_", nazwa, "_out_responses.csv"),
              row.names = FALSE, na = "", quote = FALSE)
  dane$warunkujace %>%
    mutate(plec = as.integer(factor(.data$plec)) - 1L,
           dysleksja_matura = as.integer(.data$dysleksja_matura),
           dysleksja_gimnazjum = as.integer(.data$dysleksja_gimnazjum)) %>%
    write.csv(paste0(nazwa, "/ewd_", nazwa, "_student_exog.csv"),
              row.names = FALSE, na = "", quote = FALSE)
  przygotuj_zbior_z_parametrami_dla_pvreg(parametry = parametry$parametryWejscie,
                                          kowariancje = parametry$kowariancjeWejscie,
                                          grupy = grupyWejscie) %>%
    write.csv(paste0(nazwa, "/ewd_", nazwa, "_in_estimates.csv"),
              row.names = FALSE, na = "", quote = FALSE)
  przygotuj_zbior_z_parametrami_dla_pvreg(parametry = parametry$parametryMatura,
                                          kowariancje = parametry$kowariancjeMatura,
                                          grupy = grupyMatura) %>%
    write.csv(paste0(nazwa, "/ewd_", nazwa, "_out_estimates.csv"),
              row.names = FALSE, na = "", quote = FALSE)
  writeLines(c('{',
               '\t"path": "./",',
               paste0('\t"responses_out": "ewd_', nazwa, '_out_responses.csv",'),
               paste0('\t"estimates_out": "ewd_', nazwa, '_out_estimates.csv",'),
               paste0('\t"responses_in": "ewd_', nazwa, '_in_responses.csv",'),
               paste0('\t"estimates_in": "ewd_', nazwa, '_in_estimates.csv",'),
               paste0('\t"student_exog": "ewd_', nazwa, '_student_exog.csv",'),
               '\t"student_id": "id_obserwacji",',
               '\t"school_id": "id_szkoly",',
               paste0('\t"fixed_effects": "',
                      paste(setdiff(names(dane$warunkujace),
                                    c("id_obserwacji", "id_szkoly")),
                            collapse = " "), '",'),
               paste0('\t"keep_pv": "', ifelse(nPV > 0L, 'True', 'False'), '",'),
               paste0('\t"npv": ', nPV, ','),
               paste0('\t"njobs": ', nWatkow, ','),
               '\t"out_path": "./",',
               paste0('\t"out_files": "ewd_', nazwa, '_"'),
               '}'),
             paste0(nazwa, "/ewd_", nazwa, "_pvreg_config.json"))

  return(paste0("ewd_", nazwa, "_pvreg_config.json"))
}
#' @title Obliczanie latentnych wskaznikow EWD
#' @param parametry ramka danych z parametrami modelu skalowania danego egzaminu
#' @param kowariancje ramka danych z kowariancjami parametrów modelu skalowania
#' danego egzaminu
#' @param grupy ramka danych z rozkładem liczebności grup w danych (i numerami
#' grup)
#' @return ramka danych o wartościami parametrów modelu skalowania
#' i ich kowariancji, w strukturze odpowiadającej tej oczekiwanej przez *pvreg*
#' @seealso [zapisz_dane_dla_pvreg()]
#' @importFrom dplyr %>% .data arrange bind_rows case_when desc left_join mutate select
#' @importFrom tidyr pivot_wider
przygotuj_zbior_z_parametrami_dla_pvreg = function(parametry, kowariancje, grupy) {
  parametry = parametry %>%
    left_join(grupy,
              by = "grupa") %>%
    mutate(var = ifelse(.data$parametr %in% c("group_mean", "group_sd"),
                        paste0("grupa_", .data$grupaNr),
                        ifelse(is.na(.data$kryterium),
                               .data$parametr_uwagi,
                               .data$kryterium)),
           par = ifelse(.data$parametr %in% c("group_mean", "group_sd"),
                        sub("^group_(mean|sd)$", "\\1_theta", .data$parametr),
                        paste0(tolower(sub("^([1234]PL)$", "\\1M", .data$model)),
                               "_", .data$parametr)),
           groupN_itemCATS =
             case_when(.data$parametr %in% "group_mean" ~ .data$groupN_itemCATS,
                       .data$parametr %in% c("group_sd", "c") ~ NA_integer_,
                       .data$parametr %in% "a" ~ 0L,
                       .data$parametr %in% "b" ~ 1L,
                       grepl("^b[[:digit:]]+$", .data$parametr) ~
                         suppressWarnings(as.integer(sub("^b", "",
                                                         .data$parametr))))) %>%
    select("id_elementu", "var", "par", est = "wartosc", "groupN_itemCATS", "bs") %>%
    arrange(desc(grepl("^grupa_", .data$var)), .data$var, .data$par)
  kowariancje = kowariancje %>%
    bind_rows(kowariancje %>%
                select(id_elementu1 = "id_elementu2",
                       id_elementu2 = "id_elementu1",
                       "kowariancja"),
              parametry %>%
                mutate(kowariancja = .data$bs^2) %>%
                select(id_elementu1 = "id_elementu",
                       id_elementu2 = "id_elementu",
                       "kowariancja")) %>%
    left_join(parametry %>%
                select(id_elementu1 = "id_elementu", "var", "par"),
              by = "id_elementu1") %>%
    left_join(parametry %>%
                mutate(col = paste0(.data$var, "_", .data$par)) %>%
                select(id_elementu2 = "id_elementu", "col"),
              by = "id_elementu2") %>%
    arrange(desc(grepl("^grupa_", .data$var)), .data$var, .data$par,
            desc(grepl("^grupa_", .data$col)), .data$col) %>%
    select(-"id_elementu1", -"id_elementu2") %>%
    pivot_wider(names_from = "col", values_from = "kowariancja",
                values_fill = 0)
  parametry <- parametry %>%
    select("var", "par", "est", "groupN_itemCATS") %>%
    left_join(kowariancje,
              by = c("var", "par"))
  return(parametry[, c("var", "par", "est", "groupN_itemCATS",
                       paste0(parametry$var, "_", parametry$par))])
}
