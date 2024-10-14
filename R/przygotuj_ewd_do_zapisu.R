#' @title Przetwarzanie i zapis wynikow obliczania latentnych wskaznikow EWD
#' @description
#' Przetwarza zwrócone przez *pvreg* oszacowania wskaźników EWD do struktur
#' danych umożliwiających zapisanie ich do bazy danych.
#' @inheritParams oblicz_ewd_bk
#' @param ewd ramka danych z oszacowaniami EWD wyestymowanymi przez *pvreg*
#' (element *EWD* listy zwróconej przez [estymuj_pvreg()])
#' @param nazwaWskaznika ciąg znaków - nazwa obliczonego wskaźnika EWD
#' @param dane lista ramek danych zwrócona przez [przygotuj_dane_do_ewd_bk()]
#' @param skalowania ramka danych o dwóch wierszach zawierająca informacje
#' o skalach i numerach skalowań, który powinny zostać przypisane wynikom
#' estymacji z wykorzystaniem *pvreg* (w bazie danych zapisywane jest również
#' powiązanie między wskaźnikami EWD a skalowaniami)
#' @inheritParams okresl_liczbe_zdajacych
#' @return lista ramek danych z elementami *ewd*, *wskazniki*,
#' *wskazniki_skalowania* i *liczba_zdajacych*
#' @seealso [oblicz_empiryczne_warstwice()], [okresl_liczbe_zdajacych()],
#' [oblicz_ewd_bk()], [przygotuj_pv_do_zapisu()]
#' @importFrom dplyr %>% .data across case_when left_join mutate rename_with select
przygotuj_ewd_do_zapisu = function(ewd, nazwaWskaznika, rokEWD, dane,
                                   skalowania, luWszyscy, src) {
  stopifnot(is.data.frame(ewd),
            all(c("id_szkoly", "out_schl", "out_schl_se", "eva_schl",
                  "eva_schl_se", "cor_eva_out", "mean_schl", "mean_schl_se",
                  "lu") %in% names(ewd)),
            is.list(dane),
            is.character(nazwaWskaznika), length(nazwaWskaznika) == 1,
            !anyNA(nazwaWskaznika), nazwaWskaznika != "",
            is.numeric(rokEWD), length(rokEWD) == 1, !anyNA(rokEWD),
            is.data.frame(skalowania),
            all(c("id_skali", "skalowanie") %in% names(skalowania)),
            is.data.frame(luWszyscy),
            all(c("id_szkoly", "rok", "lu_wszyscy",
                  "matura_miedzynarodowa") %in% names(luWszyscy)),
            dplyr::is.src(src) | is.null(src))
  stopifnot(all(sapply(dane, is.data.frame)))
  gamma = oblicz_empiryczne_warstwice(wyniki = ewd$out_schl, ewd = ewd$eva_schl,
                                      liczbaUczniow = ewd$lu, pr = c(0.5, 0.9))
  ewd = ewd %>%
    select("id_szkoly", "out_schl", "out_schl_se", "eva_schl", "eva_schl_se",
           "cor_eva_out", "mean_schl", "mean_schl_se", "lu") %>%
    # zmiana skali na (0, 15)
    mutate(across(c("out_schl", "out_schl_se", "eva_schl", "eva_schl_se"),
                  ~15*.)) %>%
    # przesuwanie średnich ważonych liczbą uczniów do 100 lub 0
    mutate(out_schl = 100 + .data$out_schl - sum(.data$lu / sum(.data$lu) * .data$out_schl),
           eva_schl = .data$eva_schl - sum(.data$lu / sum(.data$lu) * .data$eva_schl),
           mean_schl = .data$mean_schl - sum(.data$lu / sum(.data$lu) * .data$mean_schl)) %>%
    # zmiany nazw
    rename_with(~sub("^out_schl", nazwaWskaznika, .)) %>%
    rename_with(~sub("^mean_schl", paste0("sr_we_", nazwaWskaznika), .)) %>%
    rename_with(~sub("^eva_schl", paste0("ewd_", nazwaWskaznika), .)) %>%
    rename_with(~sub("^cor_eva_out$", paste0("kor_", nazwaWskaznika), .)) %>%
    rename_with(~sub("^(.*)_se$", "bs_\\1", .)) %>%
    # dopisywanie dodatkowych informacji
    mutate(lu_ewd = .data$lu,
           rok = rokEWD,
           rok_do = rokEWD) %>%
    left_join(dane$cechySzkol %>%
                select("id_szkoly", pomin = "pomin_szkole"),
              by = "id_szkoly") %>%
    left_join(luWszyscy,
              by = c("id_szkoly", "rok")) %>%
    # przypisywanie kategorii
    mutate(kategoria =
             case_when(.data$lu_ewd < 30 & .data$matura_miedzynarodowa ~ 205,
                       .data$lu_ewd < 30 ~ 204,
                       .data$matura_miedzynarodowa ~ 203,
                       .default = 0)) %>%
    mutate(kategoria = .data$kategoria +
             ifelse(rokEWD >= (2023 + (substr(nazwaWskaznika, 2, 2) == "t")) &
                      .data$kategoria != 0,
                    100, 0))
  wskazniki = data.frame(rodzaj_wsk = "ewd",
                         wskaznik = nazwaWskaznika,
                         rok_do = rokEWD,
                         gamma50 = unname(gamma["0.5"]),
                         gamma90 = unname(gamma["0.9"]),
                         stringsAsFactors = FALSE)
  wskaznikiSkalowania = skalowania %>%
    mutate(rodzaj_wsk = "ewd",
           wskaznik = nazwaWskaznika,
           rok_do = rokEWD) %>%
    select("rodzaj_wsk", "wskaznik", "rok_do", "id_skali", "skalowanie")
  liczbaZdajacych = okresl_liczbe_zdajacych(dane = dane,
                                            rokEWD = rokEWD,
                                            luWszyscy = luWszyscy,
                                            nazwaWskaznika = nazwaWskaznika)
  return(list(ewd = ewd,
              wskazniki = wskazniki,
              wskazniki_skalowania = wskaznikiSkalowania,
              liczba_zdajacych = liczbaZdajacych))
}
