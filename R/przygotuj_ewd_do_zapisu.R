#' @importFrom dplyr %>% .data across case_when left_join mutate rename_with select
przygotuj_ewd_do_zapisu = function(ewd, nazwaWskaznika, rokEWD, dane,
                                   skalowania, luWszyscy, src) {
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
                       .data$matura_miedzynarodowa ~ 203,
                       .data$lu_ewd < 30 ~ 204,
                       .default = 0))
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
