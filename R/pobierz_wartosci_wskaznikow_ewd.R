#' @title Obliczanie "wieloletnich" wskaznikow EWD poprzez agregacje wskaznikow "jednorocznych"
#' @description
#' Pobiera wartości wskaźników EWD (oraz powiązane z nimi informacje) z bazy
#' danych
#' @inheritParams agreguj_ewd
#' @return lista ramek danych z elementami *ewd*, *wskazniki*,
#' *wskaznikiSkalowania* i *liczbaZdajacych*
#' @seealso [agreguj_ewd()]
#' @importFrom dplyr %>% bind_rows collect distinct filter inner_join mutate select semi_join
pobierz_wartosci_wskaznikow_ewd = function(wskazniki, lata, wskaznikiWR,
                                           usunPrzyrostki, src) {
  stopifnot(is.character(wskazniki), length(wskazniki) > 0L,
            !anyNA(wskazniki),
            is.numeric(lata), length(lata) > 0, !anyNA(lata),
            is.logical(wskaznikiWR), length(wskaznikiWR) == 1,
            wskaznikiWR %in% c(TRUE, FALSE),
            is.character(usunPrzyrostki), length(usunPrzyrostki) == 1,
            !anyNA(usunPrzyrostki),
            dplyr::is.src(src) | is.null(src))
  if (is.null(src)) {
    src = ZPD::polacz()
    on.exit(ZPD::rozlacz(src))
  }

  wsk = ZPD::pobierz_wskazniki(src, doPrezentacji = NA, wszystkieKolumny = TRUE) %>%
    filter(.data$rodzaj_wsk == "ewd") %>%
    collect()
  if (wskaznikiWR) {
    wsk = bind_rows(lapply(wskazniki,
                           function(x, wsk) return(filter(wsk,
                                                          grepl(x, .data$wskaznik))),
                           wsk = wsk))
    if (nrow(wsk) == 0) stop("W bazie danych brak wskaźników pasujących do podanego wzorca (wzorców).")
    message("Do podanego wzorca (wzorców) w bazie danych pasują następujące wskaźniki: ",
            paste(sort(unique(wsk$wskaznik)), collapse = ", "), ".")
  } else {
    brakujaceWskazniki = setdiff(wskazniki, wsk$wskaznik)
    if (length(brakujaceWskazniki) > 0) {
      stop("W bazie danych brak wskaźników: ",
           paste(brakujaceWskazniki, collapse = ", "),
           ".\n(Wskaźniki znalezione w bazie to: ",
           paste(sort(unique(wsk$wskaznik)), collapse = ", "), ".)")
    }
    wsk = filter(wsk, .data$wskaznik %in% wskazniki)
  }
  wskaznikiSkalowania = wsk %>%
    select("rodzaj_wsk", "wskaznik", "rok_do", "id_skali", "skalowanie") %>%
    distinct()
  wsk = wsk %>%
    select("rodzaj_wsk", "wskaznik", "rok_do") %>%
    distinct()
  prefiksEgzamin = unique(substring(wsk$wskaznik, 1, 1))
  if (length(prefiksEgzamin) > 1) stop("Wszystkie wskaźniki muszą odnosić się do tego samego egzaminu na wyjściu (co oznacza, że ich nazwy muszą zaczynać się od tej samej litery).")

  wsk = filter(wsk, .data$rok_do %in% lata)
  lataPoOdfiltrowaniu = lapply(split(wsk, wsk$wskaznik),
                               function(x) return(unique(x$rok_do)))
  brakujaceLata = lataPoOdfiltrowaniu[sapply(lataPoOdfiltrowaniu,
                                             length) != length(lata)]
  if (length(brakujaceLata) > 0) {
    stop("Dla niektórych spośród wybranych wskaźników brak wartości wskaźników EWD w bazie z niektórych lat:\n",
         paste0(" - ", names(brakujaceLata), ": ",
                sapply(brakujaceLata,
                       function(x, lata) return(paste(setdiff(lata, x),
                                                      collapse = ", ")),
                       lata = lata),
                collapse = "\n"))
  }
  wskaznikiSkalowania = semi_join(wskaznikiSkalowania, wsk,
                                  by = c("rodzaj_wsk", "wskaznik", "rok_do"))

  ewd = ZPD::pobierz_wartosci_wskaznikow(src = src, czyPomin = TRUE) %>%
    semi_join(wsk,
              by = c("rodzaj_wsk", "wskaznik", "rok_do"),
              copy = TRUE) %>%
    collect() %>%
    mutate(matura_miedzynarodowa = grepl("maturę międzynarodową",
                                         tolower(.data$komunikat))) %>%
    select("wskaznik", "id_szkoly", sr_wy = "srednia", bs_sr_wy = "bs",
           "ewd", "bs_ewd", kor = "korelacja",
           sr_we = "srednia_we", bs_sr_we = "bs_srednia_we",
           "lu", "lu_ewd", "rok", "rok_do", "pomin", "lu_wszyscy",
           "matura_miedzynarodowa")
  liczbaZdajacych = ZPD::pobierz_wartosci_wskaznikow_lu(src = src) %>%
    inner_join(ZPD::pobierz_wartosci_wskaznikow(src = src, czyPomin = TRUE) %>%
                 semi_join(wsk,
                           by = c("rodzaj_wsk", "wskaznik", "rok_do"),
                           copy = TRUE) %>%
                 select("id_ww", "rodzaj_wsk", "wskaznik", "rok_do", "id_szkoly"),
               by = "id_ww") %>%
    select("wskaznik", "rok_do", "id_ww", "rodzaj_wsk",
           "kategoria_lu", "id_szkoly", lu = "przedm_lu",
           lu_ewd = "przedm_lu_ewd", lu_wszyscy = "przedm_lu_wszyscy") %>%
    collect()
  names(liczbaZdajacych)[names(liczbaZdajacych) == "id_szkoly"] =
    paste0(names(liczbaZdajacych)[names(liczbaZdajacych) == "id_szkoly"], "_",
           prefiksEgzamin)

  if (usunPrzyrostki != "") {
    wskaznikiBezPrzyrostkow = unique(sub(usunPrzyrostki, "", wsk$wskaznik))
    message("Nazwy wskaźników po usunięciu przyrostków: ",
            paste(sort(wskaznikiBezPrzyrostkow), collapse = ", "), ".")
    if (length(wskaznikiBezPrzyrostkow) < length(unique(wsk$wskaznik))) {
      stop("Po usunięciu przyrostków nazwy wskaźników nie są unikalne.")
    }
    wsk$wskaznik = sub(usunPrzyrostki, "", wsk$wskaznik)
    ewd$wskaznik = sub(usunPrzyrostki, "", ewd$wskaznik)
    wskaznikiSkalowania$wskaznik = sub(usunPrzyrostki, "",
                                       wskaznikiSkalowania$wskaznik)
    liczbaZdajacych$wskaznik = sub(usunPrzyrostki, "", liczbaZdajacych$wskaznik)
  }

  return(list(ewd = ewd,
              wskazniki = wsk,
              wskaznikiSkalowania = wskaznikiSkalowania,
              liczbaZdajacych = liczbaZdajacych))
}
