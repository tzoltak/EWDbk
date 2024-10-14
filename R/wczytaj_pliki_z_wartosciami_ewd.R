#' @title Obliczanie "wieloletnich" wskaznikow EWD poprzez agregacje wskaznikow "jednorocznych"
#' @description
#' Wczytuje pliki *.RData* z wartościami wskaźników EWD.
#' @param pliki wektor ciągów znaków z nazwami (ścieżkami do) plików
#' @inheritParams agreguj_ewd
#' @return lista ramek danych z elementami *ewd*, *wskazniki*,
#' *wskaznikiSkalowania* i *liczbaZdajacych*
#' @seealso [wczytaj_plik_z_wartosciami_ewd()], [agreguj_ewd()]
#' @importFrom dplyr bind_rows filter semi_join
wczytaj_pliki_z_wartosciami_ewd = function(pliki, wskazniki, lata, wskaznikiWR) {
  stopifnot(is.character(pliki), length(pliki) > 1, !anyNA(pliki),
            is.character(wskazniki), length(wskazniki) > 0L,
            !anyNA(wskazniki),
            is.numeric(lata), length(lata) > 0, !anyNA(lata),
            is.logical(wskaznikiWR), length(wskaznikiWR) == 1,
            wskaznikiWR %in% c(TRUE, FALSE))
  plikiIstnieja = sapply(pliki, file.exists)
  if (!all(plikiIstnieja)) {
    stop("Plik(i) '", paste(pliki[!plikiIstnieja], collapse = "', '"),
         "' nie istnieją.")
  }
  wsk = wskaznikiSkalowania = liczbaZdajacych = data.frame()
  ewd = vector(mode = "list", length = length(pliki))
  for (p in seq_along(pliki)) {
    ewdTemp = wczytaj_plik_z_wartosciami_ewd(pliki[p])
    wsk = bind_rows(wsk, ewdTemp$wskazniki)
    wskaznikiSkalowania = bind_rows(wskaznikiSkalowania,
                                    ewdTemp$wskaznikiSkalowania)
    liczbaZdajacych = bind_rows(liczbaZdajacych, ewdTemp$liczbaZdajacych)
    ewd[[p]] = ewdTemp$ewd
  }
  wskaznikiWEwd = unique(unlist(sapply(ewd, names)))
  wskaznikiWeWskaznikach = unique(wsk$wskaznik)
  if (!all(wskaznikiWEwd %in% wskaznikiWeWskaznikach) ||
      !(all(wskaznikiWeWskaznikach %in% wskaznikiWEwd))) {
    stop("Niezgodny zestaw wskaźników pomiędzy zestawieniami wartości wskaźników EWD a atrybutem 'wskazniki`.")
  }
  ewdPolaczone = vector(mode = "list", length = length(wskaznikiWEwd))
  names(ewdPolaczone) = sort(wskaznikiWEwd)
  for (w in names(ewdPolaczone)) {
    ewdPolaczone[[w]] = bind_rows(lapply(ewd, function(x) return(x[[w]])))
    names(ewdPolaczone[[w]])[names(ewdPolaczone[[w]]) == w] = "sr_wy"
    names(ewdPolaczone[[w]])[names(ewdPolaczone[[w]]) == paste0("bs_", w)] =
      "bs_sr_wy"
    names(ewdPolaczone[[w]]) = sub(paste0("_", w, "$"), "",
                                   names(ewdPolaczone[[w]]))
  }

  if (wskaznikiWR) {
    wsk = bind_rows(lapply(wskazniki,
                           function(x, wsk) return(filter(wsk,
                                                          grepl(x, .data$wskaznik))),
                           wsk = wsk))
    if (nrow(wsk) == 0) stop("W podanych plikach brak wskaźników pasujących do podanego wzorca (wzorców).")
    message("Do podanego wzorca (wzorców) w podanych plikach pasują następujące wskaźniki: ",
            paste(sort(unique(wsk$wskaznik)), collapse = ", "), ".")
  } else {
    brakujaceWskazniki = setdiff(wskazniki, wsk$wskaznik)
    if (length(brakujaceWskazniki) > 0) {
      stop("W podanych plikach brak wskaźników: ",
           paste(brakujaceWskazniki, collapse = ", "),
           ".\n(Wskaźniki znalezione we wczytanych plikach to: ",
           paste(sort(unique(wsk$wskaznik)), collapse = ", "), ".)")
    }
    wsk = filter(wsk, .data$wskaznik %in% wskazniki)
  }
  wsk = filter(wsk, .data$rok_do %in% lata)
  lataPoOdfiltrowaniu = lapply(split(wsk, wsk$wskaznik),
                               function(x) return(unique(x$rok_do)))
  brakujaceLata = lataPoOdfiltrowaniu[sapply(lataPoOdfiltrowaniu,
                                             length) != length(lata)]
  if (length(brakujaceLata) > 0) {
    stop("W podanych plikach dla niektórych spośród wybranych wskaźników brak wartości wskaźników EWD z niektórych lat:\n",
         paste0(" - ", names(brakujaceLata), ": ",
                sapply(brakujaceLata,
                       function(x, lata) return(paste(setdiff(lata, x),
                                                      collapse = ", ")),
                       lata = lata),
                collapse = "\n"))
  }

  return(list(ewd = semi_join(bind_rows(ewdPolaczone, .id = "wskaznik"), wsk,
                              by = c("wskaznik", "rok_do")),
              wskazniki = wsk,
              wskaznikiSkalowania = semi_join(wskaznikiSkalowania, wsk,
                                              by = c("wskaznik", "rok_do")),
              liczbaZdajacych = semi_join(liczbaZdajacych, wsk,
                                          by = c("wskaznik", "rok_do"))))
}
#' @title Obliczanie "wieloletnich" wskaznikow EWD poprzez agregacje wskaznikow "jednorocznych"
#' @description
#' Wczytuje pojedynczy plik *.RData* z wartościami wskaźników EWD i kontroluje
#' spójność jego zawartości.
#' @param plik ciągów znaków z nazwą (ścieżką do) pliku
#' @details
#' W stosunku do formatu, w jakim dane przechowywane są w wejściowych plikach,
#' do elementu *liczbaZdajacych* dopisywana jest kolumna `rok_do`.
#' @return lista z elementami *ewd* (lista ramek danych), *wskazniki*,
#' *wskaznikiSkalowania* i *liczbaZdajacych* (ramki danych)
#' @seealso [wczytaj_pliki_z_wartosciami_ewd()]
#' @importFrom dplyr anti_join right_join
wczytaj_plik_z_wartosciami_ewd = function(plik) {
  obiekty = load(plik)
  if (!("ewd" %in% obiekty)) stop("Plik '", plik, "' nie zawiera obiektu `ewd`.")
  ewd = get("ewd")
  if (!inherits(ewd, "listaWskaznikowEWD")) stop("Obiekt `ewd` w pliku '", plik,
                                                 "' nie jest klasy 'listaWskaznikowEWD'.")
  atrybuty = c("wskazniki", "wskazniki_skalowania", "liczba_zdajacych")
  if (!all(atrybuty %in% names(attributes(ewd)))) {
    stop("Obiekt `ewd` w pliku '", plik, "' nie posiada atrybutu/ów '",
         paste(atrybuty[!(atrybuty %in% names(attributes(ewd)))],
               collapse = "', '"), "'.")
  }
  if (any(duplicated(names(ewd)))) {
    stop("Obiekt `ewd` w pliku '", plik,
         "' zawiera zduplikowane nazwy elementów (zestawień wartości wskaźników EWD).")
  }
  tryCatch(stopifnot("atrybut 'wskazniki` obiektu `ewd` nie jest ramką danych." =
                       is.data.frame(attributes(ewd)$wskazniki),
                     "atrybut 'wskazniki_skalowania` obiektu `ewd` nie jest ramką danych." =
                       is.data.frame(attributes(ewd)$wskazniki_skalowania),
                     "atrybut 'liczba_zdajacych` obiektu `ewd` nie jest ramką danych." =
                       is.data.frame(attributes(ewd)$liczba_zdajacych)),
           error = function(e) stop("W pliku '", plik, "' ", e$message))
  tryCatch(stopifnot("brak niezbędnych kolumn w atrybucie `wskazniki` obiektu `ewd`." =
                       all(c("rodzaj_wsk", "wskaznik", "rok_do") %in%
                             names(attributes(ewd)$wskazniki)),
                     "brak niezbędnych kolumn w atrybucie `wskazniki_skalowania` obiektu `ewd`." =
                       all(c("rodzaj_wsk", "wskaznik", "rok_do", "id_skali", "skalowanie") %in%
                           names(attributes(ewd)$wskazniki_skalowania)),
                     "brak niezbędnych kolumn w atrybucie `liczba_zdajacych` obiektu `ewd`." =
                       all(c("rodzaj_wsk", "wskaznik", "kategoria_lu", "id_szkoly_m",
                           "lu", "lu_ewd", "lu_wszyscy") %in%
                           names(attributes(ewd)$liczba_zdajacych))),
           error = function(e) stop("W pliku '", plik, "' ", e$message))
  wskaznikiLata = split(attributes(ewd)$wskazniki,
                        attributes(ewd)$wskazniki$wskaznik)
  wskaznikiLata = lapply(wskaznikiLata, function(x) return(unique(x$rok_do)))
  if (any(sapply(wskaznikiLata, length) > 1) &&
      !("rok_do" %in% names(attributes(ewd)$liczba_zdajacych))) {
    stop("W atrybucie `wskazniki` obiektu `ewd` w pliku '", plik,
         "' wskaźnik(i): ",
         paste(names(wskaznikiLata)[sapply(wskaznikiLata, length) > 1],
               collapse = ", "),
         " ma(ją) przypisanych kilka różnych wartości `rok_do`, co jest niepoprawne (bo w takim razie nie da się przypisać wierszy z atrybutu `liczba_zdajacych` do konkretnego roku).")
  } else if (!("rok_do" %in% names(attributes(ewd)$liczba_zdajacych))) {
    attributes(ewd)$liczba_zdajacych =
      right_join(attributes(ewd)$wskazniki[, c("wskaznik", "rok_do")],
                 attributes(ewd)$liczba_zdajacych,
                 by = "wskaznik")
  }
  tryCatch(stopifnot("w obiekcie `ewd` występują wskaźniki (elementy), których nie ma w atrybucie `wskazniki`." =
                       length(setdiff(names(ewd),
                                      unique(attributes(ewd)$wskazniki$wskaznik))) == 0,
                     "w atrybucie `wskazniki` obiektu `ewd` występują wskaźniki, których nie ma w obiekcie `ewd`." =
                       length(setdiff(unique(attributes(ewd)$wskazniki$wskaznik),
                                      names(ewd))) == 0,
                     "w atrybucie `wskazniki_skalowania` obiektu `ewd` występują wskaźniki/lata, których nie ma w atrybucie `wskazniki`." =
                       nrow(anti_join(attributes(ewd)$wskazniki_skalowania,
                                      attributes(ewd)$wskazniki,
                                      by = c("wskaznik", "rok_do"))) == 0,
                     "w atrybucie `wskazniki` obiektu `ewd` występują wskaźniki/lata, których nie ma w atrybucie `wskazniki_skalowania`." =
                       nrow(anti_join(attributes(ewd)$wskazniki,
                                      attributes(ewd)$wskazniki_skalowania,
                                      by = c("wskaznik", "rok_do"))) == 0,
                     "w atrybucie `liczba_zdajacych` obiektu `ewd` występują wskaźniki/lata, których nie ma w atrybucie `wskazniki`." =
                       nrow(anti_join(attributes(ewd)$liczba_zdajacych,
                                      attributes(ewd)$wskazniki,
                                      by = c("wskaznik", "rok_do"))) == 0,
                     "w atrybucie `wskazniki` obiektu `ewd` występują wskaźniki/lata, których nie ma w atrybucie `liczba_zdajacych`." =
                       nrow(anti_join(attributes(ewd)$wskazniki,
                                      attributes(ewd)$liczba_zdajacych,
                                      by = c("wskaznik", "rok_do"))) == 0),
           error = function(e) stop("W pliku '", plik, "' ", e$message))
  return(list(ewd = lapply(ewd, identity), # pozbywanie się atrybutów
              wskazniki = attributes(ewd)$wskazniki,
              wskaznikiSkalowania = attributes(ewd)$wskazniki_skalowania,
              liczbaZdajacych = attributes(ewd)$liczba_zdajacych))
}
