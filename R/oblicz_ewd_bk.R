#' @title Obliczanie latentnych wskaznikow EWD szkol maturalnych
#' @description
#' Funkcja oblicza jednoroczne wskaźniki EWD LO i techników zgodnie z metodą
#' opracowaną przez Bartka Kondratka.
#' @param rokEWD rok egzaminu maturalnego (identyfikujący rocznik absolwentów)
#' @param skaleMatura wektor ciągów znaków z wyrażeniami regularnymi
#' identyfikującymi skale (po kolumnie `opis` w bazie danych) lub wektor
#' liczbowy z `id_skali` - wskazuje skale, które mają zostać wykorzystane do
#' skalowania wyników matury
#' @param skaleWe wektor ciągów znaków z wyrażeniami regularnymi
#' identyfikującymi skale (po kolumnie `opis` w bazie danych) lub wektor
#' liczbowy z `id_skali` - wskazuje skale, które mają zostać wykorzystane do
#' skalowania wyników egzaminu *na wejściu*
#' @param skalowanieMatura numer skalowania (w ramach każdej ze skal
#' zidentyfikowanych na podstawie argumentu `skaleMatura`), parametry z którego
#' mają zostać wykorzystane do skalowania wyników matury; domyślna wartość -1
#' oznacza wybór (oddzielnie w ramach każdej skali) największego numeru
#' skalowania spośród tych, które nie opisują wyników działania *pvreg*
#' (tzn. pochodzą ze skalowania wyników egzaminu *uirt*-em, a nie powstały
#' w wyniku obliczania latentnych wskaźników EWD)
#' @param skalowanieWe numer skalowania (w ramach każdej ze skal
#' zidentyfikowanych na podstawie argumentu `skaleMatura`), parametry z którego
#' mają zostać wykorzystane do skalowania wyników egzaminu *na wejściu*;
#' domyślna wartość -1 oznacza wybór (oddzielnie w ramach każdej skali)
#' największego numeru skalowania spośród tych, które nie opisują wyników
#' działania *pvreg* (tzn. pochodzą ze skalowania wyników egzaminu *uirt*-em,
#' a nie powstały w wyniku obliczania latentnych wskaźników EWD)
#' @param katalogSurowe ciąg znaków - ścieżka do katalogu, w którym znajdują
#' się dane z wynikami surowymi egzaminów, pobranymi przy pomocy funkcji
#' [EWDdane::pobierz_wyniki_surowe]
#' @param zapisz wartość logiczna - czy zapisać obliczone wartości wskaźników
#' EWD i PV indywidualnych oszacowań umiejętności na dysku w formie plików
#' .RData?
#' @inheritParams przygotuj_dane_do_ewd_bk
#' @param czyEgzaminOsmoklasisty wartość logiczna - musi być podana tylko jeśli
#' `rokEWD=2023` (w każdym innym przypadku jest ignorowana), aby móc określić
#' mapowanie części egzaminu (skal) egzaminu *na wyjściu* na części egzaminu
#' (skale) egzaminu *na wejściu*
#' @param src połączenie z bazą danych IBE zwracane przez funkcję [ZPD::polacz()];
#' jeśli nie podane, podjęta zostanie próba automatycznego nawiązania połączenia
#' (poprzez wywoływanie funkcji [ZPD::polacz()] z domyślnymi argumentami)
#' @inheritParams estymuj_pvreg
#' @details
#' Preferowany sposób użycia funkcji to:
#' 1. Wywołanie jej z argumentem `metoda="tylko pliki"`,
#' 2. Ręczne uruchamiania *pvreg* z konsoli systemowej - pozwala łatwo
#'    (*ręcznie*) zrównoleglać (i *balansować* obciążenia) obliczanie wskaźników
#'    i diagnozować ew. problemy, które by przy tym wynikły,
#' 3. Wywołanie funkcji jeszcze raz z argumentem `metoda="Python"`
#'    (i `nadpisz=FALSE`, co jest jednak wartością domyślną) aby wczytać
#'    obliczone wartości wskaźników i przetworzyć je do formy, w której będą
#'    mogły zostać łatwo wczytane do bazy z wykorzystaniem funkcji z pakietu
#'    *ZPDzapis*.
#' @return lista dwóch ramek danych zawierających obliczone wartości PV
#' indywidualnych oszacowań umiejętności (*PV*) i wskaźników EWD (*EWD*)
#' @seealso [znajdz_skale()], [okresl_liczbe_uczniow_w_szkolach()],
#' [pobierz_parametry_egzaminow()], [przygotuj_dane_do_ewd_bk()],
#' [unormuj_parametry_egzaminow()], [estymuj_pvreg()],
#' [przygotuj_pv_do_zapisu()], [przygotuj_ewd_do_zapisu()]
#' @importFrom dplyr %>% .data arrange bind_rows case_when distinct ends_with filter group_by inner_join mutate rename_with select summarise
#' @importFrom tidyr expand_grid
#' @export
oblicz_ewd_bk = function(rokEWD,
                         skaleMatura = paste0("^ewd;m_[^;]+;", rokEWD),
                         skaleWe =  paste0("^ewd;g[hm](LO;",
                                           rokEWD - 3L, rokEWD - 4L,
                                           "|T;", rokEWD - 4L, rokEWD - 5L,
                                           ")"),
                         skalowanieMatura = -1L, skalowanieWe = -1L,
                         katalogSurowe = "../../skalowanie/dane surowe",
                         metoda = c("tylko pliki", "Python", "R"),
                         nadpisz = FALSE, zapisz = metoda != "tylko pliki",
                         minLUcznSzk= 1L, nPV = 5L, nWatkow = 5L,
                         czyEgzaminOsmoklasisty = ifelse(rokEWD > 2023,
                                                         TRUE, NA),
                         src = NULL) {
  metoda = match.arg(metoda)
  stopifnot(is.numeric(rokEWD), length(rokEWD) == 1, !anyNA(rokEWD),
            is.numeric(skaleMatura) || is.character(skaleMatura), length(skaleMatura) > 0,
            is.numeric(skaleWe) || is.character(skaleWe), length(skaleWe) > 0,
            is.numeric(skalowanieMatura), length(skalowanieMatura) == 1,
            skalowanieMatura > 0 || skalowanieMatura == -1L,
            is.numeric(skalowanieWe), length(skalowanieWe) == 1,
            skalowanieWe > 0 || skalowanieWe == -1,
            is.logical(zapisz), length(zapisz) == 1, zapisz %in% c(TRUE, FALSE),
            is.character(katalogSurowe), length(katalogSurowe) == 1,
            is.logical(nadpisz), length(nadpisz) == 1, nadpisz %in% c(TRUE, FALSE),
            is.numeric(minLUcznSzk), length(minLUcznSzk) == 1, minLUcznSzk > 0,
            is.numeric(nPV), length(nPV) == 1, nPV > 0,
            is.numeric(nWatkow), length(nWatkow) == 1, nWatkow > 0,
            is.logical(czyEgzaminOsmoklasisty), length(czyEgzaminOsmoklasisty) == 1,
            dplyr::is.src(src) || is.null(src))
  stopifnot(as.integer(rokEWD) == rokEWD,
            as.integer(skalowanieMatura) == skalowanieMatura,
            as.integer(skalowanieWe) == skalowanieWe,
            as.integer(minLUcznSzk) == minLUcznSzk,
            as.integer(nPV) == nPV,
            as.integer(nWatkow) == nWatkow,
            dir.exists(katalogSurowe))
  if (rokEWD == 2023) {
    stopifnot("Dla 2023 r. trzeba podać argument `czyEgzaminOsmoklasisty`." =
                !is.na(czyEgzaminOsmoklasisty))
  } else {
    czyEgzaminOsmoklasisty = rokEWD > 2023
  }
  nrSkalowaniaMatura = skalowanieMatura
  nrSkalowaniaWe = skalowanieWe

  # zbieranie informacji o skalach i skalowaniach
  skaleMatura = znajdz_skale(skaleMatura, skalowanie = skalowanieMatura, src = src) %>%
    mutate(czesc_egzaminu_we =
             sub("^ewd;m_([^;]+);.*$", "\\1", .data$opis_skali)) %>%
    mutate(czesc_egzaminu_we =
             case_when(.data$czesc_egzaminu_we %in% c("h", "jp", "w") &
                         czyEgzaminOsmoklasisty ~ "jp",
                       .data$czesc_egzaminu_we %in% c("b", "c", "f", "g", "i", "m") &
                         czyEgzaminOsmoklasisty ~ "m",
                       .data$czesc_egzaminu_we %in% c("h", "jp", "w") ~ "h",
                       .data$czesc_egzaminu_we %in% c("b", "c", "f", "g", "i", "m") ~ "m",
                       .data$czesc_egzaminu_we %in% c("ja") ~ "ja"),
           skalowanie = ifelse(nrSkalowaniaMatura == -1L,
                               .data$max_skalowanie_bez_pvreg,
                               .data$skalowanie)) %>%
    arrange(.data$opis_skali)
  message("Skale pasujące do podanych argumentem `skaleMatura`:")
  skaleMatura %>%
    select("id_skali", "opis_skali", "rodzaj_skali", "rodzaj_egzaminu", "rok",
           "skala_do_prezentacji", "skalowanie", "max_skalowanie") %>%
    rename_with(~gsub("_", " ", .)) %>%
    as.data.frame(check.names = FALSE) %>%
    print()
  skaleWe = znajdz_skale(skaleWe, skalowanie = skalowanieWe, src = src) %>%
    mutate(typ_szkoly = sub("^.*;(g[hm]|e8j[ap]|e8m)(LO|T);.*$", "\\2", .data$opis_skali)) %>%
    group_by(.data$id_skali, .data$opis_skali, .data$rodzaj_skali,
             .data$skala_do_prezentacji, .data$rodzaj_egzaminu,
             .data$typ_szkoly, .data$max_skalowanie,
             .data$max_skalowanie_bez_pvreg, .data$skalowanie) %>%
    summarise(lata = list(sort(.data$rok, decreasing = TRUE)),
              .groups = "drop") %>%
    mutate(czesc_egzaminu_we =
             sub("^ewd;[ge8]+([hm]|ja|jp)(LO|T);.*$", "\\1", .data$opis_skali),
           skalowanie = ifelse(rep(nrSkalowaniaWe == -1L, n()),
                               .data$max_skalowanie_bez_pvreg,
                               .data$skalowanie)) %>%
    ungroup() %>%
    arrange(.data$opis_skali)
  message("Skale pasujące do podanych argumentem `skaleWe`:")
  skaleWe %>%
    mutate(lata = sapply(.data$lata, paste, collapse = ", ")) %>%
    select("id_skali", "opis_skali", "rodzaj_skali", "rodzaj_egzaminu", "lata",
           "skala_do_prezentacji", "skalowanie", "max_skalowanie") %>%
    rename_with(~gsub("_", " ", .)) %>%
    as.data.frame(check.names = FALSE) %>%
    print()

  skale = inner_join(skaleMatura %>%
                       filter(.data$rodzaj_egzaminu %in% "matura",
                              .data$rodzaj_skali %in% "ewd",
                              .data$rok %in% rokEWD,
                              !is.na(.data$skalowanie)) %>%
                       expand_grid(typ_szkoly = c("LO", "T")),
                     skaleWe %>%
                       filter(.data$rodzaj_egzaminu %in% c("egzamin gimnazjalny",
                                                           "egzamin ósmoklasisty"),
                              .data$rodzaj_skali %in% "ewd",
                              !is.na(.data$skalowanie)),
                     by = c("czesc_egzaminu_we", "typ_szkoly"),
                     suffix = c("_matura", "_we"))
  message("Połączone skale na wyjściu i wejściu:")
  skale %>%
    mutate(lata = sapply(.data$lata, paste, collapse = ", ")) %>%
    select("typ_szkoly", "id_skali_matura", "opis_skali_matura",
           "skalowanie_matura", "id_skali_we", "opis_skali_we", lata_we = "lata",
           "skalowanie_we") %>%
    arrange(.data$id_skali_matura, .data$typ_szkoly, .data$id_skali_we) %>%
    rename_with(~gsub("_", " ", .)) %>%
    as.data.frame(check.names = FALSE) %>%
    print()
  if (nrow(skale) == 0) {
    stop("Nie udało się połączyć żadnych skal matury ze skalami na wejściu.")
  } else if (nrow(anti_join(skale,
                            select(skaleMatura, id_skali_matura = "id_skali"),
                            by = "id_skali_matura")) > 0) {
    stop("Nie udało się połączyć niektórych skal matury z żadną skalą na wejściu.")
  }

  luWszyscy = okresl_liczbe_uczniow_w_szkolach(rokEWD = rokEWD,
                                               katalogSurowe = katalogSurowe,
                                               src = src)

  # samo obliczanie EWD
  ewd = vector(mode = "list", length = nrow(skale))
  names(ewd) = paste0("m", tolower(substr(skale$typ_szkoly, 1, 1)),
                      sub("^ewd;m_([^;]+);.*", "\\1",
                          skale$opis_skali_matura))
  skalowania = bind_rows(skale %>%
                           select(ends_with("_we")) %>%
                           rename_with(~sub("_we$", "", .)) %>%
                           mutate(indeks = 1L:nrow(skale)),
                         skale %>%
                           select(ends_with("_matura")) %>%
                           rename_with(~sub("_matura$", "", .)) %>%
                           mutate(indeks = 1L:nrow(skale))) %>%
    group_by(.data$id_skali) %>%
    mutate(skalowanie_zapisz = .data$max_skalowanie + 1L:n()) %>%
    ungroup()
  pv = vector(mode = "list", length = nrow(skalowania))
  names(pv) = paste0(skalowania$opis_skali, ";", skalowania$skalowanie_zapisz)
  for (i in seq_along(ewd)) {
    message("\n# Obliczanie EWD w ramach modelu: '", names(ewd)[i], "'\n  start: ",
            format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n")
    parametry = pobierz_parametry_egzaminow(skale = skale[i, ], src = src)
    dane = przygotuj_dane_do_ewd_bk(skale = skale[i, ],
                                    rokEWD = rokEWD,
                                    typSzkoly = skale$typ_szkoly[i],
                                    tematyLaureatowMatura = parametry$tematyLaureatowMatura,
                                    parametryMatura = parametry$parametryMatura,
                                    tematyLaureatowWejscie = parametry$tematyLaureatowWejscie,
                                    parametryWejscie = parametry$parametryWejscie,
                                    katalogSurowe = katalogSurowe,
                                    minLUcznSzk = minLUcznSzk,
                                    src = src)
    parametry = unormuj_parametry_egzaminow(parametry = parametry,
                                            dane = dane)
    wyniki = estymuj_pvreg(dane = dane, parametry = parametry,
                           nazwa = names(ewd)[i],
                           metoda = metoda,
                           nPV = nPV, nWatkow = nWatkow,
                           nadpisz = nadpisz)
    if (!is.null(wyniki)) {
      indeksWe = which(skalowania$id_skali == skale$id_skali_we[i] &
                         skalowania$indeks == i)
      indeksMatura = which(skalowania$id_skali == skale$id_skali_matura[i] &
                             skalowania$indeks == i)
      pv[[indeksWe]] =
        przygotuj_pv_do_zapisu(pv = wyniki$pv %>%
                                 filter(.data$exam_var == 0L),
                               skale = skalowania[indeksWe, , drop = FALSE],
                               parametry =
                                 list(parametry = parametry$parametryWejscie,
                                      kowariancje = parametry$kowariancjeWejscie),
                               czesciEgzaminu =
                                 attributes(dane$wejscie)$czesciEgzaminu,
                               grupy = levels(dane$wejscie$grupa),
                               nazwaWskaznika = names(ewd)[i],
                               rokEWD = rokEWD)
      pv[[indeksMatura]] =
        przygotuj_pv_do_zapisu(pv = wyniki$pv %>%
                                 filter(.data$exam_var == 1L),
                               skale = skalowania[indeksMatura, , drop = FALSE],
                               parametry =
                                 list(parametry = parametry$parametryMatura,
                                      kowariancje = parametry$kowariancjeMatura),
                               czesciEgzaminu =
                                 attributes(dane$matura)$czesciEgzaminu,
                               grupy = levels(dane$matura$grupa),
                               nazwaWskaznika = names(ewd)[i],
                               rokEWD = rokEWD)
      ewd[[i]] =
        przygotuj_ewd_do_zapisu(ewd = wyniki$ewd,
                                nazwaWskaznika = names(ewd)[i],
                                rokEWD = rokEWD,
                                dane = dane,
                                skalowania =
                                  skalowania[c(indeksWe, indeksMatura), ] %>%
                                  select("id_skali",
                                         skalowanie = "skalowanie_zapisz"),
                                luWszyscy = luWszyscy,
                                src = src)
    }
    message("\n  koniec: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), "\n")
  }
  # koniec
  class(pv) = c("listaWynikowSkalowania", class(pv))
  ewd = structure(lapply(ewd, function(x) return(x$ewd)),
                  wskazniki =
                    bind_rows(lapply(ewd,
                                     function(x) return(x$wskazniki))),
                  wskazniki_skalowania =
                    bind_rows(lapply(ewd,
                                     function(x) return(x$wskazniki_skalowania))),
                  liczba_zdajacych =
                    bind_rows(lapply(ewd,
                                     function(x) return(x$liczba_zdajacych))),
                  dataUtworzenia = Sys.time())
  class(ewd) = c("listaWskaznikowEWD", class(ewd))
  if (zapisz) {
    nazwyObiektow = paste0("ewd", rokEWD, c("PV", ""))
    nazwyPlikow = paste0(nazwyObiektow, ".RData")
    i = 0
    while (any(file.exists(nazwyPlikow))) {
      i = i + 1
      nazwyPlikow = paste0(nazwyObiektow,"-", i, ".RData")
    }
    if (i > 0) {
      message("Co najmniej jeden z plików '",
              paste(paste0(nazwyObiektow, ".RData"), collapse = "', '"),
              "' już istnieje. Wyniki zostały zapisane do plików '",
              paste(nazwyPlikow, collapse = "', '"), "'.")
    }
    save(pv, file = nazwyPlikow[1])
    save(ewd, file = nazwyPlikow[2])
  }
  return(list(pv = pv,
              ewd = ewd))
}
