#' @title Obliczanie "wieloletnich" wskaznikow EWD poprzez agregacje wskaznikow "jednorocznych"
#' @description
#' Oblicza *wieloletnie* wskaźniki EWD poprzez agregację wskaźników
#' *jednorocznych*.
#' @param wskazniki wektor ciąg znaków z nazwami wskaźników, które mają zostać
#' zagregowane lub wyrażeniami regularnymi, do których nazwy takich wskaźników
#' mają pasować - por. argument `wskaznikiWR`
#' @param lata wektor liczb naturalnych - lata, które mają zostać
#' zagregowane
#' @param pliki opcjonalnie wektor ciągów znaków z nazwami plików (zapisanych
#' na dysku np. w wyniku użycia `oblicz_ewd_bk()`) zawierających wartości
#' wskaźników, z których mają one zostać wczytane zamiast z bazy danych
#' @param zapisz wartość logiczna - czy zapisać obliczone wartości zagregowanych
#' wskaźników EWD na dysku w formie pliku .RData?
#' @param wskaznikiWR wartość logiczna - czy traktować argument `wskazniki`jako
#' zawierający wyrażenia regularne (a nie nazwy wskaźników)? domyślnie `TRUE`,
#' jeśli argument `wskaźniki` to pojedynczy ciąg znaków, a `FALSE`, jeśli jest
#' to wektor kilku ciągów znaków
#' @param usunPrzyrostki jeśli wartości wskaźników są pobierane z bazy (tj.
#' `pliki=NULL`), ciąg znaków z wyrażeniem regularnym opisującym ciąg znaków,
#' który powinien zostać usunięty z nazw wskaźników pobranych z bazy (por.
#' sekcja *details* poniżej); jeśli z nazw wskaźników pobranych z bazy nie ma
#' być usuwany żaden ciąg znaków, jako wartość argumentu należy podać pusty
#' ciąg znaków (`""`)
#' @param traktujLataJakoNiezalezne wartość logiczna - czy przy obliczaniu
#' (ko)wariancji traktować wskaźniki z poszczególnych lat jako niezależne od
#' siebie, czy jako powiązane zmienne? (obliczając empiryczne kowariancje
#' pomiędzy wskaźnikami w poszczególnych latach) - obecnie jedyna możliwa do
#' wyboru wartość to `TRUE`, ale argument ma przypominać, że można by
#' zaimplementować i alternatywne podejście
#' @param src połączenie z bazą danych IBE zwracane przez funkcję [ZPD::polacz()];
#' jeśli nie podane, podjęta zostanie próba automatycznego nawiązania połączenia
#' (poprzez wywoływanie funkcji [ZPD::polacz()] z domyślnymi argumentami)
#' @return lista klasy *listaWskaznikowEWD*
#' @details
#' Podając wartości argumentu `wskazniki` należy mieć na uwadze, czy wartości
#' wskaźników mają być pobrane z bazy danych, czy wczytane z zapisanych na dysku
#' plików (jeśli zostanie podany argument `pliki`) - nazwy wskaźników w bazie
#' oprócz ciągu znaków identyfikującego egzamin na wyjściu, typ szkoły
#' i przedmiot (obszar), np. *mlb*, zawierają również przyrostki identyfikujące
#' wersję wskaźnika (w tym odróżniające wskaźniki *jednoroczne* od
#' *wieloletnich*!), podczas gdy w plikach zapisanych na dysku nazwy wskaźników
#' są tych przyrostków pozbawione (są one dodawane dopiero na etapie zapisywania
#' wartości wskaźników do bazy).
#' @seealso [oblicz_ewd_bk()]
#' @importFrom stats weighted.mean
#' @importFrom dplyr %>% across all_of arrange filter group_by inner_join last mutate semi_join select summarise starts_with
#' @export
agreguj_ewd = function(wskazniki, lata, pliki = NULL, zapisz = TRUE,
                       wskaznikiWR = length(wskazniki) == 1,
                       usunPrzyrostki = "_.*$",
                       traktujLataJakoNiezalezne = TRUE,
                       src = NULL) {
  stopifnot(is.character(wskazniki), length(wskazniki) > 0L,
            !anyNA(wskazniki),
            is.numeric(lata), length(lata) > 0, !anyNA(lata),
            is.null(pliki) || is.character(pliki),
            is.logical(zapisz), length(zapisz) == 1, zapisz %in% c(TRUE, FALSE),
            is.logical(wskaznikiWR), length(wskaznikiWR) == 1,
            wskaznikiWR %in% c(TRUE, FALSE),
            is.character(usunPrzyrostki), length(usunPrzyrostki) == 1,
            !anyNA(usunPrzyrostki),
            is.logical(traktujLataJakoNiezalezne),
            length(traktujLataJakoNiezalezne) == 1,
            traktujLataJakoNiezalezne %in% c(TRUE, FALSE),
            dplyr::is.src(src) || is.null(src))
  stopifnot(all(as.integer(lata) == lata))
  if (!traktujLataJakoNiezalezne) stop("Agregacja traktująca wyniki z poszczególnych lat jako powiązane zmienne losowe nie została jeszcze zaimplementowana.")
  if (!is.null(pliki)) {
    stopifnot(is.character(pliki), length(pliki) > 1, !anyNA(pliki))
    ewd = wczytaj_pliki_z_wartosciami_ewd(pliki, wskazniki, lata, wskaznikiWR)
  } else {
    ewd = pobierz_wartosci_wskaznikow_ewd(wskazniki, lata, wskaznikiWR,
                                          usunPrzyrostki, src)
  }
  message("Wczytano wartości wskaźników jednorocznych.")
  wskazniki = ewd$wskazniki %>%
    group_by(.data$rodzaj_wsk, .data$wskaznik) %>%
    summarise(rok_do = max(lata),
              .groups = "drop")
  wskaznikiSkalowania = ewd$wskaznikiSkalowania %>%
    group_by(.data$rodzaj_wsk, .data$wskaznik) %>%
    mutate(rok_do = max(lata)) %>%
    ungroup()
  liczbaZdajacych = ewd$liczbaZdajacych
  luWszyscy = ewd$ewd %>%
    group_by(.data$wskaznik, .data$id_szkoly) %>%
    summarise(lu_wszyscy = sum(.data$lu_wszyscy),
              .groups = "drop")
  ewd = ewd$ewd %>%
    select(-"lu_wszyscy") %>%
    arrange(.data$wskaznik, .data$id_szkoly, .data$rok) %>%
    group_by(.data$wskaznik, .data$id_szkoly) %>%
    mutate(pominPojedynczy = is.na(.data$kor) & !all(is.na(.data$kor)),
           pominieteRoczniki = any(.data$pominPojedynczy)) %>%
    filter(!.data$pominPojedynczy)

  liczbaZdajacych = liczbaZdajacych %>%
    semi_join(rename_with(ungroup(ewd),
                          ~sub("^id_szkoly$",
                               grep("^id_szkoly_", names(liczbaZdajacych),
                                    value = TRUE), .)),
              by = c("wskaznik", "rok_do",
                     grep("^id_szkoly_", names(liczbaZdajacych), value = TRUE))) %>%
    group_by(.data$id_ww, .data$rodzaj_wsk, .data$wskaznik, .data$kategoria_lu,
             across(starts_with("id_szkoly_"))) %>%
    summarise(across(starts_with("lu"), sum),
              .groups = "drop") %>%
    as.data.frame()

  ewd = ewd %>%
    summarise(ewd = weighted.mean(.data$ewd, .data$lu_ewd),
              sr_wy = weighted.mean(.data$sr_wy, .data$lu_ewd),
              sr_we = weighted.mean(.data$sr_we, .data$lu_ewd),
              kor =
                sum(.data$kor*.data$bs_ewd*.data$bs_sr_wy *
                      (.data$lu_ewd / sum(.data$lu_ewd))^2) /
                sum(.data$bs_ewd^2 * (.data$lu_ewd / sum(.data$lu_ewd))^2)^0.5 /
                sum(.data$bs_sr_wy^2 * (.data$lu_ewd / sum(.data$lu_ewd))^2)^0.5,
              bs_ewd = sum(.data$bs_ewd^2 * (.data$lu_ewd / sum(.data$lu_ewd))^2)^0.5,
              bs_sr_wy = sum(.data$bs_sr_wy^2 * (.data$lu_ewd / sum(.data$lu_ewd))^2)^0.5,
              bs_sr_we = sum(.data$bs_sr_we^2 * (.data$lu_ewd / sum(.data$lu_ewd))^2)^0.5,
              across(starts_with("lu"), sum),
              across(all_of(c("pomin", "matura_miedzynarodowa")), dplyr::last),
              kategoria = case_when(all(.data$rok_do != max(lata)) ~ 306, # "W połączonej bazie wyników matury i egzaminu ósmoklasisty dla tej szkoły brak wyników z najnowszego rocznika."
                                    .data$lu_ewd < 30 & any(.data$matura_miedzynarodowa) ~ 305, # "W połączonej bazie wyników matury i egzaminu ósmoklasisty dla analizowanego okresu dla tej szkoły dysponujemy mniej niż 30 wynikami. Część uczniów tej szkoły zdawała Maturę Międzynarodową (IB), nie mamy dostępu do ich wyników."
                                    .data$lu_ewd < 30 ~ 304, # "W połączonej bazie wyników matury i egzaminu ósmoklasisty dla analizowanego okresu dla tej szkoły dysponujemy mniej niż 30 wynikami."
                                    any(.data$matura_miedzynarodowa) ~ 303, # "Część uczniów tej szkoły zdawała Maturę Międzynarodową (IB), nie mamy dostępu do ich wyników. W związku z tym wyznaczenie pozycji szkoły na wykresie może być mniej precyzyjne."
                                    any(.data$pominieteRoczniki) ~ 307, # "Z przyczyn technicznych przy obliczaniu wartości wskaźnika pominięto wyniki uczniów, którzy byli jedynymi zdającymi maturę z tego przedmiotu w danym roku."
                                    n_distinct(.data$rok_do) == 1 ~ 301, # "W połączonej bazie wyników matury i egzaminu ósmoklasisty dla tej szkoły dysponujemy wynikami tylko dla 1 roku. W związku z tym wyznaczenie pozycji szkoły na wykresie może być mniej precyzyjne."
                                    n_distinct(.data$rok_do) == 2 & length(lata) > 2 ~ 300, # "W połączonej bazie wyników matury i egzaminu ósmoklasisty dla tej szkoły dysponujemy wynikami tylko dla 2 lat. W związku z tym wyznaczenie pozycji szkoły na wykresie może być mniej precyzyjne."
                                    .default = 0),
              rok = max(.data$rok),
              rok_do = max(lata),
              .groups = "drop") %>%
    left_join(luWszyscy,
              by = c("wskaznik", "id_szkoly")) %>%
    select("wskaznik", "id_szkoly", "sr_we", "bs_sr_we", "ewd", "bs_ewd",
           "kor", "sr_wy", "bs_sr_wy", "lu", "lu_ewd", "rok", "rok_do", "pomin",
           "lu_wszyscy", "matura_miedzynarodowa", "kategoria")
  wskazniki = wskazniki %>%
    select(-starts_with("gamma")) %>%
    inner_join(ewd %>%
                 group_by(.data$wskaznik, .data$rok_do) %>%
                 summarise(gamma50 =
                             oblicz_empiryczne_warstwice(wyniki = .data$sr_wy,
                                                         ewd = .data$ewd,
                                                         liczbaUczniow = .data$lu_ewd,
                                                         pr = 0.5),
                           gamma90 =
                             oblicz_empiryczne_warstwice(wyniki = .data$sr_wy,
                                                         ewd = .data$ewd,
                                                         liczbaUczniow = .data$lu_ewd,
                                                         pr = 0.9),
                           .groups = "drop"),
               by = c("wskaznik", "rok_do"))
  message("Zakończono agregowanie.")

  ewd = split(ewd, ewd$wskaznik) %>%
    lapply(function(x) {
      wskaznik = x$wskaznik[1]
      names(x)[grepl("^(bs_|)(sr_we|ewd|sr_wy|kor)$", names(x))] <-
        paste0(names(x)[grepl("^(bs_|)(sr_wy|ewd|sr_we|kor)$", names(x))],
               "_", wskaznik)
      names(x) <- sub("sr_wy_", "", names(x))
      return(as.data.frame(x[, names(x) != "wskaznik"]))
    })
  ewd = structure(ewd,
                  wskazniki = as.data.frame(wskazniki),
                  wskazniki_skalowania = as.data.frame(wskaznikiSkalowania),
                  liczba_zdajacych = as.data.frame(liczbaZdajacych),
                  dataUtworzenia = Sys.time(),
                  class = c("listaWskaznikowEWD", "list"))
  if (zapisz) {
    nazwaPlikuPodstawa = paste0("ewd-agr", max(lata), "-", min(lata))
    nazwaPliku = paste0(nazwaPlikuPodstawa, ".RData")
    i = 0
    while (file.exists(nazwaPliku)) {
      i = i + 1
      nazwaPliku = paste0(nazwaPlikuPodstawa,"-", i, ".RData")
    }
    if (i > 0) {
      message("Plik '", paste0(nazwaPlikuPodstawa, ".RData"),
              "' już istnieje. Wyniki zostały zapisane do pliku '",
              nazwaPliku, "'.")
    }
    save(ewd, file = nazwaPliku)
  }
  return(ewd)
}
