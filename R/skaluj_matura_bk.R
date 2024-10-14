#' @title Skalowanie wynikow matury w celu uzyskania parametrow zadan
#' @description
#' Funkcja przeprowadza skalowanie wyników matury, z wykorzystaniem *Staty*
#' i pakietu *uirt*, w celu uzyskania parametrów zadań i grup zdających,
#' które zostaną następnie wykorzystane w estymacji wskaźników EWD
#' z wykorzystaniem *pvreg*.
#' @param rokEWD rok egzaminu maturalnego (identyfikujący rocznik absolwentów)
#' @inheritParams znajdz_skale
#' @param skalujTylkoGdyBrakSkalowan wartość logiczna - czy wykluczyć ze
#' skalowania wszystkie skale, które mają już zapisane w bazie wyniki
#' **jakiegokolwiek** skalowania?
#' @param zapisz wartość logiczna - czy zapisać obliczone wartości parametrów
#' i oszacowania umiejętności na dysku w formie pliku .RData?
#' @param katalogSurowe ciąg znaków - ścieżka do katalogu, w którym znajdują
#' się dane z wynikami surowymi egzaminów, pobranymi przy pomocy funkcji
#' [EWDdane::pobierz_wyniki_surowe]
#' @inheritParams skaluj_uirt
#' @inheritParams skaluj_az_dobrze
#' @param src połączenie z bazą danych IBE zwracane przez funkcję [ZPD::polacz()];
#' jeśli nie podane, podjęta zostanie próba automatycznego nawiązania połączenia
#' (poprzez wywoływanie funkcji [ZPD::polacz()] z domyślnymi argumentami)
#' @details
#' Napotkawszy w wynikach skalowania zadania spełniające kryteria do usunięcia
#' funkcja usuwa je (wszystkie na raz) i przeprowadza skalowanie jeszcze raz
#' (*do skutku*). Informacja o usunięciu kryteriów jest zapisywana w wynikach
#' skalowania, aby umożliwić odpowiednie zmodyfikowanie skali przy zapisie
#' wyników skalowania do bazy danych.
#'
#' Wczytywanie wyników skalowania do bazy odbywa się z wykorzystaniem funkcji
#' [ZPDzapis::zapisz_skalowanie] z wykorzystaniem wyników skalowania zapisanych
#' na dysku w formie plików .RData.
#' @return lista klasy *listaWynikowSkalowania*
#' @seealso [znajdz_skale()], [przygotuj_dane_do_skalowania()],
#' [skaluj_az_dobrze()]
#' @importFrom dplyr %>% .data arrange filter left_join mutate pull rename_with select
#' @export
skaluj_matura_bk = function(rokEWD,
                            skale = paste0("^ewd;m_[^;]+;", rokEWD),
                            skalujTylkoGdyBrakSkalowan = TRUE,
                            zapisz = TRUE, katalogSurowe = "../../dane surowe",
                            nadpiszWynikiSkalowaniaNaDysku = FALSE,
                            maxNIter = 1000L,
                            uzyjRozkladowAPrioriDlaZadan3PL = list(a = c(1.5, 2),
                                                                   b = c(0,   2.5),
                                                                   c = c(2.5, 5.5)),
                            kryteriaUsuwaniaZadan = c(absA = 0.1, absB = 10),
                            src = NULL) {
  stopifnot(is.numeric(rokEWD), length(rokEWD) == 1,
            is.numeric(skale) || is.character(skale), length(skale) > 0,
            is.logical(skalujTylkoGdyBrakSkalowan),
            length(skalujTylkoGdyBrakSkalowan) == 1,
            skalujTylkoGdyBrakSkalowan %in% c(TRUE, FALSE),
            is.logical(zapisz), length(zapisz) == 1, zapisz %in% c(TRUE, FALSE),
            is.character(katalogSurowe), length(katalogSurowe) == 1,
            is.logical(nadpiszWynikiSkalowaniaNaDysku),
            length(nadpiszWynikiSkalowaniaNaDysku) == 1,
            nadpiszWynikiSkalowaniaNaDysku %in% c(TRUE, FALSE),
            is.numeric(maxNIter), length(maxNIter) == 1, maxNIter > 0,
            is.list(uzyjRozkladowAPrioriDlaZadan3PL) || is.null(uzyjRozkladowAPrioriDlaZadan3PL),
            is.numeric(kryteriaUsuwaniaZadan),
            length(kryteriaUsuwaniaZadan) == 2,
            !anyNA(kryteriaUsuwaniaZadan), all(kryteriaUsuwaniaZadan >= 0),
            all(c("absA", "absB") %in% names(kryteriaUsuwaniaZadan)),
            dplyr::is.src(src) | is.null(src))
  stopifnot(as.integer(rokEWD) == rokEWD,
            as.integer(maxNIter) == maxNIter,
            dir.exists(katalogSurowe))
  if (is.list(uzyjRozkladowAPrioriDlaZadan3PL)) {
    stopifnot(all(names(uzyjRozkladowAPrioriDlaZadan3PL) %in% c("a", "b", "c")),
              !duplicated(names(uzyjRozkladowAPrioriDlaZadan3PL)),
              all(sapply(uzyjRozkladowAPrioriDlaZadan3PL, is.numeric)),
              all(sapply(uzyjRozkladowAPrioriDlaZadan3PL, length) == 2),
              all(sapply(uzyjRozkladowAPrioriDlaZadan3PL, is.finite)))
  }
  if (rokEWD > 2024) {
    stop("Funkcja nie obsługuje skalowania dla lat obliczania wskaźników EWD późniejszych niż 2024.")
  }

  # zbieranie informacji o skalach i skalowaniach
  skale = znajdz_skale(skale, src = src) %>%
    mutate(ma_skalowanie = !is.na(.data$max_skalowanie),
           skalowanie = ifelse(is.na(.data$max_skalowanie),
                               1, .data$max_skalowanie + 1),
           opis_skalowania = .data$opis_skali) %>%
    arrange(.data$opis_skali)
  skale %>%
    select("id_skali", "opis_skali", "rodzaj_skali", "skala_do_prezentacji",
           "ma_skalowanie") %>%
    rename_with(~gsub("_", " ", .)) %>%
    as.data.frame(check.names = FALSE) %>%
    print()
  if (skalujTylkoGdyBrakSkalowan) {
    skale = skale %>%
      filter(!.data$ma_skalowanie)
  }
  if (nrow(skale) == 0) {
    message("Brak skal do przeprowadzenia skalowania.")
    return(NULL)
  }
  # samo skalowanie
  wyniki = vector(mode = "list", length = nrow(skale))
  names(wyniki) = sub("^ewd;(m_[^;]+);.*", "\\1", skale$opis_skali)
  for (i in seq_along(wyniki)) {
    message("\n# Skalowanie skali: '", skale$opis_skali[i], "' (skalowanie nr ",
            skale$skalowanie[i], ")\n  start: ",
            format(Sys.time(), "%Y.%m.%d %H:%M:%S"))
    dane = przygotuj_dane_do_skalowania(skala = skale[i, ], rokEWD = rokEWD,
                                        katalogSurowe = katalogSurowe,
                                        laureaci = "usuń", src = src)
    zadania = attributes(dane)$czesciEgzaminu
    zadania3pl = zadania %>%
      filter(.data$typ_pytania %in% c("jednokrotnego wyboru", "pseudootwarte")) %>%
      pull("kryterium")
    # pętla skalowania z usuwaniem kryteriów oceny, dla których nie udało się
    # uirtowi (poprawnie) oszacować wariancji - co do zasady z powodu
    # okołozerowej dyskryminacji
    usunieteZadania = vector(mode = "character", length = 0)
    parametry = skaluj_az_dobrze(dane, zadania3pl,
                                 nazwa = paste0(gsub(";", "-", skale$opis_skali[i])),
                                 nadpiszWynikiSkalowaniaNaDysku = nadpiszWynikiSkalowaniaNaDysku,
                                 maxNIter = maxNIter,
                                 usunUjemneDyskryminacje = FALSE,
                                 uzyjRozkladowAPrioriDlaZadan3PL = uzyjRozkladowAPrioriDlaZadan3PL,
                                 kryteriaUsuwaniaZadan = kryteriaUsuwaniaZadan)
    # zapis ostatecznych wyników
    wyniki[[i]] = list(skalowania = data.frame(id_skali = skale$id_skali[i],
                                               skalowanie = skale$skalowanie[i],
                                               opis = skale$opis_skalowania[i],
                                               estymacja = "MML (UIRT)",
                                               do_prezentacji = FALSE,
                                               data = Sys.Date()),
                       skalowania_grupy =
                         data.frame(id_skali = skale$id_skali[i],
                                    skalowanie = skale$skalowanie[i],
                                    grupa = levels(parametry$grupy$grupa)[parametry$grupy$grupa]),
                       skalowania_elementy =
                         data.frame(id_skali = skale$id_skali[i],
                                    skalowanie = skale$skalowanie[i],
                                    id_elementu = NA_integer_,
                                    parametry$parametry %>%
                                      mutate(grupa = levels(.data$grupa)[.data$grupa])) %>%
                         left_join(zadania %>%
                                     select("id_skali", "kryterium", "kolejnosc"),
                                   by = c("id_skali", "kryterium")) %>%
                         select("id_skali", "kolejnosc", "skalowanie",
                                "parametr", "model", "wartosc", "uwagi", "bs",
                                "id_elementu", "grupowy", "grupa", "idTemp"),
                       skalowania_elementy_kowariancje = parametry$kowariancje,
                       usunieteKryteria = usunieteZadania)
    class(wyniki[[i]]) = c("wynikiSkalowania", class(wyniki[[i]]))
    attributes(wyniki[[i]])$dataSkalowania = Sys.time()
    message("  koniec: ", format(attributes(wyniki[[i]])$dataSkalowania,
                                 "%Y.%m.%d %H:%M:%S"))
  }
  # koniec
  class(wyniki) = c("listaWynikowSkalowania", class(wyniki))
  if (zapisz) {
    nazwaObiektu = paste0("ewd", rokEWD, "SkalowanieMatura")
    nazwaPliku = paste0(nazwaObiektu, ".RData")
    i = 0
    while (file.exists(nazwaPliku)) {
      i = i + 1
      nazwaPliku = paste0(nazwaObiektu,"-", i, ".RData")
    }
    if (i > 0) {
      message("Plik '", paste0(nazwaObiektu, ".RData"),
              "' już istnieje. Wyniki zostały zapisane do pliku '",
              nazwaPliku, "'.")
    }
    assign(nazwaObiektu, wyniki)
    save(list = nazwaObiektu, file = nazwaPliku)
  }
  return(wyniki)
}
