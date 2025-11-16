#' @title Skalowanie wynikow egzaminu na wejsciu w celu uzyskania parametrow zadan
#' @description
#' Funkcja przeprowadza skalowanie wyników egzaminu *na wejściu* (egzaminu
#' ósmoklasisty, ew. egzaminu gimnazjalnego), z wykorzystaniem *Staty*
#' i pakietu *uirt*, w celu uzyskania parametrów zadań i grup zdających,
#' które zostaną następnie wykorzystane w estymacji wskaźników EWD
#' z wykorzystaniem *pvreg*.
#' @seealso [znajdz_skale()], [przygotuj_dane_do_skalowania()],
#' [skaluj_az_dobrze()]
#' @inheritParams skaluj_matura_bk
#' @inheritParams skaluj_uirt
#' @inheritParams skaluj_az_dobrze
#' @inherit skaluj_matura_bk details
#' @inherit skaluj_matura_bk return
#' @importFrom dplyr %>% .data arrange bind_rows group_by mutate rename_with select summarise
#' @export
skaluj_we_bk <- function(rokEWD,
                         skale = paste0("^ewd;g[hm](LO;",
                                        rokEWD - 3, rokEWD - 4,
                                        "|T;", rokEWD - 4, rokEWD - 5,
                                        ")"),
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
  if (rokEWD > 2025) {
    stop("Funkcja nie obsługuje skalowania dla lat obliczania wskaźników EWD późniejszych niż 2025.")
  }

  # zbieranie informacji o skalach i skalowaniach
  skale = znajdz_skale(skale, src = src) %>%
    mutate(typ_szkoly = sub("^.*;(g[hm]|e8ja|e8jp|e8m)(LO|T);.*$", "\\2",
                            .data$opis_skali)) %>%
    group_by(.data$id_skali, .data$opis_skali, .data$rodzaj_skali,
             .data$skala_do_prezentacji, .data$rodzaj_egzaminu,
             .data$typ_szkoly, .data$max_skalowanie) %>%
    summarise(lata = list(sort(.data$rok, decreasing = TRUE)),
              .groups = "drop") %>%
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
  names(wyniki) = sub("^ewd;((g|e8)[^;]*(LO|T));.*", "\\1", skale$opis_skali)
  for (i in seq_along(wyniki)) {
    message("\n# Skalowanie skali: '", skale$opis_skali[i], "' (skalowanie nr ",
            skale$skalowanie[i], ")\n  start: ",
            format(Sys.time(), "%Y.%m.%d %H:%M:%S"))
    dane = przygotuj_dane_do_skalowania(skala = skale[i, ], rokEWD = rokEWD,
                                        katalogSurowe = katalogSurowe,
                                        dodajRokDoNazwTematow = TRUE,
                                        laureaci = "usuń", src = src)
    zadania = attributes(dane)$czesciEgzaminu
    lata = unique(zadania$rok)
    parametry = list(grupy = data.frame(),
                     parametry = data.frame(),
                     kowariancje = data.frame())
    usunieteZadania = vector(mode = "character", length = 0)
    # oddzielne skalowanie poszczególnych lat egzaminu
    for (r in lata) {
      parametryRok = skaluj_we_rok(r = r, opis_skali = skale$opis_skali[i],
                                   dane = dane, zadania = zadania,
                                   nadpiszWynikiSkalowaniaNaDysku = nadpiszWynikiSkalowaniaNaDysku,
                                   maxNIter = maxNIter,
                                   uzyjRozkladowAPrioriDlaZadan3PL = uzyjRozkladowAPrioriDlaZadan3PL,
                                   kryteriaUsuwaniaZadan = kryteriaUsuwaniaZadan)
      parametry$grupy = bind_rows(parametry$grupy,
                                  parametryRok$grupy)
      parametry$parametry = bind_rows(parametry$parametry,
                                      parametryRok$parametry)
      parametry$kowariancje = bind_rows(parametry$kowariancje,
                                        parametryRok$kowariancje)
      usunieteZadania = c(usunieteZadania, parametryRok$usunieteZadania)
    } # koniec pętli skalowania po latach egzaminu
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
    nazwaObiektu = paste0("ewd", rokEWD, "SkalowanieWe")
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
#' @importFrom stats na.omit
#' @importFrom dplyr %>% .data all_of filter pull select
skaluj_we_rok = function(r, opis_skali, dane, zadania,
                         nadpiszWynikiSkalowaniaNaDysku, maxNIter,
                         uzyjRozkladowAPrioriDlaZadan3PL,
                         kryteriaUsuwaniaZadan) {
  message("  Skalowanie egzaminu w roku ", r)
  zadania = zadania %>%
    filter(.data$rok == r)
  zadania3pl = zadania %>%
    filter(.data$typ_pytania %in% c("jednokrotnego wyboru", "pseudootwarte")) %>%
    pull("kryterium")
  dane = dane %>%
    filter(.data$rok == r) %>%
    select(-all_of(setdiff(grep("^([kp]|t[[:digit:]]+)_", names(dane),
                                value = TRUE),
                           c(zadania$kryterium,
                             na.omit(zadania$zmienna_temat)))))
  wyniki = skaluj_az_dobrze(dane, zadania3pl,
                            nazwa = paste0(gsub(";", "-", opis_skali), "-", r),
                            nadpiszWynikiSkalowaniaNaDysku = nadpiszWynikiSkalowaniaNaDysku,
                            maxNIter = maxNIter, usunUjemneDyskryminacje = TRUE,
                            uzyjRozkladowAPrioriDlaZadan3PL = uzyjRozkladowAPrioriDlaZadan3PL,
                            kryteriaUsuwaniaZadan = kryteriaUsuwaniaZadan)
  wyniki$parametry$idTemp = paste0(wyniki$parametry$idTemp, "_", r)
  wyniki$kowariancje$idTemp = paste0(wyniki$kowariancje$idTemp, "_", r)
  wyniki$kowariancje$idTemp2 = paste0(wyniki$kowariancje$idTemp2, "_", r)
  return(list(grupy = wyniki$grupy,
              parametry = wyniki$parametry,
              kowariancje = wyniki$kowariancje,
              usunieteZadania = wyniki$usunieteZadania))
}
