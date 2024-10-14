#' @title Wczytywanie wynikow egzaminow zapisanych na dysku
#' @description
#' Funkcja wczytuje wyniki surowe egzaminu, zapisane wcześniej na dysku
#' funkcją [EWDdane::pobierz_wyniki_surowe()] z pakietu *EWDdane*.
#' @param katalogDane ciąg znaków - ścieżka do katalogu, w którym znajdują
#' się dane z wynikami surowymi egzaminów, pobranymi przy pomocy funkcji
#' [EWDdane::pobierz_wyniki_surowe()]
#' @param rodzajEgzaminu ciąg znaków
#' @param rok liczba naturalna
#' @param idSkali liczba naturalna - id_skali w bazie dla skali, która ma zostać
#' zastosowana do wyników surowych
#' @param kryteria opcjonalnie wektor tekstowy z nazwami wszystkich
#' kryteriów oceny, jakie powinna zawierać dana część/ci egzaminu
#' @param usunObserwacjeBezWynikow wartość logiczna wskazująca, czy
#' z przygotowanego zbioru powinny zostać usunięte obserwacje, które nie mają
#' żadnych wyników (niebędących brakami danych) kryteriów oceny przypisanej do
#' danej skali (i ew. zawężonych do podanych argumentem `kryteria`) - domyślnie
#' prawda (w praktyce ma zastosowanie w 2023 r. w związku z przeprowadzeniem
#' w jednym roku matur w starszej i nowszej formule)
#' @param src połączenie z bazą danych IBE zwracane przez funkcję [ZPD::polacz()];
#' jeśli nie podane, podjęta zostanie próba automatycznego nawiązania połączenia
#' (poprzez wywoływanie funkcji [ZPD::polacz()] z domyślnymi argumentami)
#' @details
#' Funkcja co do zasady dołącza do wyników egzaminu dane kontekstowe, zawężając
#' grupę zwracanych obserwacji do tych, dla których te dane istnieją. Jeśli
#' jednak stwierdzi, że w efekcie usunięci zostaliby wszyscy, to zwróci wyniki,
#' bez dołączania do nich danych kontekstowych. Dojdzie do tego w szczególności
#' przy wczytywaniu wyników egzaminu gimnazjalnego z lat wcześniejszych niż
#' 2006 r., gdyż są to dane CKE i dane pobierane funkcją
#' [EWDdane::pobierz_dane_kontekstowe()] ich nie obejmują.
#' @return data frame (data table)
#' @seealso [ZPD::pobierz_skale()], [ZPD::pobierz_testy()],
#' [ZPD::zastosuj_skale()], [przygotuj_dane_do_skalowania()]
#' @importFrom stats setNames
#' @importFrom dplyr %>% .data collect distinct filter full_join inner_join matches pick pull rename select semi_join
wczytaj_wyniki_surowe = function(katalogDane, rodzajEgzaminu,
                                 rok, idSkali, kryteria = NULL,
                                 usunObserwacjeBezWynikow = TRUE, src = NULL) {
  stopifnot(is.character(katalogDane), length(katalogDane) == 1,
            is.character(rodzajEgzaminu), length(rodzajEgzaminu) == 1,
            is.numeric(rok),
            is.numeric(idSkali), length(idSkali) == 1,
            is.character(kryteria) | is.null(kryteria),
            is.logical(usunObserwacjeBezWynikow), length(usunObserwacjeBezWynikow) == 1,
            usunObserwacjeBezWynikow %in% c(TRUE, FALSE),
            dplyr::is.src(src) | is.null(src))
  stopifnot(dir.exists(katalogDane),
            rodzajEgzaminu %in% c("sprawdzian", "egzamin gimnazjalny", "matura",
                                  "egzamin ósmoklasisty"))
  if (is.null(src)) {
    src = ZPD::polacz()
    on.exit(ZPD::rozlacz(src))
  }

  katalogDane = paste0(sub("/$", "", katalogDane), "/")
  plikDane = paste0(katalogDane, rodzajEgzaminu, " ", rok, ".RData")
  if (!file.exists(plikDane)) {
    stop("Nie można wczytać danych z pliku '", plikDane, "'. Plik nie istnieje.")
  }
  obiekty = load(plikDane)
  idTestu = suppressMessages(
    ZPD::pobierz_skale(src, doPrezentacji = NA) %>%
      rename(rok_testu = "rok") %>%
      filter(.data$id_skali == idSkali, .data$rok_testu == rok) %>%
      select("id_testu") %>%
      semi_join(ZPD::pobierz_testy(src) %>% filter(!.data$czy_egzamin)) %>%
      distinct() %>%
      collect() %>%
      pull()
  )
  if (length(idTestu) > 1) {
    stop("Jeśli skala jest powiązana z testem nie będącym 'atomową' częścią egzaminu, ",
         "to musi być powiązana z tylko jednym takim testem w danym roku.")
  }
  for (i in obiekty) {
    temp = suppressMessages(ZPD::zastosuj_skale(get(i), src, idSkali))
    # Jeśli skala ma przypisanych test który nie jest częścią egzaminu, to
    # trzeba usunąć z danych "pierwotne" id_testu poszczególnych części egzaminu
    # i zastąpić je id_testu właśnie tego testu (co dzieje się kawałek dalej).
    if (length(idTestu) == 1) {
      temp = temp[, !(names(temp) %in% "id_testu"), drop = FALSE]
    }
    maska1 = grepl("^[kp]_[[:digit:]]+$", names(temp))
    if (!is.null(kryteria)) {
      maska2 = all(names(temp)[maska1] %in% kryteria)
    } else {
      maska2 = TRUE
    }
    if (any(maska1) & all(maska2) &
        all(c("wynikiSurowe", "czescEgzaminu") %in% class(get(i)))) {
      if (!exists("dane", environment(), inherits = FALSE)) {
        assign("dane", temp)
      } else {
        dane = suppressMessages(full_join(get("dane"), temp))
        if (all(kryteria %in% names(dane)) & !is.null(kryteria)) {
          break
        }
      }
    }
  }
  if (!exists("dane", environment(), inherits = FALSE)) {
    stop("W pliku '", plikDane, "' nie ma obiektu, który zawierałby wyniki ",
         "wszystkich (pseudo)kryteriów oceny egzaminu '", rodzajEgzaminu,
         "' przypisanych do danej skali (w roku", rok, ").")
  }
  rm(list = c(obiekty, "obiekty", "temp"))
  if (usunObserwacjeBezWynikow) {
    n = nrow(dane)
    dane = dane %>%
      filter((pick(matches("^[kp]_")) %>%
                is.na() %>%
                rowMeans()) < 1)
    if (nrow(dane) < n) {
      message("  Usunięto ", format(n - nrow(dane), big.mark = "'"),
              " obserwacji, które nie miały wyniku żadnego kryterium tej skali.")
    }
  }
  if (length(idTestu) == 1) {
    dane$id_testu =  idTestu
  }
  # wczytywanie danych kontekstowych i filtrowanie populacji "wzorcowej"
  plikDane = paste0(katalogDane, rodzajEgzaminu, "-kontekstowe.RData")
  if (!file.exists(plikDane)) {
    stop("Nie można wczytać danych z pliku '", plikDane, "'. Plik nie istnieje.")
  }
  obiekty = load(plikDane)
  maska = grepl("^[[:alpha:]]Kontekstowe$", obiekty)
  if (!any(maska)) {
    stop("W pliku '", plikDane, "' brak obiektu zawierającego dane kontekstowe.")
  } else if (!("daneKontekstowe" %in% class(get(obiekty[maska])))) {
    stop("W pliku '", plikDane, "' brak obiektu zawierającego dane kontekstowe.")
  }
  daneKontekstowe = get(obiekty[maska])
  rm(list = c(obiekty, "obiekty", "maska"))
  temp = suppressMessages(inner_join(daneKontekstowe, dane))
  if (nrow(temp) > 0) {
    dane = temp
  } else {
    warning("Nie udało się przyłączyć zbioru danych kontekstowych do zbioru z wynikami surowymi.",
            immediate. = TRUE)
  }
  return(dane)
}
