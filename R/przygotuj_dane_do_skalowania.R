#' @title Przetwarzanie danych z wynikami egzaminow
#' @description
#' Funkcja przygotowuje dane do skalowania, wywołując kolejno bardziej
#' specjalizowane funkcje odpowiedzialne za obsłużenie laureatów, dodanie
#' zmiennych opisujących wybór tematów oraz przypisanie zdających do grup.
#' @param skala ramka danych o jednym wierszy zawierająca kolumny
#' *id_skali*, *rodzaj_egzaminu*, *lata* i *typ_szkoly* (co do zasady pojedynczy
#' wiersz nieco przekształconego efektu wywołania [znajdz_skale()])
#' @inheritParams skaluj_matura_bk
#' @inheritParams przygotuj_mapowanie_kryteriow_na_czesci_egzaminu
#' @inheritParams usun_wyniki_laureatow
#' @param laureaci - ciąg znaków opisujący, w jaki sposób postąpić z wynikami
#' laureatów
#' @param echo wartość logiczna - czy wyświetlić na konsoli tabele z rozkładami
#' wyboru tematów wypracowania i przypisania zdających do grup
#' @return ramka danych z wynikami egzaminu oraz dodatkowymi zmiennymi:
#' ew. *pseudozadaniami* opisującymi wybór tematów wypracowania oraz zmienną
#' opisującą przypisanie zdających do grup
#' @seealso [wczytaj_wyniki_surowe()],
#' [przygotuj_mapowanie_kryteriow_na_czesci_egzaminu()],
#' [usun_wyniki_laureatow()], [dodaj_wybory_tematow()],
#' [okresl_poziom_egzaminu()], [okresl_grupe()], [skaluj_matura_bk()],
#' [skaluj_we_bk()], [przygotuj_dane_do_ewd_bk()]
#' @importFrom dplyr %>% bind_rows
przygotuj_dane_do_skalowania = function(skala, rokEWD, katalogSurowe,
                                        dodajRokDoNazwTematow = FALSE,
                                        laureaci = c("usuń", "zostaw",
                                                     "usuń rozprawki"),
                                        echo = TRUE,
                                        src = NULL) {
  laureaci = match.arg(laureaci)
  stopifnot(is.data.frame(skala), nrow(skala) == 1,
            "rodzaj_egzaminu" %in% names(skala),
            is.numeric(rokEWD), length(rokEWD) == 1,
            is.character(katalogSurowe), length(katalogSurowe) == 1,
            is.logical(dodajRokDoNazwTematow),
            length(dodajRokDoNazwTematow) == 1,
            is.logical(echo), length(echo) == 1, echo %in% c(TRUE, FALSE))
  stopifnot(echo %in% c(TRUE, FALSE))
  if (skala$rodzaj_egzaminu != "matura") {
    stopifnot("lata" %in% names(skala))
    lata = skala$lata[[1]]
    stopifnot(is.numeric(lata), length(lata) > 0)
  } else {
    lata = rokEWD
  }

  dane = czesciEgzaminu = vector(mode = "list", length = length(lata))
  for (i in seq_along(dane)) {
    dane[[i]] = wczytaj_wyniki_surowe(katalogDane = katalogSurowe,
                                      rodzajEgzaminu = skala$rodzaj_egzaminu,
                                      rok = lata[i], idSkali = skala$id_skali,
                                      src = src)
    # mapowanie kryteriów na części egzaminu (i inne przydatne informacje)
    czesciEgzaminu[[i]] =
      przygotuj_mapowanie_kryteriow_na_czesci_egzaminu(skala$id_skali,
                                                       dodajRokDoNazwTematow = dodajRokDoNazwTematow,
                                                       src = src) %>%
      filter(.data$kryterium %in% names(dane[[i]]))
    # obsługa laureatów
    if (laureaci %in% c("usuń", "usuń rozprawki")) {
      dane[[i]] = usun_wyniki_laureatow(dane[[i]], czesciEgzaminu[[i]],
                                        tylkoRozprawki = laureaci %in% "usuń rozprawki",
                                        usunPusteWiersze = TRUE)
    }
    # ew. tworzenie zmiennych opisujących wybór tematów
    if (any(!is.na(czesciEgzaminu[[i]]$temat))) {
      dane[[i]] = dodaj_wybory_tematow(dane[[i]], czesciEgzaminu[[i]],
                                       echo = echo)
    }
    if (skala$rodzaj_egzaminu == "matura") {
      # określanie poziomów zdawanego egzaminu
      dane[[i]] = okresl_poziom_egzaminu(dane[[i]], czesciEgzaminu[[i]],
                                         zmienNazweGdyTylkoJednaCzesc = TRUE)
      # dodawanie grupowania
      dane[[i]] = okresl_grupe(dane[[i]], rokWy = rokEWD,
                               katalogSurowe = katalogSurowe, echo = echo)
    } else if (skala$rodzaj_egzaminu %in% c("egzamin gimnazjalny",
                                            "egzamin ósmoklasisty")) {
      # dodawanie grupowania
      dane[[i]] = okresl_grupe(dane[[i]], rokWy = rokEWD,
                               katalogSurowe = katalogSurowe,
                               typSzkoly = skala$typ_szkoly, rokWe = lata[i],
                               echo = echo)
    }
  }
  dane = bind_rows(dane)
  czesciEgzaminu = bind_rows(czesciEgzaminu)

  attributes(dane)$czesciEgzaminu = czesciEgzaminu
  return(dane)
}
