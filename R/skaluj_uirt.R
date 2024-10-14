#' @title Skalowanie wynikow egzaminow w Stacie przy pomocy pakietu uirt
#' @description
#' Skaluje wyniki egzaminu uruchamiając funkcje pakietu *uirt* w Stacie.
#' @param dane ramka danych z wynikami egzaminu do wyskalowania - co do zasady
#' zwrócona przez [przygotuj_dane_do_skalowania()]
#' @param zadania3pl wektor ciągów znaków - nazwy (pseudo)kryteriów oceny
#' (zmiennych w ramce danych przekazanej argumentem `dane`), w odniesieniu do
#' których *uirt* ma testować występowanie zgadywania (użycie modelu 3PL)
#' @param nazwa ciąg znaków - podstawa do tworzenia nazw plików, w których
#' zapisane zostaną zarówno dane i kod Staty, jak i wyniki skalowania
#' wyeksportowane ze Staty
#' @param nadpiszWynikiSkalowaniaNaDysku wartość logiczna - czy napotkawszy
#' zapisane na dysku wyniki skalowania zapisane przez *uirt* funkcja powinna
#' przeprowadzić skalowanie i je nadpisać (`TRUE`), czy wczytać już istniejące,
#' nie przeprowadzając skalowania (`FALSE`)?
#' @param maxNIter liczba (naturalna) - maksymalna liczba iteracji estymacji
#' *uirt* w *Stacie*
#' @param usunUjemneDyskryminacje wartość logiczna - czy *uirt* powinien
#' automatycznie usuwać z modelu zadania o ujemnych dyskryminacjach?
#' @param uzyjRozkladowAPrioriDlaZadan3PL lista z wektorami dwuelementowymi
#' w formacie `c(wartoscOczekiwana, odchylenieStandardowe)` rozkładów *a priori*
#' parametrów zadań 3PL, używanych przez *uirt* w estymacji; lista może zawierać
#' elementy o nazwach `a`, `b` lub `c` - najlepiej wszystkie z nich; jeśli
#' *uirt* ma nie używać rozkładów a priori dla zadań 3PL, należy jako wartość
#' tego argumentu podać `NULL` (ale co do zasady nie jest to dobre rozwiązanie);
#' co do zasady domyślną wartość tego argumentu należy uznać za rozsądną
#' @return lista z wynikami skalowania zawierająca następujące elementy:
#' *grupy* (ramka danych), *parametry* (ramka danych), *kowariancje*
#' (ramka danych), *usunieteZadania* (wektor ciągów znaków, może mieć zerową
#' długość)
#' @seealso [obrob_wyniki_uirt()], [skaluj_az_dobrze()]
#' @export
#' @importFrom stats relevel
#' @importFrom utils read.csv
#' @importFrom dplyr %>% .data arrange count desc mutate pull select slice
skaluj_uirt = function(dane, zadania3pl, nazwa,
                       nadpiszWynikiSkalowaniaNaDysku = FALSE,
                       maxNIter = 1000L, usunUjemneDyskryminacje = FALSE,
                       uzyjRozkladowAPrioriDlaZadan3PL = list(a = c(1.5, 2),
                                                              b = c(0,   2.5),
                                                              c = c(2.5, 5.5))) {
  stopifnot(is.data.frame(dane),
            is.character(zadania3pl), !anyNA(zadania3pl),
            is.character(nazwa), length(nazwa) == 1,
            !anyNA(nazwa), nazwa != "",
            is.logical(nadpiszWynikiSkalowaniaNaDysku),
            length(nadpiszWynikiSkalowaniaNaDysku) == 1,
            nadpiszWynikiSkalowaniaNaDysku %in% c(TRUE, FALSE),
            is.numeric(maxNIter), length(maxNIter) == 1, maxNIter > 0,
            is.list(uzyjRozkladowAPrioriDlaZadan3PL) || is.null(uzyjRozkladowAPrioriDlaZadan3PL))
  stopifnot(as.integer(maxNIter) == maxNIter,
            all(zadania3pl %in% names(dane)))
  if (is.list(uzyjRozkladowAPrioriDlaZadan3PL)) {
    stopifnot(all(names(uzyjRozkladowAPrioriDlaZadan3PL) %in% c("a", "b", "c")),
              !duplicated(names(uzyjRozkladowAPrioriDlaZadan3PL)),
              all(sapply(uzyjRozkladowAPrioriDlaZadan3PL, is.numeric)),
              all(sapply(uzyjRozkladowAPrioriDlaZadan3PL, length) == 2),
              all(sapply(uzyjRozkladowAPrioriDlaZadan3PL, is.finite)))
  }
  zadania = grep("^([kp]|t[[:digit:]]+)_", names(dane), value = TRUE)
  if (length(zadania) < 2) stop("Dane zawierają wyniki mniej niż dwóch zmiennych, które mają być uwzględnione w skalowaniu (tj. takich, których nazwa pasuje do wyrażenia regularnego '^([kp]|t[[:digit:]]+)_'.")
  zadaniaBezWariancji = 2 > apply(dane[, zadania, drop = FALSE], 2,
                                  function(x) {
                                    return(length(unique(x[!is.na(x)])))})
  if (any(zadaniaBezWariancji)) {
    warning("Zmienne:-n- ", paste(zadania[zadaniaBezWariancji], collapse = ",\n- "),
            "\nprzyjmują w danych mniej niż dwie różne wartości (nie będące brakami danych). Uwzględnienie ich w skalowaniu będzie niemożliwe.",
            immediate. = TRUE)
  }

  najliczniejszaGrupa = dane %>%
    count(.data$grupa, name = "n") %>%
    arrange(desc(.data$n)) %>%
    slice(1) %>%
    pull(.data$grupa)
  dane = dane %>%
    mutate(grupa = factor(.data$grupa) %>%
             relevel(levels(.data$grupa)[najliczniejszaGrupa]))

  nazwaPlikuZWynikami = paste0(nazwa, "_estimates.csv")
  if (!file.exists(nazwaPlikuZWynikami) | nadpiszWynikiSkalowaniaNaDysku) {
    haven::write_dta(dane %>%
                       select("id_obserwacji", "grupa", all_of(zadania)) %>%
                       mutate(grupa = factor(.data$grupa)),
                     paste0(nazwa, ".dta"))
    writeLines(paste0('*log using log_irt_', nazwa, ', replace

use ', nazwa, '.dta, clear
compress
save ', nazwa, '.dta, replace
local egz_items = "', paste(zadania, collapse = " "), '"
local guess_items = "', paste(zadania3pl, collapse = " "), '"

di in red "Dla tych zadań będzie szacowany parametr c: `guess_items\'

uirt `egz_items\', gr(grupa, slow) guess(`guess_items\') nit(', maxNIter, ') not',
ifelse(usunUjemneDyskryminacje, "", " aneg"),
ifelse(!is.null(uzyjRozkladowAPrioriDlaZadan3PL),
       paste0(" priors(`guess_items', a(", paste(uzyjRozkladowAPrioriDlaZadan3PL$a, collapse = ","),
              ") b(", paste(uzyjRozkladowAPrioriDlaZadan3PL$b, collapse = ","),
              ") c(", paste(uzyjRozkladowAPrioriDlaZadan3PL$c, collapse = ","),"))"),
       ""), '

export_uirt, prefix(', nazwa, ')

mat l e(group_par)
mat l e(item_par)

cap mkdir ', nazwa, '-ICC
cd ', nazwa, '-ICC
uirt_icc *,tw(xtitle({&theta} - ', nazwa, '))
cd ../

*log close'), paste0(nazwa, ".do"))

    skalowanie = system2("StataMP-64", args = paste0("/e do ", nazwa, ".do"))
    if (skalowanie != 0) stop("Nie udało się poprawnie przeprowadzić skalowanie przy pomocy Staty - sprawdź plik z logiem Staty.")
  } else {
    message("W katalogu znajduje się już plik z wynikami skalowania o nazwie '",
            nazwaPlikuZWynikami, "'. Zostanie on wczytany zamiast przeprowadzania skalowania.\n",
            "Jeśli chcesz przeprowadzić skalowanie, usuń ten plik lub wywołaj funkcję z arkgumentem `nadpiszWynikiSkalowaniaNaDysku=TRUE`.")
  }
  parametry = read.csv(nazwaPlikuZWynikami, stringsAsFactors = FALSE)
  parametry = obrob_wyniki_uirt(parametry, dane)
  parametry$usunieteZadania = setdiff(zadania, parametry$parametry$kryterium)
  return(parametry)
}
