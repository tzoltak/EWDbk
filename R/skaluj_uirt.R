#' @importFrom stats relevel
#' @importFrom utils read.csv
#' @importFrom dplyr %>% .data arrange count desc mutate pull select slice
skaluj_uirt = function(dane, zadania3pl, nazwa, nadpisz = FALSE,
                       maxNIter = 1000L, usunUjemneDyskryminacje = TRUE,
                       uzyjRozkladowAPrioriDlaZadan3PL = list(a = c(1.5, 2),
                                                              b = c(0,   2.5),
                                                              c = c(2.5, 5.5))) {
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
  if (!file.exists(nazwaPlikuZWynikami) | nadpisz) {
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
            "Jeśli chcesz przeprowadzić skalowanie, usuń ten plik lub wywołaj funkcję z arkgumentem `nadpisz=TRUE`.")
  }
  parametry = read.csv(nazwaPlikuZWynikami, stringsAsFactors = FALSE)
  parametry = obrob_wyniki_uirt(parametry, dane)
  parametry$usunieteZadania = setdiff(zadania, parametry$parametry$kryterium)
  return(parametry)
}
