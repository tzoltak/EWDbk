#' @importFrom stats na.omit
#' @importFrom dplyr any_of select
# pętla skalowania z usuwaniem kryteriów oceny, dla których nie udało się
# uirtowi (poprawnie) oszacować wariancji - co do zasady z powodu
# okołozerowej dyskryminacji
skaluj_az_dobrze = function(dane, zadania3pl, nazwa,
                            nadpiszWynikiSkalowaniaNaDysku,
                            maxNIter, usunUjemneDyskryminacje,
                            uzyjRozkladowAPrioriDlaZadan3PL = list(a = c(1.5, 2),
                                                                   b = c(0,   2.5),
                                                                   c = c(2.5, 5.5)),
                            kryteriaUsuwaniaZadan = list(absA = 0.1,
                                                         absB = 10)) {
  usunieteZadania = vector(mode = "character", length = 0)
  wyniki = list(parametry = data.frame(bs = NaN))
  zleZadania = NA_character_
  j = 1
  while (length(zleZadania) > 0) {
    message("  ", j, ". uruchomienie uirt",
            ifelse(length(usunieteZadania) > 0,
                   paste0(" (usunięte kryteria oceny: ",
                          paste(usunieteZadania, collapse = ", "), ")"),
                   ""))
    dane = dane %>%
      select(-any_of(usunieteZadania))
    zadania3pl = intersect(zadania3pl, names(dane))
    wyniki = suppressWarnings(
      skaluj_uirt(dane = dane, zadania3pl = zadania3pl, nazwa = paste0(nazwa, "-", j),
                  nadpisz = nadpiszWynikiSkalowaniaNaDysku, maxNIter = maxNIter,
                  usunUjemneDyskryminacje = usunUjemneDyskryminacje,
                  uzyjRozkladowAPrioriDlaZadan3PL = uzyjRozkladowAPrioriDlaZadan3PL))
    zleZadania =
      zwroc_zadania_niespelniajace_kryteriow(wyniki$parametry,
                                             absA = kryteriaUsuwaniaZadan$absA,
                                             absB = kryteriaUsuwaniaZadan$absB,
                                             bsNotNaN = FALSE)
    if (length(zleZadania) == 0) {
      zleZadania =
        zwroc_zadania_niespelniajace_kryteriow(wyniki$parametry,
                                               absA = kryteriaUsuwaniaZadan$absA,
                                               absB = kryteriaUsuwaniaZadan$absB,
                                               bsNotNaN = TRUE)
      if (length(zleZadania) == 1 && NA %in% zleZadania) {
        zleZadania = vector(mode = "character", length = 0)
        warning("Na podstawie podanych kryteriów nie ma zadań do usunięcia, ale któryś z parametrów grupowych wciąż ma estymowaną ujemną wariancję.",
                immediate. = TRUE)
      } else if (length(zleZadania) > 0) {
        message("Na podstawie podanych kryteriów nie ma zadań do usunięcia, ale jakieś parametry zadań wciąż mają estymowane ujemne wariancje.")
      }
    }
    usunieteZadania = c(usunieteZadania, wyniki$usunieteZadania,
                        na.omit(zleZadania))
    j = j + 1
  }
  return(list(grupy = wyniki$grupy,
              parametry = wyniki$parametry,
              kowariancje = wyniki$kowariancje,
              usunieteZadania = usunieteZadania))
}
# Uwaga, w wynikowym wektorze morze się znaleźć NA, co oznacza, że problem
# dotyczy parametru grupowego!
zwroc_zadania_niespelniajace_kryteriow = function(parametry,
                                                  absA = 0.1, absB = 10,
                                                  bsNotNaN = FALSE) {
  zleZadania = c(
    parametry$kryterium[abs(parametry$wartosc) < absA &
                          parametry$parametr %in% "a"],
    parametry$kryterium[abs(parametry$wartosc) > absB &
                          grepl("^b[[:digit:]]*$", parametry$parametr)])
  if (bsNotNaN) {
    zleZadania = c(zleZadania,
                   parametry$kryterium[is.nan(parametry$bs)])
  }
  return(unique(zleZadania))
}
