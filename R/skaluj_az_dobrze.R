#' @title Skalowanie wynikow egzaminow w Stacie przy pomocy pakietu uirt
#' @description
#' Skaluje wyniki egzaminu w pętli aż uda się spełnić podane argumentami
#' kryteria *jakości* zadań (tzn. żadne zadanie nie zostanie usunięte na
#' podstawie wyestymowanych w *uirt* wartości parametrów).
#' @inheritParams skaluj_uirt
#' @param kryteriaUsuwaniaZadan dwuelementowy wektor liczbowy, o nazwach
#' elementów `absA` i `absB` - zadania, których wyestymowane wartości
#' bezwzględne parametrów będą, odpowiednio: dyskryminacji (`absA`) - mniejsze,
#' niż podana wartość lub trudności (`absB`) - większe, niż podana wartość,
#' zostaną usunięte z estymacji; usuwanie zadań można efektywnie wyłączyć
#' poprzez podanie argumentu `kryteriaUsuwaniaZadan=c(absA=0, absB=Inf)`, ale
#' nie jest to dobry pomysł, gdyż używana przez *uirt* metoda szacowania błędów
#' standardowych parametrów zadań nie działa dobrze dla zadań z dyskryminacją
#' bliską 0 (co zwykle idzie w parze z bardzo dużymi co do wartości bezwzględnej
#' oszacowaniami trudności); co do zasady domyślną wartość tego argumentu należy
#' uznać za rozsądną; uwaga, niezależnie od podanej wartości `absA` ***uirt*
#' zawsze będzie usuwał zadania o ujemnych wartościach dyskryminacji!**
#' @details
#' Zbyt niskie (bliskie zera; zadania o ujemnej dyskryminacji każemy *uirt*-owi
#' usuwać automatycznie) wartości dyskryminacji sprawiają trudności przy
#' szacowaniu (ko)wariancji parametrów zadań metodą stosowaną przez *uirt*
#' (choć skądinąd daje ona lepsze oszacowania tych (ko)wariancji, jeśli zadania
#' mają rozsądne własności pomiarowe). Funkcja pozwala dojść do takiego modelu,
#' który nie tylko się wyestymuje, ale również będzie miał kompletne
#' (i rozsądne) oszacowania (ko)wariancji parametrów modelu - jest to ważne,
#' gdyż również te (ko)wariancje będą potem wykorzystywane przy estymacji
#' wskaźników EWD przez *pvreg*.
#' @inherit skaluj_uirt return
#' @seealso [skaluj_uirt()], [zwroc_zadania_niespelniajace_kryteriow()],
#' [skaluj_matura_bk()], [skaluj_we_bk()]
#' @importFrom stats na.omit
#' @importFrom dplyr any_of select
skaluj_az_dobrze = function(dane, zadania3pl, nazwa,
                            nadpiszWynikiSkalowaniaNaDysku,
                            maxNIter, usunUjemneDyskryminacje,
                            uzyjRozkladowAPrioriDlaZadan3PL = list(a = c(1.5, 2),
                                                                   b = c(0,   2.5),
                                                                   c = c(2.5, 5.5)),
                            kryteriaUsuwaniaZadan = c(absA = 0.1,
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
                  nadpiszWynikiSkalowaniaNaDysku = nadpiszWynikiSkalowaniaNaDysku,
                  maxNIter = maxNIter,
                  usunUjemneDyskryminacje = usunUjemneDyskryminacje,
                  uzyjRozkladowAPrioriDlaZadan3PL = uzyjRozkladowAPrioriDlaZadan3PL))
    zleZadania =
      zwroc_zadania_niespelniajace_kryteriow(wyniki$parametry,
                                             absA = kryteriaUsuwaniaZadan["absA"],
                                             absB = kryteriaUsuwaniaZadan["absB"],
                                             bsNotNaN = FALSE)
    if (length(zleZadania) == 0) {
      zleZadania =
        zwroc_zadania_niespelniajace_kryteriow(wyniki$parametry,
                                               absA = kryteriaUsuwaniaZadan["absA"],
                                               absB = kryteriaUsuwaniaZadan["absB"],
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
#' @title Skalowanie wynikow egzaminow w Stacie przy pomocy pakietu uirt
#' @description
#' Zwraca nazwy zadań, których wyestymowane parametry nie spełniają podanych
#' kryteriów.
#' @param parametry ramka danych z wyestymowanymi wartościami parametrów zadań
#' @param absA liczba nieujemna - minimalna wartość bezwzględna dyskryminacji
#' @param absB liczba nieujemna - maksymalna wartość bezwzględna trudności
#' @param bsNotNaN wartość logiczna - czy jako niespełniające kryteriów
#' traktować także zadania, dla których nie udało się wyestymować błędu
#' standardowego któregoś z parametrów?
#' @return ciąg znaków z nazwami zadań
#' @seealso [skaluj_az_dobrze()]
# Uwaga, w wynikowym wektorze może się znaleźć NA, co oznacza, że problem
# dotyczy parametru grupowego!
zwroc_zadania_niespelniajace_kryteriow = function(parametry,
                                                  absA = 0.1, absB = 10,
                                                  bsNotNaN = FALSE) {
  stopifnot(is.data.frame(parametry),
            is.numeric(absA), length(absA) == 1, !anyNA(absA), absA >= 0,
            is.numeric(absB), length(absB) == 1, !anyNA(absB), absB >= 0,
            is.logical(bsNotNaN), length(bsNotNaN) == 1,
            bsNotNaN %in% c(TRUE, FALSE))
  stopifnot(all(c("kryterium", "parametr", "wartosc") %in% names(parametry)))
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
