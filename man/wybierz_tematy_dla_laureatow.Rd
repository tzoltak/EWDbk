% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wybierz_temat_dla_laureatow.R
\name{wybierz_tematy_dla_laureatow}
\alias{wybierz_tematy_dla_laureatow}
\title{Przygotowywanie danych do obliczenia wskaznikow EWD}
\usage{
wybierz_tematy_dla_laureatow(parametry)
}
\arguments{
\item{parametry}{ramka danych z wartościami parametrów modelu, zwrócona przez
\code{\link[ZPD:pobierz_parametry]{ZPD::pobierz_parametry()}}}
}
\value{
ramka danych z przypisaniem nazwy zmiennej opisującej temat
rozprawki, której wartość należy laureatom ustawić na 1, do części egzaminów
(p. sekcja \emph{details} w celu zapoznania się ze sposobem obsługi szczególnych
przypadków)
}
\description{
Określa, którym ze zmiennych opisujących wybór tematów rozprawki należy
ustawić wartość 1 dla laureatów w poszczególnych częściach egzaminu (tak aby
skutkowało to maksymalizacją oszacowań umiejętności laureatów).
}
\details{
Funkcja robi najlepsze, co może, mając dostęp tylko do parametrów modelu.
Zwraca ramkę danych o 0 wierszach, jeśli w parametrach nie widzi żadnych
tematów, ale może też zwrócić pusty ciąg znaków w kolumnie \code{temat}, jeśli
w ramach danej części egzaminu należy wybrać "temat odniesienia" (a więc ten,
który "nie ma swojej zmiennej") lub temat, którego "zmienna selekcyjna"
została usunięta z modelu skalowania.
}
\seealso{
\code{\link[=pobierz_parametry_egzaminow]{pobierz_parametry_egzaminow()}},
\code{\link[=przygotuj_mapowanie_kryteriow_na_czesci_egzaminu]{przygotuj_mapowanie_kryteriow_na_czesci_egzaminu()}}
}
