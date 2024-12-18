% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dodaj_wybory_tematow.R
\name{dodaj_wybory_tematow}
\alias{dodaj_wybory_tematow}
\title{Przetwarzanie danych z wynikami egzaminow}
\usage{
dodaj_wybory_tematow(dane, czesciEgzaminow, echo = TRUE)
}
\arguments{
\item{dane}{ramka danych z wynikami egzaminu}

\item{czesciEgzaminow}{ramka danych z charakterystykami (pseudo)kryteriów
oceny skali (występujących w wynikach przekazanych argumentem \code{dane}),
zwrócona przez [przygotuj_mapowanie_kryteriow_na_czesci_egzaminu())]

[przygotuj_mapowanie_kryteriow_na_czesci_egzaminu())]: R:przygotuj_mapowanie_kryteriow_na_czesci_egzaminu())}

\item{echo}{wartość logiczna - czy wyświetlić na konsoli tabelę z rozkładami
wyboru tematów wypracowania}
}
\value{
ramka danych przekazana argumentem \code{dane}, z dodanymi (jeśli w danej
skali występuje rozprawka/rozprawki) zmiennymi opisującymi wybór tematów
rozprawek (wypracowań)
}
\description{
Funkcja dodaje do danych z wynikami egzaminu zmienne opisujące wybór tematów
rozprawki (wypracowania).
}
\seealso{
\code{\link[=przygotuj_zmienne_tematy_w_ramach_czesci_egzaminu]{przygotuj_zmienne_tematy_w_ramach_czesci_egzaminu()}},
\code{\link[=przygotuj_dane_do_skalowania]{przygotuj_dane_do_skalowania()}}
}
