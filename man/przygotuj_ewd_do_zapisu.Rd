% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/przygotuj_ewd_do_zapisu.R
\name{przygotuj_ewd_do_zapisu}
\alias{przygotuj_ewd_do_zapisu}
\title{Przetwarzanie i zapis wynikow obliczania latentnych wskaznikow EWD}
\usage{
przygotuj_ewd_do_zapisu(
  ewd,
  nazwaWskaznika,
  rokEWD,
  dane,
  skalowania,
  luWszyscy,
  src
)
}
\arguments{
\item{ewd}{ramka danych z oszacowaniami EWD wyestymowanymi przez \emph{pvreg}
(element \emph{EWD} listy zwróconej przez \code{\link[=estymuj_pvreg]{estymuj_pvreg()}})}

\item{nazwaWskaznika}{ciąg znaków - nazwa obliczonego wskaźnika EWD}

\item{rokEWD}{rok egzaminu maturalnego (identyfikujący rocznik absolwentów)}

\item{dane}{lista ramek danych zwrócona przez \code{\link[=przygotuj_dane_do_ewd_bk]{przygotuj_dane_do_ewd_bk()}}}

\item{skalowania}{ramka danych o dwóch wierszach zawierająca informacje
o skalach i numerach skalowań, który powinny zostać przypisane wynikom
estymacji z wykorzystaniem \emph{pvreg} (w bazie danych zapisywane jest również
powiązanie między wskaźnikami EWD a skalowaniami)}

\item{luWszyscy}{ramka danych zwrócona przez
\code{\link[=okresl_liczbe_uczniow_w_szkolach]{okresl_liczbe_uczniow_w_szkolach()}}}

\item{src}{połączenie z bazą danych IBE zwracane przez funkcję \code{\link[ZPD:polacz]{ZPD::polacz()}};
jeśli nie podane, podjęta zostanie próba automatycznego nawiązania połączenia
(poprzez wywoływanie funkcji \code{\link[ZPD:polacz]{ZPD::polacz()}} z domyślnymi argumentami)}
}
\value{
lista ramek danych z elementami \emph{ewd}, \emph{wskazniki},
\emph{wskazniki_skalowania} i \emph{liczba_zdajacych}
}
\description{
Przetwarza zwrócone przez \emph{pvreg} oszacowania wskaźników EWD do struktur
danych umożliwiających zapisanie ich do bazy danych.
}
\seealso{
\code{\link[=oblicz_empiryczne_warstwice]{oblicz_empiryczne_warstwice()}}, \code{\link[=okresl_liczbe_zdajacych]{okresl_liczbe_zdajacych()}},
\code{\link[=oblicz_ewd_bk]{oblicz_ewd_bk()}}, \code{\link[=przygotuj_pv_do_zapisu]{przygotuj_pv_do_zapisu()}}
}
