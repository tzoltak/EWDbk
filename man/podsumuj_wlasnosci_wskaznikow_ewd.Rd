% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/podsumuj_wlasnosci_wskaznikow_ewd.R
\name{podsumuj_wlasnosci_wskaznikow_ewd}
\alias{podsumuj_wlasnosci_wskaznikow_ewd}
\title{Podsumowanie wlasnosci obliczonych wskaznikow EWD}
\usage{
podsumuj_wlasnosci_wskaznikow_ewd(ewd, gamma = 0.95)
}
\arguments{
\item{ewd}{obiekt klasy \emph{listaWskaznikowEWD}, w szczególności zwrócony przez
\code{\link[=oblicz_ewd_bk]{oblicz_ewd_bk()}} lub \code{\link[=agreguj_ewd]{agreguj_ewd()}}}

\item{gamma}{poziom ufności dla obszarów ufności szkół, przy sprawdzaniu, czy
przecinają one osie układu współrzędnych wykresu (100, 0)}
}
\value{
ramka danych
}
\description{
Przygotowuje proste zestawienie najważniejszych własności wskaźników EWD
zapisanych w obiekcie klasy \emph{listaWskaznikowEWD}.
}
