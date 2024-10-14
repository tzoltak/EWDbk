#' @title Przetwarzanie i zapis wynikow obliczania latentnych wskaznikow EWD
#' @description
#' Oblicza parametry warstwic EWD w populacji szkół ważonych liczbą uczniów,
#' analizując empiryczny rozkład wartości wskaźników w dwuwymiarowej przestrzeni
#' (wyniki_egzaminu_na_wyjściu x EWD).
#' @param wyniki wektor liczb - oszacowania średnich wyników egzaminu na
#' wyjściu poszczególnych szkół
#' @param ewd wektor liczb - oszacowania EWD poszczególnych szkół
#' @param liczbaUczniow wektor liczb (dodatnich) - liczba uczniów
#' uwzględnionych przy obliczaniu danego wskaźnika EWD w poszczególnych szkołach
#' @param pr wektor liczb z przedziału (0, 1) - częstości, dla których mają
#' zostać wyznaczone odcinające je w empirycznych danych wartości dystrybuanty
#' rozkładu chi-kwadrat o dwóch stopniach swobody
#' @return wektor liczb (prawdopodobieństw) o długości równej długości
#' argumentu `pr`
#' @seealso [kwantyl_wazony()], [przygotuj_ewd_do_zapisu()]
#' @importFrom stats cov.wt mahalanobis pchisq
# Funkcja pierwotnie jako wielkoscWarstwic() w pakiecie EWDwskazniki
oblicz_empiryczne_warstwice = function(wyniki, ewd, liczbaUczniow, pr = c(0.5, 0.9)) {
  stopifnot(is.numeric(wyniki), !anyNA(wyniki),
            is.numeric(ewd), !anyNA(ewd),
            is.numeric(liczbaUczniow), all(liczbaUczniow > 0),
            length(wyniki) == length(ewd),
            length(wyniki) == length(liczbaUczniow),
            is.numeric(pr), length(pr) > 0, all(pr > 0), all(pr < 1))
  kow = cov.wt(cbind(wyniki, ewd), liczbaUczniow, method = "ML")
  odlMahSt = mahalanobis(cbind(wyniki, ewd), kow$center, kow$cov)
  kwantyle = kwantyl_wazony(odlMahSt, liczbaUczniow, pr)
  prEmp = round(pchisq(kwantyle, df = 2), 3)
  return(prEmp)
}
#' @title Przetwarzanie i zapis wynikow obliczania latentnych wskaznikow EWD
#' @description
#' Oblicza kwantyle ważone.
#' @param x wektor liczb, dla którego mają zostać obliczone kwantyle
#' @param wagi wektor liczb nieujemnych z wagami, jakie powinny zostać
#' przypisane kolejnym elementom wektora `x`
#' @param probs wektor liczb z przedziału \[0, 1\] - kwantyle, które powinny
#' zostać obliczone
#' @param na.rm wartość logiczna - czy przy obliczaniu kwantyli ignorować ew.
#' braki danych w `x`
#' @details
#' Funkcja pierwotnie znajdowała się w pakiecie *EWDogolny*.
#' @return wektor liczb
#' @seealso [oblicz_empiryczne_warstwice()]
#' @importFrom stats na.omit
# Funkcja pierwotnie w pakiecie EWDogolny
kwantyl_wazony = function(x, wagi, probs=seq(0, 1, 0.25), na.rm = FALSE) {
  stopifnot(is.numeric(x),
            is.numeric(wagi), all(wagi >= 0), length(wagi) == length(x),
            is.numeric(probs), all(probs >= 0), all(probs <= 1),
            is.logical(na.rm), length(na.rm) == 1)

  temp = data.frame(x = x, wagi = wagi, wagiOdwr = wagi)
  # obsługa ew. braków danych w x
  if (na.rm) {  # gdzie braki, to zignoruj i po prostu policz na pozostałych
    temp = na.omit(temp)
  } else {
    if (with(temp, any(is.na(x[wagi > 0])))) {  # jeśli jakieś obserwacje z niezerowymi wagami mają braki, zwróć brak danych
      return(setNames(rep(NA, length(probs)), probs))
    } else { # jeśli braki dotyczą tylko obserwacji z wagami równymi zero, można je zignorować
      temp = na.omit(temp)
    }
  }
  # samo wyliczanie kwantyli
  temp = temp[order(temp$x), ]
  temp$wagi[!is.na(temp$wagi)] =
    cumsum(temp$wagi[!is.na(temp$wagi)] ) / sum(temp$wagi, na.rm = TRUE)
  temp$wagiOdwr[!is.na(temp$wagiOdwr)] =
    rev(cumsum(rev(temp$wagiOdwr[!is.na(temp$wagiOdwr)]))) / sum(temp$wagiOdwr, na.rm = TRUE)
  wynik = vector()
  for (i in probs) {
    wynik = c(wynik, mean(temp$x[temp$wagi >= i & temp$wagiOdwr >= (1 - i)]))
  }
  return(setNames(wynik, probs))
}
