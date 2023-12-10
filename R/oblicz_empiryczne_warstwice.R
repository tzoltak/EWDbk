#' @importFrom stats cov.wt mahalanobis pchisq
# Funkcja pierwotnie jako wielkoscWarstwic() w pakiecie EWDwskazniki
oblicz_empiryczne_warstwice = function(wyniki, ewd, liczbaUczniow, pr = c(0.5, 0.9)) {
  kow = cov.wt(cbind(wyniki, ewd), liczbaUczniow, method = "ML")
  odlMahSt = mahalanobis(cbind(wyniki, ewd), kow$center, kow$cov)
  kwantyle = kwantyl_wazony(odlMahSt, liczbaUczniow, pr)
  prEmp = round(pchisq(kwantyle, df = 2), 3)
  return(prEmp)
}
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
