#' @importFrom dplyr %>% .data all_of distinct select semi_join
dodaj_wybor_czesci_egzaminu = function(dane, czesciEgzaminow) {
  sufiksNF = ifelse(all(c(FALSE, TRUE) %in% czesciEgzaminow$czy_sf), "nf", "")
  czesci = czesciEgzaminow %>%
    select("prefiks", "czy_sf") %>%
    distinct()
  wyborCzesci = matrix(0L, nrow = nrow(dane), ncol = nrow(czesci),
                       dimnames = list(NULL,
                                       paste0("s", ifelse(czesci$czy_sf,
                                                          "", sufiksNF),
                                              "_",
                                              sub("^m_", "", czesci$prefiks)))) %>%
    as.data.frame()
  for (i in 1:nrow(czesci)) {
    kryteriaCzesci = semi_join(czesciEgzaminow, czesci[i, ],
                               by = c("prefiks", "czy_sf"))$kryterium
    wyborCzesci[rowSums(!is.na(select(dane, all_of(kryteriaCzesci)))) > 0, i] = 1L
  }
  cbind(dane, wyborCzesci) %>%
    return()
}
