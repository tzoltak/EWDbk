#' @title Przetwarzanie danych z wynikami egzaminow
#' @description
#' Funkcja dodaje do danych z wynikami egzaminu zmienne (pseudokryteria)
#' opisujące wybór części egzaminu.
#' @inheritParams usun_wyniki_laureatow
#' @details
#' Zmienne (pseudokryteria) opisujące fakt przystąpienia (lub) nie do
#' poszczególnych części egzaminu umożliwiają modelowanie różnic w średnim
#' poziomie (ale już **nie** w zróżnicowaniu) umiejętności pomiędzy uczniami
#' wybierającymi różne przedmioty (na różnych poziomach) w sytuacji, gdy
#' skalowanych jest jednocześnie wiele przedmiotów i modelowanie takich różnic
#' poprzez grupy w modelu wielogrupowym prowadziłoby do rozpatrywania zbyt
#' dużej liczby grup, z których niektóre były by bardzo małoliczne.
#'
#' W ramach przyjętej procedury skalowania podejście to
#' **nie jest wykorzystywane**, niemniej funkcja pozostaje włączona do pakietu.
#' @return ramka danych przekazana argumentem `dane`, z dodanymi zmiennymi
#' opisującymi wybór (zdawanie) poszczególnych części egzaminu
#' @importFrom dplyr %>% .data all_of distinct select semi_join
dodaj_wybor_czesci_egzaminu = function(dane, czesciEgzaminow) {
  stopifnot(is.data.frame(dane),
            is.data.frame(czesciEgzaminow))
  stopifnot(all(c("id_obserwacji", "rok") %in% names(dane)),
            all(c("kryterium", "czy_sf", "prefiks") %in% names(czesciEgzaminow)),
            all(grep("^[kp]_", names(dane), value = TRUE) %in%
                  czesciEgzaminow$kryterium))
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
