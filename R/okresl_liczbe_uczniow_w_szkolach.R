#' @title Przygotowywanie danych do obliczenia wskaznikow EWD
#' @description
#' Na podstawie wczytanych z dysku *danych kontekstowych* dotyczących matury
#' określa liczbę wszystkich absolwentów poszczególnych szkół w danym roczniku
#' i dołącza do tego zestawienia informację z bazy szkół, czy w danej szkole
#' przeprowadzana jest matura międzynarodowa.
#' @inheritParams oblicz_ewd_bk
#' @return ramka danych z kolumnami *id_szkoly*, *lu_wszyscy*, *rok*,
#' *matura_miedzynarodowa*
#' @seealso [ZPD::pobierz_szkoly()], [oblicz_ewd_bk()]
#' @importFrom dplyr %>% .data collect count filter left_join select
okresl_liczbe_uczniow_w_szkolach = function(rokEWD, katalogSurowe, src) {
  stopifnot(is.numeric(rokEWD), length(rokEWD) == 1, !anyNA(rokEWD),
            is.character(katalogSurowe), length(katalogSurowe) == 1)
  stopifnot(as.integer(rokEWD) == rokEWD,
            dir.exists(katalogSurowe))
  if (is.null(src)) {
    src = ZPD::polacz()
    on.exit(ZPD::rozlacz(src))
  }

  katalogSurowe = paste0(sub("/$", "", katalogSurowe), "/")
  plikDane = paste0(katalogSurowe, "matura-kontekstowe.RData")
  if (!file.exists(plikDane)) {
    stop("Nie można wczytać danych z pliku '", plikDane, "'. Plik nie istnieje.")
  }
  obiekty = load(plikDane)
  maska = grepl("^[[:alpha:]]Kontekstowe$", obiekty)
  if (!any(maska)) {
    stop("W pliku '", plikDane, "' brak obiektu zawierającego dane kontekstowe.")
  } else if (!("daneKontekstowe" %in% class(get(obiekty[maska])))) {
    stop("W pliku '", plikDane, "' brak obiektu zawierającego dane kontekstowe.")
  }
  get(obiekty[maska]) %>%
    filter(.data$populacja_wy,
           .data$rok == rokEWD) %>%
    count(.data$id_szkoly, .data$rok, name = "lu_wszyscy") %>%
    left_join(ZPD::pobierz_szkoly(src) %>%
                filter(.data$rok == rokEWD) %>%
                select("id_szkoly", "rok", "matura_miedzynarodowa") %>%
                collect(),
              by = c("id_szkoly", "rok")) %>%
    return()
}
