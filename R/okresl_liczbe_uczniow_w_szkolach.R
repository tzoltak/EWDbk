#' @importFrom dplyr %>% .data collect count filter left_join select
okresl_liczbe_uczniow_w_szkolach = function(rokEWD, katalogDane, src) {
  if (is.null(src)) {
    src = ZPD::polacz()
    on.exit(ZPD::rozlacz(src))
  }

  katalogDane = paste0(sub("/$", "", katalogDane), "/")
  plikDane = paste0(katalogDane, "matura-kontekstowe.RData")
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
