#' @title Przetwarzanie danych z wynikami egzaminow
#' @description
#' Dodaje do danych zmienną opisującą przypisanie uczniów do grup wyróżnianych
#' w procedurze skalowania wyników matury lub egzaminu na wejściu.
#' @param dane ramka danych z wynikami egzaminu
#' @param rokWy liczba (całkowita) - rok matury (i jednocześnie rok definiujący
#' jednoroczny wskaźnik EWD, na potrzeby obliczania którego ma być
#' przeprowadzone skalowanie, do którego przygotowywane są dane)
#' @param katalogSurowe ciąg znaków - ścieżka do katalogu, w którym znajdują
#' się dane z wynikami surowymi egzaminów oraz *danymi kontekstowymi*, pobranymi
#' przy pomocy funkcji [EWDdane::pobierz_wyniki_surowe]
#' @param typSzkoly `"LO"`, `"T"` albo `NULL` - jeśli przetwarzane dane dotyczą
#' egzaminu na wejściu, typ szkoły, który obejmują dane - niezbędny, aby
#' określić, *dane kontekstowe* z którego roku opisują uczniów o typowej
#' długości toku kształcenia, a z którego roku opisują uczniów o toku
#' kształcenia wydłużonym o rok (jeśli przetwarzane dane dotyczą matury,
#' należy podać `NULL`)
#' @param rokWe liczba (całkowita) albo `NULL` - jeśli przetwarzane dane dotyczą
#' egzaminu na wejściu, typ szkoły, który obejmują dane - niezbędny, aby
#' określić, *dane kontekstowe* z którego roku opisują uczniów o typowej
#' długości toku kształcenia, a z którego roku opisują uczniów o toku
#' kształcenia wydłużonym o rok (jeśli przetwarzane dane dotyczą matury,
#' należy podać `NULL`)
#' @param echo wartość logiczna - czy wyświetlić na konsoli tabelę z rozkładem
#' grup
#' @details
#' W odniesieniu do matury wyróżniane grupy to:
#' -  dla przedmiotów obowiązkowych: *LO PP i PR*, *LO PP*, *T PP i PR*, *T PP*
#'    i *inne*;
#' -  dla przedmiotów nieobowiązkowych: *LO PR*, *T PR* i *inne*.
#' W odniesieniu do egzaminów na wejściu (egzamin ósmoklasisty lub gimnazjalny):
#' -  dla LO: *LO*, *inne*, *LOw*, *innew* (dwie ostatnie są tworzone przez
#'    uczniów zdających egzamin w roku o jeden wcześniejszym, niż wynikałoby to
#'    z długości toku kształcenia w LO),
#' -  dla techników: *T*, *inne*, *Tw*, *innew* (dwie ostatnie są tworzone przez
#'    uczniów zdających egzamin w roku o jeden wcześniejszym, niż wynikałoby to
#'    z długości toku kształcenia w technikum).#'
#' @return ramka danych przekazana argumentem `dane`, z dodaną zmienną *grupa*
#' opisującą przypisanie uczniów do grup wyróżnianych w skalowaniu
#' @seealso [wyswietl_rozklad_grup()], [przygotuj_dane_do_skalowania()]
#' @importFrom dplyr %>% .data anti_join case_when filter left_join mutate rename select
okresl_grupe = function(dane, rokWy, katalogSurowe, typSzkoly = NULL,
                        rokWe = NULL, echo = TRUE) {
  stopifnot(is.data.frame(dane),
            is.numeric(rokWy), length(rokWy) == 1,
            as.integer(rokWy) == rokWy,
            is.character(katalogSurowe), length(katalogSurowe) == 1,
            is.character(typSzkoly) | is.null(typSzkoly),
            is.numeric(rokWe) | is.null(rokWe),
            is.logical(echo), length(echo) == 1, echo %in% c(TRUE, FALSE))
  grupyWejscie = c("LO", "LOw", "T", "Tw", "inne", "innew")
  grupyMatura = c("LO", "LO PP", "LO PP i PR", "LO PR",
                  "T", "T PP", "T PP i PR", "T PR", "inne")
  if (!is.null(rokWe)) { # egzamin gimnazjalny lub ósmoklasisty
    stopifnot(is.numeric(rokWe), as.integer(rokWe) == rokWe,
              length(rokWe) == 1,
              !is.null(typSzkoly),
              length(typSzkoly) == 1, typSzkoly %in% c("LO", "T"))
    czyRocznikWydl = (rokWy - rokWe) > case_when(typSzkoly == "LO" && rokWy < 2023 ~ 3,
                                                 typSzkoly == "LO" ~ 4,
                                                 typSzkoly == "T" && rokWy < 2024 ~ 4,
                                                 typSzkoly == "T" ~ 5)

    mKontekstowe = paste0(sub("/$", "", katalogSurowe),
                          "/matura-kontekstowe.RData")
    if (!file.exists(mKontekstowe)) {
      stop("W podanej lokalizacji ('", katalogSurowe, "' nie można znaleźć pliku 'matura-kontekstowe.RData'.")
    }
    if (rokWe < 2019 ||
        (rokWe == 2019 && rokWy == 2023 && typSzkoly == "T") ||
        (rokWe == 2019 && rokWy == 2022 && typSzkoly == "LO")) {
      gKontekstowe = paste0(sub("/$", "", katalogSurowe),
                            "/egzamin gimnazjalny-kontekstowe.RData")
      if (!file.exists(gKontekstowe)) {
        stop("W podanej lokalizacji ('", katalogSurowe, "' nie można znaleźć pliku 'egzamin gimnazjalny-kontekstowe.RData'.")
      }
    } else {
      gKontekstowe = paste0(sub("/$", "", katalogSurowe),
                            "/egzamin ósmoklasisty-kontekstowe.RData")
      if (!file.exists(gKontekstowe)) {
        stop("W podanej lokalizacji ('", katalogSurowe, "' nie można znaleźć pliku 'egzamin ósmoklasisty-kontekstowe.RData'.")
      }
    }

    obiekty = try(load(mKontekstowe))
    if (!("mKontekstowe" %in% obiekty)) {
      stop("Nie udało się poprawnie wczytać pliku 'matura-kontekstowe.RData', lub nie zawiera on obiektu 'mKontekstowe'.")
    }
    obiekty = try(load(gKontekstowe))
    if (!("gKontekstowe" %in% obiekty) &&
        (rokWe < 2019 ||
         (rokWe == 2019 && rokWy == 2023 && typSzkoly == "T") ||
         (rokWe == 2019 && rokWy == 2022 && typSzkoly == "LO"))) {
      stop("Nie udało się poprawnie wczytać pliku 'egzamin gimnazjalny-kontekstowe.RData', lub nie zawiera on obiektu 'gKontekstowe'.")
    } else if (!("gKontekstowe" %in% obiekty)) {
      stop("Nie udało się poprawnie wczytać pliku 'egzamin ósmoklasisty-kontekstowe.RData', lub nie zawiera on obiektu 'gKontekstowe'.")
    }

    mKontekstowe = get("mKontekstowe") %>%
      filter(.data$rok %in% rokWy) %>%
      select("id_obserwacji", "rok", "typ_szkoly", "populacja_wy", "pomin_szkole") %>%
      left_join(get("gKontekstowe") %>%
                  filter(.data$rok %in% ((rokWy - 3):(rokWy - 5))) %>%
                  select("id_obserwacji", rok_we = "rok"),
                by = "id_obserwacji")
    rm(gKontekstowe)

    dane = dane %>%
      left_join(mKontekstowe %>%
                  rename(rok_wy = "rok", rok = "rok_we",
                         typ_szkoly_wy = "typ_szkoly",
                         populacja_wy_m = "populacja_wy",
                         pomin_szkole_m = "pomin_szkole"),
                by = c("id_obserwacji", "rok")) %>%
      mutate(wydl = .data$rok_wy - .data$rok -
               case_when(.data$typ_szkoly_wy == "LO" & rokWy < 2023 ~ 3,
                         .data$typ_szkoly_wy == "LO" ~ 4,
                         .data$typ_szkoly_wy == "T" & rokWy < 2024 ~ 4,
                         .data$typ_szkoly_wy == "T" ~ 5),
             grupa =
               case_when((!.data$populacja_wy_m | .data$pomin_szkole_m) & !czyRocznikWydl ~ "inne",
                         (!.data$populacja_wy_m | .data$pomin_szkole_m) & czyRocznikWydl ~ "innew",
                         .data$wydl %in% 0 & .data$typ_szkoly_wy %in% "LO" &
                           typSzkoly != "T" ~ "LO",
                         .data$wydl %in% 1 & .data$typ_szkoly_wy %in% "LO" &
                           typSzkoly != "T" ~ "LOw",
                         .data$wydl %in% 0 & .data$typ_szkoly_wy %in% "T" &
                           typSzkoly != "LO" ~ "T",
                         .data$wydl %in% 1 & .data$typ_szkoly_wy %in% "T" &
                           typSzkoly != "LO" ~ "Tw",
                         !czyRocznikWydl ~ "inne",
                         .default = "innew") %>%
               factor(levels = grupyWejscie) %>%
               droplevels()) %>%
      select(-"populacja_wy_m")
  } else { # matura
    if ("poziom" %in% names(dane)) {
      stopifnot(all(!is.na(dane$poziom)))
    } else {
      dane$poziom = ""
    }

    dane = dane %>%
      # jeśli w danych występuje i PP i PR, to osoby, które pisały tylko PR
      # chcemy wyrzucić do "innych"
      mutate(poziom = ifelse(.data$poziom == "PR" & any(.data$poziom == "PP"),
                             NA_character_,
                             .data$poziom)) %>%
      # koniec cudowania
      mutate(grupa = ifelse(.data$typ_szkoly %in% c("LO", "T") &
                              .data$poziom %in% c("PP", "PP i PR", "PR", "") &
                              .data$populacja_wy &
                              !.data$pomin_szkole,
                            sub(" $", "", paste(.data$typ_szkoly, .data$poziom)),
                            "inne") %>%
               factor(levels = grupyMatura) %>%
               droplevels())
  }
  if (echo) {
    message("Rozkład grup wyróżnionych w ramach skalowania:")
    wyswietl_rozklad_grup(dane)
  }
  return(dane)
}
