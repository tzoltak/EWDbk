#' @title Przygotowywanie danych do obliczenia wskaznikow EWD
#' @description
#' Przygotowuje zestaw danych do obliczania latentnych wskaźników EWD w formie
#' odpowiadającej interfejsowi wejściowemu *pvreg*.
#' @inheritParams pobierz_parametry_egzaminow
#' @inheritParams oblicz_ewd_bk
#' @param typSzkoly ciąg znaków - typ szkoły (co do zasady `"LO"` lub `"T"`) -
#' wykorzystywany do odfiltrowania parametrów modeli skalowania opisujących
#' rozkłady umiejętności w ramach grup dotyczących tylko tych grup, które
#' obejmują uczniów szkół danego typu
#' @param tematyLaureatowMatura ramka danych zwrócona przez
#' [wybierz_tematy_dla_laureatow()]
#' @param parametryMatura ramka danych z parametrami modelu skalowania matury
#' (p. [pobierz_parametry_egzaminow])
#' @param tematyLaureatowWejscie ramka danych zwrócona przez
#' [wybierz_tematy_dla_laureatow()]
#' @param parametryWejscie ramka danych z parametrami modelu skalowania egzaminu
#' na wejściu (p. [pobierz_parametry_egzaminow])
#' @param minLUcznSzk liczba (naturalna) - szkoły z liczbą uczniów (wchodzących
#' do obliczeń) poniżej zadanej liczby zostaną wykluczone z estymacji
#' @return lista ramek danych o następujących elementach: *matura*, *wejscie*,
#' *warunkujace*, *cechySzkol*, z których trzy pierwsze zawierają dane do
#' wykorzystania w obliczeniach przez *pvreg* a ostatnia informacje przydatne
#' w obróbce wyników i przygotowywaniu ich do zapisania do bazy danych
#' @seealso [przygotuj_dane_do_skalowania()], [podmien_wyniki_laureatow()],
#' [wyswietl_rozklad_grup()], [oblicz_ewd_bk()], [unormuj_parametry_egzaminow()]
#' @importFrom stats na.omit
#' @importFrom dplyr %>% .data add_count all_of distinct filter inner_join left_join matches mutate select semi_join
przygotuj_dane_do_ewd_bk = function(skale, rokEWD, typSzkoly,
                                    tematyLaureatowMatura, parametryMatura,
                                    tematyLaureatowWejscie, parametryWejscie,
                                    katalogSurowe, minLUcznSzk, src) {
  stopifnot(is.data.frame(skale), nrow(skale) == 1,
            "id_skali_matura" %in% names(skale),
            "id_skali_we" %in% names(skale),
            "rodzaj_egzaminu_matura" %in% names(skale),
            "rodzaj_egzaminu_we" %in% names(skale),
            "typ_szkoly" %in% names(skale),
            "lata" %in% names(skale),
            is.numeric(rokEWD), length(rokEWD) == 1,
            is.character(typSzkoly), length(typSzkoly) == 1, !anyNA(typSzkoly),
            is.data.frame(tematyLaureatowMatura),
            is.data.frame(parametryMatura),
            is.data.frame(tematyLaureatowWejscie),
            is.data.frame(parametryWejscie),
            is.character(katalogSurowe), length(katalogSurowe) == 1,
            is.numeric(minLUcznSzk), length(minLUcznSzk) == 1, minLUcznSzk > 0)

  zmienneZadaniaMatura = unique(na.omit(parametryMatura$kryterium))
  zmienneTematyMatura = unique(grep("^t[[:digit:]]+_",
                                    parametryMatura$parametr_uwagi,
                                    value = TRUE))
  zmienneZadaniaWejscie = unique(na.omit(parametryWejscie$kryterium))
  zmienneTematyWejscie = unique(grep("^t[[:digit:]]+_",
                                     parametryWejscie$parametr_uwagi,
                                     value = TRUE))

  message("Przygotowywanie danych z wynikami matury.")
  daneMatura = przygotuj_dane_do_skalowania(skala = skale %>%
                                              select(id_skali = "id_skali_matura",
                                                     rodzaj_egzaminu = "rodzaj_egzaminu_matura",
                                                     "typ_szkoly"),
                                            katalogSurowe = katalogSurowe,
                                            rokEWD = rokEWD,
                                            dodajRokDoNazwTematow = FALSE,
                                            laureaci = "zostaw",
                                            echo = FALSE, src = src) %>%
    podmien_wyniki_laureatow(tematyLaureatow = tematyLaureatowMatura,
                             zmienneTematy = zmienneTematyMatura) %>%
    filter(.data$populacja_wy,
           !.data$pomin_szkole,
           grepl(paste0("^", typSzkoly, " "), .data$grupa))
  usunieteZmienneMatura =
    setdiff(grep("^([kp]|t[[:digit:]]+)_", names(daneMatura), value = TRUE),
            c(zmienneZadaniaMatura, zmienneTematyMatura))
  message("Przygotowywanie danych z wynikami egzaminu na wejściu.")
  daneWejscie = przygotuj_dane_do_skalowania(skala = skale %>%
                                               select(id_skali = "id_skali_we",
                                                      rodzaj_egzaminu = "rodzaj_egzaminu_we",
                                                      "typ_szkoly", "lata"),
                                             katalogSurowe = katalogSurowe,
                                             rokEWD = rokEWD,
                                             dodajRokDoNazwTematow = TRUE,
                                             laureaci = "zostaw",
                                             echo = FALSE, src = src) %>%
    podmien_wyniki_laureatow(tematyLaureatow = tematyLaureatowWejscie,
                             zmienneTematy = zmienneTematyWejscie) %>%
    filter(.data$populacja_we,
           grepl(paste0("^", typSzkoly, "(|w)$"), .data$grupa))
  usunieteZmienneWejscie =
    setdiff(grep("^([kp]|t[[:digit:]]+)_", names(daneWejscie), value = TRUE),
            c(zmienneZadaniaWejscie, zmienneTematyWejscie))
  message("Łączenie danych na wyjściu i na wejściu:")
  obserwacje = inner_join(daneMatura %>% select("id_obserwacji", "id_szkoly"),
                          daneWejscie %>% select("id_obserwacji"),
                          by = "id_obserwacji") %>%
    add_count(.data$id_szkoly, name = "n") %>%
    filter(.data$n >= minLUcznSzk) %>%
    select(-"n", -"id_szkoly")
  daneMatura = daneMatura %>%
    semi_join(obserwacje,
              by = "id_obserwacji") %>%
    select(-all_of(usunieteZmienneMatura))
  message("  Rozkład grup w przygotowanym pliku wyników matury:")
  wyswietl_rozklad_grup(daneMatura)

  daneWejscie = daneWejscie %>%
    semi_join(obserwacje,
              by = "id_obserwacji") %>%
    select(-all_of(usunieteZmienneWejscie))
  message("  Rozkład grup w przygotowanym pliku wyników egzaminu na wejściu:")
  wyswietl_rozklad_grup(daneWejscie)

  return(list(matura = daneMatura %>%
                select("id_obserwacji", "id_testu", "grupa",
                       matches("^([kp]|t[[:digit:]]+)_")) %>%
                mutate(grupa = droplevels(.data$grupa)),
              wejscie = daneWejscie %>%
                select("id_obserwacji", "id_testu", "grupa",
                       matches("^([kp]|t[[:digit:]]+)_")) %>%
                mutate(grupa = droplevels(.data$grupa)),
              warunkujace = daneMatura %>%
                select("id_obserwacji", "id_szkoly", "plec",
                       dysleksja_matura = "dysleksja") %>%
                left_join(daneWejscie %>%
                            select("id_obserwacji",
                                   dysleksja_gimnazjum = "dysleksja"),
                          by = "id_obserwacji"),
              cechySzkol = daneMatura %>%
                select("id_szkoly", "typ_szkoly", "publiczna", "specjalna",
                       "dla_doroslych", "przyszpitalna", "artystyczna",
                       "pomin_szkole") %>%
                distinct()))
}
