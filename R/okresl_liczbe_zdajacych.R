#' @importFrom dplyr %>% .data across arrange bind_rows count distinct filter group_by inner_join left_join matches mutate select summarise
#' @importFrom tidyr pivot_longer
okresl_liczbe_zdajacych = function(dane, rokEWD, luWszyscy, nazwaWskaznika) {
  liczbaZdajacych = dane$matura %>%
    select("id_obserwacji", matches("^[kp]_")) %>%
    pivot_longer(-"id_obserwacji", names_to = "kryterium", values_to = "wynik",
                 values_drop_na = TRUE) %>%
    inner_join(attributes(dane$matura)$czesciEgzaminu %>%
                 select("kryterium", kategoria_lu = "czesc_egzaminu"),
               by = "kryterium") %>%
    select(-"kryterium", -"wynik") %>%
    distinct() %>%
    inner_join(dane$warunkujace %>%
                 select("id_obserwacji", "id_szkoly"),
               by = "id_obserwacji")
  liczbaZdajacychR = liczbaZdajacych %>%
    filter(grepl(" rozszerzona$", .data$kategoria_lu)) %>%
    count(.data$id_szkoly, .data$kategoria_lu, name = "lu")
  liczbaZdajacychL = liczbaZdajacych %>%
    mutate(kategoria_lu = sub(" (podstawowa|rozszerzona)$",
                              " łącznie",
                              .data$kategoria_lu)) %>%
    distinct() %>%
    count(.data$id_szkoly, .data$kategoria_lu, name = "lu")
  liczbaZdajacychO = liczbaZdajacych %>%
    mutate(kategoria_lu = "ogółem") %>%
    distinct() %>%
    count(.data$id_szkoly, .data$kategoria_lu, name = "lu")

  bind_rows(liczbaZdajacychR,
            liczbaZdajacychL,
            liczbaZdajacychO) %>%
  mutate(id_ww = NA_integer_,
         rodzaj_wsk = "ewd",
         wskaznik = nazwaWskaznika,
         lu_ewd = .data$lu,
         rok = rokEWD) %>%
    left_join(luWszyscy,
              by = c("id_szkoly", "rok")) %>%
    select("id_ww", "rodzaj_wsk", "wskaznik", "kategoria_lu",
           id_szkoly_m = "id_szkoly", "lu", "lu_ewd", "lu_wszyscy") %>%
    arrange(.data$id_szkoly_m, .data$wskaznik, .data$kategoria_lu) %>%
    return()
}
