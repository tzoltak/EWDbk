#' @importFrom dplyr %>% .data arrange distinct group_by inner_join left_join matches mutate select summarise
#' @importFrom tidyr pivot_longer pivot_wider
okresl_poziom_egzaminu = function(dane, czesciEgzaminu,
                                  zmienNazweGdyTylkoJednaCzesc = TRUE) {
  poziomy = dane %>%
    select("id_obserwacji", "rok", matches("^[kp]_")) %>%
    pivot_longer(matches("^[kp]_"), names_to = "kryterium",
                 values_to = "wynik", values_drop_na = TRUE) %>%
    inner_join(czesciEgzaminu %>%
                 mutate(poziom = sub("^.*_([pr])$", "\\1",
                                     .data$prefiks),
                        czesc_egzaminu = sub("_[pr]$", "",
                                             .data$prefiks)) %>%
                 select("kryterium", "czesc_egzaminu", "poziom"),
               by = "kryterium") %>%
    select("id_obserwacji", "rok", "czesc_egzaminu", "poziom") %>%
    distinct() %>%
    arrange(.data$id_obserwacji, .data$rok,
            .data$czesc_egzaminu, .data$poziom) %>%
    group_by(.data$id_obserwacji, .data$rok, .data$czesc_egzaminu) %>%
    summarise(poziom = paste(c("p" = "PP", "r" = "PR")[.data$poziom],
                             collapse = " i "),
              .groups = "drop") %>%
    pivot_wider(names_from = "czesc_egzaminu", values_from = "poziom")
  if (zmienNazweGdyTylkoJednaCzesc & ncol(poziomy) == 3) {
    names(poziomy)[3] = "poziom"
  }
  dane %>%
    left_join(poziomy,
              by = c("id_obserwacji", "rok")) %>%
    return()
}
