#' @importFrom dplyr .data across all_of cur_data filter matches mutate select
usun_wyniki_laureatow = function(dane, czesciEgzaminow, tylkoRozprawki = FALSE,
                                 usunPusteWiersze = TRUE) {
  zmienneLaureaci =
    data.frame(zmienna = grep("^laur_", names(dane), value = TRUE)) %>%
    mutate(prefiks = sub("^laur_", "", .data$zmienna))
  if (tylkoRozprawki) {
    zmienneLaureaci = zmienneLaureaci %>%
      filter(.data$prefiks %in% filter(czesciEgzaminow, !is.na(.data$temat))$prefiks)
  } else {
    zmienneLaureaci = zmienneLaureaci %>%
      filter(.data$prefiks %in% czesciEgzaminow$prefiks)
  }
  for (i in 1:nrow(zmienneLaureaci)) {
    dane = dane %>%
      mutate(across(all_of(filter(czesciEgzaminow,
                                  .data$prefiks == zmienneLaureaci$prefiks[i])$kryterium),
                    ~ifelse(.data[[zmienneLaureaci$zmienna[i]]],
                            NA_real_, .)))
  }
  if (usunPusteWiersze) {
    dane = dane %>%
      filter(rowSums(!is.na(select(cur_data(), matches("^[kp]_")))) > 0)
  }
  return(dane)
}
