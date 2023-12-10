#' @importFrom dplyr %>% .data count mutate select
wyswietl_rozklad_grup = function(dane) {
  dane %>%
    count(.data$grupa, name = "n") %>%
    mutate("%" = paste0(format(100 * .data$n / sum(.data$n),
                               digits = 1, nsmall = 1),
                        "%"),
           n = format(.data$n, big.mark = "'")) %>%
    select("grupa", "n", "%") %>%
    as.data.frame(check.names = FALSE) %>%
    print()
}
