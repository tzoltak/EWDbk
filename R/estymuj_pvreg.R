#' @importFrom utils read.csv
#' @importFrom dplyr %>% .data bind_rows count inner_join mutate select slice starts_with
#' @importFrom tidyr pivot_longer
estymuj_pvreg = function(dane, parametry, nazwa, nPV, nWatkow, nadpisz,
                         metoda = c("tylko pliki", "Python", "R")) {
  metoda = match.arg(metoda)
  stopifnot(is.character(nazwa), length(nazwa) == 1,
            grepl("^[[:alpha:][:digit:]_-]+$", nazwa),
            is.numeric(nPV), length(nPV) == 1, nPV >= 0,
            is.numeric(nWatkow), length(nWatkow) == 1, nWatkow > 0,
            is.logical(nadpisz), length(logical) == 1)
  stopifnot(as.integer(nPV) == nPV,
            as.integer(nWatkow) == nWatkow,
            nadpisz %in% c(TRUE, FALSE))

  if (metoda == "R") {
    stop("Implementacja w R jest wciąż zawodna i nie może być używana.")
    ewd = pvreg(dane, parametry, nPV = nPV)
    pv = ewd$pv
    ewd = ewd$ewd
  } else {
    if (!dir.exists(nazwa)) {
      dir.create(nazwa)
    }
    plikiDoNadpisania = intersect(list.files(paste0(nazwa, "/")),
                                  paste0("ewd_", nazwa,
                                         c("_reffs.csv", "_pvs.csv")))
    if (length(plikiDoNadpisania) > 0 & !nadpisz) {
      if (metoda == "tylko pliki") {
        message("\nW katalogu '", nazwa, "' istnieją już pliki z wynikami estymacji EWD: '",
                paste(plikiDoNadpisania, collapse = "', '"), "'.",
                "\nJeśli chcesz je nadpisać, użyj argumentu `nadpisz=TRUE`.")
      } else {
        message("\nW katalogu '", nazwa, "' istnieją już pliki z wynikami estymacji EWD: '",
                paste(plikiDoNadpisania, collapse = "', '"), "'.",
                "\nZostaną one wczytane bez przeprowadzania estymacji.",
                "\nJeśli chcesz przeprowadzić estymację i je nadpisać, użyj argumentu `nadpisz=TRUE`.")
      }
    } else {
      if (length(plikiDoNadpisania) > 0) {
        message("W katalogu '", nazwa, "' istnieją już pliki z wynikami estymacji EWD: '",
                paste(plikiDoNadpisania, collapse = "', '"),
                "'.\nZostaną one nadpisane.")
      }
      plikSterujacy = zapisz_dane_dla_pvreg(dane, parametry, nazwa,
                                            nPV = nPV, nWatkow = nWatkow)
      if (metoda == "tylko pliki") {
        writeLines(c("./py31010pvreg/Scripts/activate",
                     sub("^([[:upper:]]:).*", "\\1", getwd()),
                     paste0("cd '", sub("^([[:upper:]]:)", "", getwd()),
                            "/", nazwa, "'"),
                     paste0("python -m pvreg -c ", plikSterujacy)),
                   con = paste0(nazwa, "/RUNME.txt"))
        return(NULL)
      }
      obecnyKatalog = getwd()
      on.exit(setwd(obecnyKatalog))
      setwd(nazwa)
      message("\nEstymacja przy użyciu pvreg.py\n")
      system2("python", args = paste("-m pvreg -c", plikSterujacy),
              stdout = paste0("log-", nazwa, ".log"),
              stderr = paste0("log-", nazwa, ".log"))
      # unlink(list.files(path = "./", pattern = "^mlv_data[[:digit:]]+\\.csv$"))
      setwd(obecnyKatalog)
      on.exit(NULL)
    }
    # wczytywanie wyników
    ewd = try(read.csv(paste0(nazwa, "/ewd_", nazwa, "_reffs.csv"),
                       stringsAsFactors = FALSE))
    if (inherits(ewd, "try-error")) stop("Nie udało się wczytać pliku z obliczonymi przez pvreg oszacowaniami efektów losowych ('",
                                         paste0(nazwa, "/ewd_", nazwa, "_reffs.csv"), "').")
    if ("cov_mean_eva_schl" %in% names(ewd)) {
      ewd <- ewd %>%
        mutate(cov_mean_eva_schl =
                 sapply(strsplit(gsub("[][]", "", .data$W), ", "),
                        function(x) as.numeric(x[2]))) %>%
        select(-"COV", -"W", -"B", -"m")
    } else {
      ewd <- ewd %>%
        select(-any_of(ends_with("_COV")))
    }
    pv =  try(read.csv(paste0(nazwa, "/ewd_", nazwa, "_pvs.csv"),
                       stringsAsFactors = FALSE))
    if (inherits(pv, "try-error")) {
      warning("Nie udało się wczytać pliku z obliczonymi przez pvreg PV umiejętności uczniów ('",
              paste0(nazwa, "/ewd_", nazwa, "_pvs.csv"), "').")
      pv = NULL
    }
  }

  if (all(c("mean_schl", "eva_schl", "mean_schl_se", "eva_schl_se",
            "cov_mean_eva_schl", "id_szkoly") %in% names(ewd))) {
    # obsługa starszych wersje pvreg, które nie zwracały śr. wyników na wyjściu i ich korelacji z EWD
    ewd = ewd %>%
      mutate(out_schl = .data$mean_schl + .data$eva_schl,
             out_schl_se = (.data$mean_schl_se^2 + .data$eva_schl_se^2 +
                              2 * ewd$cov_mean_eva_schl)^0.5,
             cor_eva_out = (.data$cov_mean_eva_schl + .data$eva_schl_se^2) /
               (.data$out_schl_se * .data$eva_schl_se)) %>%
      inner_join(dane$warunkujace %>%
                   count(.data$id_szkoly, name = "lu"),
                 by = "id_szkoly")
  } else if (all(c("st3_sch3_mean_schl_out", "st3_sch3_mean_schl_out_se",
                   "st3_sch3_eva_schl", "st3_sch3_eva_schl_se",
                   "st3_sch3_R", "N", "id_szkoly") %in% names(ewd))) {
    # oszacowania średniej na wejściu i jej BS można by też wziąść z PV - są
    # zapisywane w pliku przez pvreg - ale one są nieściągnięte
    ewd = ewd %>%
      mutate(mean_schl = .data$st3_sch3_mean_schl_out - .data$st3_sch3_eva_schl,
             mean_schl_se = (.data$st3_sch3_mean_schl_out_se^2 +
                               .data$st3_sch3_eva_schl_se^2 -
                               2 * .data$st3_sch3_R *
                               .data$st3_sch3_mean_schl_out_se *
                               .data$st3_sch3_eva_schl_se)^0.5)
    # tylko wskaźniki odnoszące się do zmodyfikowanej metody liczenia, zamiana
    # nazw na występujące w plikach z wcześniejszej wersji pvreg
    ewd = ewd %>%
      select("id_szkoly",
             eva_schl = "st3_sch3_eva_schl",
             eva_schl_se = "st3_sch3_eva_schl_se",
             out_schl = "st3_sch3_mean_schl_out",
             out_schl_se = "st3_sch3_mean_schl_out_se",
             cor_eva_out = "st3_sch3_R",
             "mean_schl", "mean_schl_se",
             lu = "N")
  } else {
    stop("W pliku z obliczonymi przez pvreg oszacowaniami efektów losowych ('",
         paste0(nazwa, "/ewd_", nazwa, "_reffs.csv"), "') brakuje niezbędnych kolumn.")
  }

  pv = bind_rows(dane$wejscie %>%
                   select("id_obserwacji", "id_testu", "grupa") %>%
                   mutate(exam_var = 0L),
                 dane$matura %>%
                   select("id_obserwacji", "id_testu", "grupa") %>%
                   mutate(exam_var = 1L)) %>%
    inner_join(pv,
               by = c("id_obserwacji", "exam_var"))
  if (any(grepl("^pv_", names(pv)))) {
    pv <- pv %>%
      pivot_longer(starts_with("pv_"), names_to = "nr_pv", names_prefix = "pv_",
                   names_transform = list(nr_pv = as.integer),
                   values_to = "wynik") %>%
      mutate(nr_pv = .data$nr_pv + 1L,
             bs = NA_real_,
             estymacja = "PV")
  } else {
    pv <- pv %>%
      mutate(wynik = NA_real_,
             nr_pv = NA_integer_,
             bs = NA_real_,
             estymacja = "PV") %>%
      slice(0L)
    warning("W plikach z obliczonymi przez pvreg PV umiejętności uczniów ('",
            paste0(nazwa, "/ewd_", nazwa, "_pvs.csv"),
            "') nie zostały zapisane żadne PV.", immediate. = TRUE)
  }

  return(list(ewd = ewd,
              pv = pv))
}
