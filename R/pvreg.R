#' @title Proba sportowania programu Bartka Kondratka pod R
#' @description
#' Próba (nieudana) implementacji *pvreg* w R.
#' @param dane lista *ramek danych* z danymi
#' @param parametry lista *ramek danych* z parametrami zadań i grup zdających
#' @param fixedEffects wektor tekstowy z nazwami zmiennych, które mają zostać
#' wykorzystane w modelu warunkującym umiejętności
#' @param nPV liczba (naturalna) - liczba PV z indywidualnymi oszacowaniami
#' umiejętności, które mają zostać wykorzystane w estymacji
#' @param keepPV wartość logiczna - **nie zaimplementowane** - argument
#' wyłącznie dla fasadowej zgodności z API *pvreg*
#' @param nJobs liczba (naturalna) - liczba równoległych wątków, w których
#' ma być prowadzona estymacja; **nie zaimplementowane** - argument wyłącznie
#' dla fasadowej zgodności z API *pvreg*
#' @param nItBurn0 liczba (naturalna) - liczba iteracji pierwszego etapu *burn-in*
#' @param nItBurn1 liczba (naturalna) - liczba iteracji drugiego etapu *burn-in*
#' @param nItFinal liczba (naturalna) - liczba iteracji ostatnie etapu
#' estymacji
#' @param echo liczba (naturalna) - 0 oznacza brak zwracania na konsolę
#' informacji o procesie estymacji, większe wartości stopniowo zwiększają
#' poziom szczegółowości informacji zwracanych na konsoli
#' @param methodMultilevelPriors ciąg znaków
#' @param rescalePriors wartość logiczna
#' @return *ramka danych* zawierająca oszacowania średnich wyników szkół
#' *na wejściu*, EWD, ich błędów standardowych i kowariancji
#' @importFrom dplyr %>% all_of select
pvreg = function(dane, parametry,
                 fixedEffects = setdiff(names(dane$warunkujace),
                                        c("id_obserwacji", "id_szkoly")),
                 nPV = 5L, keepPV = TRUE, nJobs = NULL,
                 nItBurn0 = 10L, nItBurn1 = 20L, nItFinal = 10L, echo = 1L,
                 methodMultilevelPriors = c("simulateIndep",
                                            "simulateCond",
                                            "predRandResVar",
                                            "predRandAllVars",
                                            "predFixedAllVars"),
                 rescalePriors = FALSE) {
  methodMultilevelPriors = match.arg(methodMultilevelPriors)
  if (!is.null(nJobs)) {
    warning("Implementacja w R w obecnej wersji nie umożliwia liczenia z wykorzystaniem wielu wątków.")
  }
  dane$warunkujace = dane$warunkujace %>%
    select(all_of(c("id_obserwacji", "id_szkoly", fixedEffects)))
  data = load_data(dane, parametry)

  pvResults = generate_pv_single(items = data$items, groups = data$groups,
                                 responses = data$responses,
                                 estimates = data$estimates,
                                 nItBurn0 = nItBurn0, nItBurn1 = nItBurn1,
                                 nItFinal = nItFinal, nDraw = nPV,
                                 mLvData = data$students,
                                 mLvFormula = data$fixedEffectsFormula,
                                 echo = echo,
                                 methodMultilevelPriors = methodMultilevelPriors,
                                 rescalePriors = rescalePriors)
  return(list(ewd = calculate_school_reff(pvResults$blups),
              pv = pvResults$pvs))
}
#' @importFrom dplyr %>% any_of inner_join left_join select
generate_pv_single = function(items, groups, responses, estimates,
                              nItBurn0, nItBurn1, nItFinal, nDraw,
                              mLvData, mLvFormula, echo = 1L,
                              methodMultilevelPriors, rescalePriors) {
  responses = inner_join(mLvData,
                         responses,
                         by = c("id_obserwacji", "grupa", "examOut"))
  pvs = mLvData %>%
    select("id_obserwacji", "id_szkoly", "examOut")
  blups = vector(mode = "list", length = nDraw)
  for (chain in seq_len(nDraw)) {
    if (echo > 0L) message("Chain ", chain, " (of ", nDraw, "):\n  Adding noise to group & item parameters.")
    estimatesTemp = noisify_estimates(estimates)
    if (echo > 0L) message("  Estimating EAPs.")
    responses = responses %>%
      left_join(estimate_eaps(items = estimatesTemp$items,
                              groups = estimatesTemp$groups,
                              responses = responses,
                              nIP = 121L),
                by = c("id_obserwacji", "grupa", "examOut")) %>%
      left_join(groups %>%
                  select("grupa", priorMean = "mean", priorSD = "sd"),
                by = "grupa")
    if (echo > 1L) print(compute_group_theta_pars(responses), row.names = FALSE, digits = 2L)
    if (echo > 0L) message("  Burn in", ifelse(echo > 3L, ":", "."))
    for (nothing in seq_len(nItBurn0)) {
      responses$estTheta = draw_mcmc(items = estimatesTemp$items,
                                     responses = responses,
                                     echo = echo > 3L)
    }
    if (echo > 2L) print(compute_group_theta_pars(responses), row.names = FALSE, digits = 2L)
    if (echo > 0L) message("  Multilevel burn in", ifelse(echo > 3L, ":", "."))
    for (i in seq_len(nItBurn1)) {
      if ("model" %in% ls()) { # w zasadzie powinno być szybciej
        model = lme4::refit(model, responses$estTheta)
      } else {
        model = lme4::lmer(mLvFormula,
                           data = rename(responses, theta = "estTheta"))
      }
      if (i == 1L & echo > 2L) print(summary(model), correlation = FALSE)
      priors = get_priors_from_multilevel_model(model, mLvData,
                                                method = methodMultilevelPriors)
      if (rescalePriors) {
        priors = rescale_priors(priors, groups)
      }
      responses = responses %>%
        select(-"priorMean", -"priorSD") %>%
        left_join(priors,
                  by = intersect(setdiff(names(responses),
                                         c("priorMean", "priorSD")),
                                 names(priors)))
      responses$estTheta =
        draw_mcmc(items = estimatesTemp$items,
                  responses = responses,
                  echo = echo > 3L)
    }
    if (echo > 2L) print(compute_group_theta_pars(responses), row.names = FALSE, digits = 2L)
    if (echo > 1L) print(summary(model), correlation = FALSE)
    if (echo > 0L) message("  Final stage", ifelse(echo > 3L, ":", "."))
    for (nothing in seq_len(nItFinal)) {
      responses$estTheta = draw_mcmc(items = estimatesTemp$items,
                                     responses = responses,
                                     echo = echo > 3L)
    }
    if (echo > 1L) print(compute_group_theta_pars(responses), row.names = FALSE, digits = 2L)
    model = lme4::lmer(mLvFormula,
                       data = inner_join(mLvData, responses %>%
                                           select("id_obserwacji", "grupa",
                                                  theta = "estTheta"),
                                         by = c("id_obserwacji", "grupa")))
    if (echo > 1L) print(summary(model), correlation = FALSE)
    pvs = pvs %>%
      left_join(responses %>%
                  select("id_obserwacji", "examOut", "estTheta") %>%
                  rename_with(~paste0("pv_", chain - 1), "estTheta"),
                by = c("id_obserwacji", "examOut"))
    blups[[chain]] = get_schools_blups(model)
    rm(model)
    responses = responses %>%
      select(-any_of(c("estTheta", "estThetaSE", "marginalL",
                       "priorMean", "priorSD")))
  }
  return(list(pvs = pvs,
              blups = blups))
}
#' @importFrom dplyr %>% .data bind_cols bind_rows count filter group_by inner_join left_join mutate select summarise
#' @importFrom tidyr pivot_wider
load_data = function(dane, parametry) {
  stopifnot(!any(levels(dane$matura$grupa) %in% levels(dane$wejscie$grupa)),
            !any(levels(dane$wejscie$grupa) %in% levels(dane$matura$grupa)),
            !any(is.na(dane$warunkujace)))

  grupy = bind_rows(parametry$parametryWejscie %>%
                      filter(!is.na(.data$grupa)) %>%
                      select("grupa", "parametr", "wartosc"),
                    parametry$parametryMatura %>%
                      filter(!is.na(.data$grupa)) %>%
                      select("grupa", "parametr", "wartosc")) %>%
    mutate(parametr = sub("^group_", "", .data$parametr)) %>%
    pivot_wider(names_from = "parametr", values_from = "wartosc") %>%
    mutate(grupaNr = seq_along(.data$grupa)) %>%
    left_join(bind_rows(dane$wejscie %>%
                          count(.data$grupa, name = "nObs") %>%
                          mutate(grupa =
                                   levels(.data$grupa)[.data$grupa]),
                        dane$matura %>%
                          count(.data$grupa, name = "nObs") %>%
                          mutate(grupa =
                                   levels(.data$grupa)[.data$grupa])),
              by = "grupa") %>%
    select("grupa", "grupaNr", "mean", "sd", "nObs")

  zadania = bind_rows(parametry$parametryWejscie,
                      parametry$parametryMatura) %>%
    filter(is.na(.data$grupa)) %>%
    select(name = "kryterium", "model", "parametr", "wartosc") %>%
    group_by(.data$name, .data$model) %>%
    summarise(pars = list(setNames(.data$wartosc[order(.data$parametr)],
                                   .data$parametr[order(.data$parametr)])),
              respCats = list(c(0L, seq_along(grep("^b[[:digit:]]?$",
                                                   .data$parametr)))),
              nCats = length(.data$respCats[[1]]),
              .groups = "drop") %>%
    mutate(model = sub("PL$", "plm", .data$model))

  odpowiedzi =
    bind_rows(dane$wejscie %>%
                select("id_obserwacji", "grupa", matches("^([kp]|t[[:digit:]]+)_")) %>%
                mutate(grupa = levels(.data$grupa)[.data$grupa],
                       examOut = FALSE),
              dane$matura %>%
                select("id_obserwacji", "grupa", matches("^([kp]|t[[:digit:]]+)_")) %>%
                mutate(grupa = levels(.data$grupa)[.data$grupa],
                       examOut = TRUE))
  odpowiedzi = bind_cols(odpowiedzi %>%
                           select("id_obserwacji", "grupa", "examOut"),
                         responses = odpowiedzi %>%
                           select(matches("^([kp]|t[[:digit:]]+)_")) %>%
                           as.matrix()) %>%
    left_join(grupy %>%
                select("grupa", "grupaNr"),
              by = "grupa") %>%
    select("id_obserwacji", grupa = "grupaNr", "examOut", "responses")

  parametry = przygotuj_zbior_z_parametrami_dla_pvreg(
    parametry = bind_rows(parametry$parametryWejscie,
                          parametry$parametryMatura),
    kowariancje = bind_rows(parametry$kowariancjeWejscie,
                            parametry$kowariancjeMatura),
    grupy = grupy %>%
      select("grupa", "grupaNr", groupN_itemCATS = "nObs"))

  warunkujace =
    bind_rows(dane$wejscie %>%
                select("id_obserwacji", "grupa") %>%
                mutate(examOut = FALSE,
                       grupa = levels(.data$grupa)[.data$grupa]),
              dane$matura %>%
                select("id_obserwacji", "grupa") %>%
                mutate(examOut = TRUE,
                       grupa = levels(.data$grupa)[.data$grupa])) %>%
    left_join(grupy %>%
                select("grupa", "grupaNr"),
              by = "grupa") %>%
    select("id_obserwacji", grupa = "grupaNr", "examOut") %>%
    inner_join(dane$warunkujace,
               by = "id_obserwacji")

  return(list(groups = grupy %>%
                select(grupa = "grupaNr", "mean", "sd", "nObs"),
              items = zadania,
              responses = odpowiedzi,
              estimates = parametry,
              students = warunkujace,
              fixedEffectsFormula =
                paste0("theta ~ examOut*(",
                       paste(setdiff(names(dane$warunkujace),
                                     c("id_szkoly", "id_obserwacji")),
                             collapse = " + "),
                       ") + (1 + examOut | id_szkoly) + (1 | id_obserwacji)")))
}
#' @importFrom dplyr %>% .data filter group_by mutate select summarise
#' @importFrom tidyr pivot_wider
noisify_estimates = function(estimates) {
  estimates$estNoise = as.vector(
    mvtnorm::rmvnorm(1L, estimates$est,
                     as.matrix(estimates[, paste0(estimates$var, "_", estimates$par)])))
  estimates = estimates %>%
    select("var", "par", "est", "estNoise") %>%
    mutate(est = ifelse(.data$par %in% "c" & .data$estNoise <= 0,
                        0.0001, .data$estNoise),
           est = ifelse(.data$par %in% "c" & .data$estNoise >= 1,
                        0.9999, .data$estNoise),
           est = ifelse(.data$par %in% "sd_theta" & .data$estNoise <= 0,
                        0.0001, .data$estNoise))
  groupsNoise = estimates %>%
    filter(.data$par %in% c("mean_theta", "sd_theta")) %>%
    mutate(par = sub("_theta$", "", .data$par),
           grupa = as.integer(sub("^grupa_", "", .data$var))) %>%
    select("grupa", "par", "estNoise") %>%
    pivot_wider(names_from = "par", values_from = "estNoise")
  itemsNoise = estimates %>%
    filter(!(.data$par %in% c("mean_theta", "sd_theta"))) %>%
    mutate(model = sub("_.*$", "", .data$par),
           par = sub("^.*_", "", .data$par)) %>%
    group_by(.data$var, .data$model) %>%
    summarise(pars = list(setNames(.data$estNoise[order(.data$par)],
                                   .data$par[order(.data$par)])),
              respCats = list(c(0L, seq_along(grep("^b[[:digit:]]?$",
                                                   .data$par)))),
              nCats = length(.data$respCats[[1]]),
              .groups = "drop") %>%
    mutate(pars = ifelse(.data$model %in% "grm", # correcting for potential misorder of categories in GRM after adding noise
                         lapply(.data$pars, function(x) return(x[c(1L, 1L + order(x[-1L]))])),
                         .data$pars)) %>%
    select(name = "var", "model", "pars", "respCats", "nCats")
  return(list(groups = groupsNoise,
              items = itemsNoise))
}
#' @importFrom dplyr %>% .data group_by mutate summarise
estimate_eaps = function(items, groups, responses, nIP) {
  compute_likelihood_on_grid(items = items, groups = groups,
                             responses = responses, nIP = nIP) %>%
    group_by(.data$grupa, .data$examOut, .data$id_obserwacji) %>%
    mutate(wLikelihood = .data$w * .data$likelihood /
             sum(.data$w * .data$likelihood)) %>%
    summarise(estTheta = sum(.data$theta * .data$wLikelihood),
              estThetaSE = (sum(.data$theta^2 * .data$wLikelihood) - .data$estTheta^2)^0.5,
              .groups = "drop") %>%
    return()
}
#' @importFrom stats dnorm runif
#' @importFrom dplyr %>% .data left_join mutate
draw_mcmc = function(items, responses, echo = TRUE) {
  responses$randTheta = rnorm(nrow(responses), responses$estTheta, responses$estThetaSE)

  if (!("marginalL" %in% names(responses))) {
    responses$marginalL =
      compute_likelihood_on_thetas(items, responses, "estTheta")$likelihood
  }
  l = responses$marginalL *
    dnorm(responses$estTheta, responses$priorMean, responses$priorSD)
  marginalLRand =
    compute_likelihood_on_thetas(items, responses, "randTheta")$likelihood
  lRand = marginalLRand *
    dnorm(responses$randTheta, responses$priorMean, responses$priorSD)

  updates = (runif(nrow(responses), 0, 1) > (lRand / l)) %in% FALSE
  responses$updatedTheta = ifelse(updates, responses$randTheta, responses$estTheta)
  responses$marginalL = ifelse(updates, marginalLRand, responses$marginalL)
  if (echo) {
    message("    Updated ", format(sum(updates), big.mark = "'"),
            " out of ", format(length(updates), big.mark = "'"),
            " (dev.=", round(-2*sum(log(ifelse(updates, lRand, l))), 1L), ").")
  }
  return(responses$updatedTheta)
}
#' @importFrom dplyr %>% .data bind_rows filter mutate select
#' @importFrom tidyr expand_grid
compute_likelihood_on_grid = function(items, groups, responses, nIP) {
  responses = responses %>%
    split(responses$grupa)
  for (g in seq_along(responses)) {
    group = groups %>%
      filter(.data$grupa == unique(responses[[g]]$grupa))
    responses[[g]] = responses[[g]] %>%
      mutate(responses =
               .data$responses[, !apply(is.na(.data$responses), 2, all),
                               drop = FALSE]) %>%
      expand_grid(make_quad(group, nIP)) %>%
      mutate(likelihood = 1)
    for (i in seq_len(ncol(responses[[g]]$responses))) {
      item = items %>%
        filter(.data$name == colnames(responses[[g]]$responses)[i])
      responses[[g]]$likelihood = responses[[g]]$likelihood *
        compute_item_likelihood(x = responses[[g]]$responses[, i],
                                item = item,
                                theta = responses[[g]]$theta,
                                NAsAs1 = TRUE)
    }
    responses[[g]] = responses[[g]] %>%
      select("grupa", "examOut", "id_obserwacji", "theta", "w", "likelihood")
  }
  return(bind_rows(responses))
}
#
make_quad = function(group, nIP, useGH = FALSE, min = -6, max = 6) {
  stopifnot(is.data.frame(group), nrow(group) == 1L)
  if (useGH) {
    quad = lme4::GHrule(nIP, asMatrix = FALSE)[, c("z", "w")]
    names(quad) = c("theta", "w")
  } else {
    quad = data.frame(theta = seq(min, max, length.out = nIP))
    quad$w = dnorm(quad$theta)
    quad$w = quad$w / sum(quad$w)
  }
  quad$theta = quad$theta * group$sd + group$mean
  return(quad)
}
#' @importFrom dplyr %>% .data all_of bind_rows filter mutate select
compute_likelihood_on_thetas = function(items, responses, thetaName) {
  responses = responses %>%
    select("id_obserwacji", "grupa", "examOut", "responses", all_of(thetaName))
  names(responses)[names(responses) == thetaName] = "theta"
  responses = responses %>%
    split(responses$grupa)
  for (g in seq_along(responses)) {
    responses[[g]] = responses[[g]] %>%
      mutate(responses =
               .data$responses[, !apply(is.na(.data$responses), 2, all),
                               drop = FALSE]) %>%
      mutate(likelihood = 1)
    for (i in seq_len(ncol(responses[[g]]$responses))) {
      item = items %>%
        filter(.data$name == colnames(responses[[g]]$responses)[i])
      responses[[g]]$likelihood = responses[[g]]$likelihood *
        compute_item_likelihood(x = responses[[g]]$responses[, i],
                                item = item,
                                theta = responses[[g]]$theta,
                                NAsAs1 = TRUE)
    }
    responses[[g]] = responses[[g]] %>%
      select("grupa", "examOut", "id_obserwacji", "likelihood")
  }
  return(bind_rows(responses))
}
#
compute_item_likelihood = function(x, item, theta, NAsAs1 = TRUE) {
  stopifnot(is.data.frame(item), nrow(item) == 1L)
  pars = as.list(item$pars[[1]])
  respCats = item$respCats[[1]]

  if (item$model == "3plm") {
    p = pars$c + (1 - pars$c) / (1 + exp(-pars$a * (theta - pars$b)))
    p = ifelse(x == 1, p, 1 - p)
  } else if (item$model == "2plm") {
    p = 1 / (1 + exp(-pars$a * (theta - pars$b)))
    p = ifelse(x == 1, p, 1 - p)
  } else if (item$model == "grm") {
    bPars = unlist(pars[grep("^b", names(pars))])
    p = rep(NA_real_, length(x))
    for (r in seq_along(respCats)) {
      if (r == 1L) {
        p = ifelse(x == respCats[r],
                   1 / (1 + exp(pars$a * (theta - bPars[r]))),
                   p)
      } else if (r == length(respCats)) {
        p = ifelse(x == respCats[r],
                   1 - 1 / (1 + exp(pars$a * (theta - bPars[r - 1L]))),
                   p)
      } else {
        p = ifelse(x == respCats[r],
                   1 / (1 + exp(-pars$a * (theta - bPars[r - 1L]))) -
                     1 / (1 + exp(-pars$a * (theta - bPars[r]))),
                   p)
      }
    }
  } else if (item$model == "gpcm") {
    bPars = unlist(pars[grep("^b", names(pars))])
    expCats = matrix(NA_real_, nrow = length(theta), ncol = length(respCats))
    expCats[, 1L] = 1
    for (r in seq_along(expCats)[-1L]) {
      expCats[, r] = exp(pars$a * (respCats[r]*theta - sum(bPars[1L:(r - 1L)])))
    }
    expCats = expCats / rowSums(expCats)
    p = rep(NA_real_, length(x))
    for (r in seq_along(expCats)) {
      p = ifelse(x == respCats[r], expCats[, r], p)
    }
  } else {
    stop("Nieobsługiwany rodzaj modelu odpowiedzi na zadanie: '", item$model, "'.")
  }
  if (NAsAs1) p = ifelse(is.na(p), 1, p)
  return(p)
}
#' @importFrom stats coef formula model.frame model.matrix rnorm simulate vcov
#' @importFrom dplyr %>% .data inner_join mutate pick select starts_with
#' @importFrom tidyr pivot_longer pivot_wider
get_priors_from_multilevel_model = function(model, mLvData,
                                                       method = c("simulateIndep",
                                                                  "simulateCond",
                                                                  "predRandResVar",
                                                                  "predRandAllVars",
                                                                  "predFixedAllVars")) {
  method = match.arg(method)
  if (method == "simulateIndep") {
    simdata = simulate(model, nsim = 100, re.form = ~0)
    return(mLvData %>%
             select("id_obserwacji", "grupa", "examOut") %>%
             bind_cols(priorMean = rowMeans(simdata),
                       priorSD = (rowMeans(simdata^2) - rowMeans(simdata)^2)^0.5))
  } else if (method == "simulateCond") {
    simdata = simulate(model, nsim = 100, re.form = NULL)
    return(mLvData %>%
             select("id_obserwacji", "grupa", "examOut") %>%
             bind_cols(priorMean = rowMeans(simdata),
                       priorSD = (rowMeans(simdata^2) - rowMeans(simdata)^2)^0.5))
  } else {
    if (method == "predFixedAllVars") {
      fixefs = matrix(lme4::fixef(model), ncol = 1)
    } else {
      fixefs = t(mvtnorm::rmvnorm(1, lme4::fixef(model), as.matrix(vcov(model))))
    }
    dataTemp = model.matrix(model)
    uniquePatterns = !duplicated(dataTemp)
    dataTemp = cbind(model.frame(model)[uniquePatterns,
                                        !(names(model.frame(model)) %in%
                                            c(names(lme4::VarCorr(model)),
                                              all.vars(formula(model))[1]))],
                     `_predRand_fixed` = dataTemp[uniquePatterns, ] %*% fixefs,
                     `_var_fixed` = apply(dataTemp[uniquePatterns, ], 1,
                                          function(x, vcov) {
                                            x = matrix(rep(x, length(x)),
                                                       ncol = length(x))
                                            return(sum(vcov * x * t(x)))
                                          },
                                          vcov = as.matrix(vcov(model))))
    mLvData = left_join(data.frame(model.frame(model)), # aby pozbyć się niepotrzebnych atrybutów
                        mLvData %>%
                          select("id_obserwacji", "examOut", "id_szkoly", "grupa"),
                        by = c("id_obserwacji", "examOut", "id_szkoly")) %>%
      left_join(dataTemp,
                by = setdiff(names(dataTemp),
                             c("_predRand_fixed", "_var_fixed")))
    mLvData$`_var_indError` = attributes(lme4::VarCorr(model))$sc^2

    groupRandEffs = lme4::ranef(model, condVar = TRUE)
    groupRandEffs = mapply(
      function(x, nm, rand) {
        if (nm == "id_obserwacji") {
          stopifnot(ncol(x) == 1, names(x) == "(Intercept)")
          if (rand) {
            means = rnorm(nrow(x), x[, 1],
                          apply(attributes(x)$postVar, 3,
                                identity, simplify = TRUE))
          } else {
            means = x[, 1]
          }
          x = data.frame(id_obserwacji = as.integer(rownames(x)),
                         `_predRand_id_obserwacji` = means,
                         `_var_id_oserwacji` = apply(attributes(x)$postVar, 3,
                                                     as.vector, simplify = TRUE),
                         check.names = FALSE)
          return(x)
        } else if (nm == "id_szkoly") {
          stopifnot(ncol(x) == 2,
                    all(c("(Intercept)", "examOutTRUE") %in% names(x)))
          if (rand) {
            means = data.frame(
              t(mapply(mvtnorm::rmvnorm,
                       n = rep(1, nrow(x)),
                       mean = apply(x, 1, identity, simplify = FALSE),
                       sigma = apply(attributes(x)$postVar, 3, identity,
                                     simplify = FALSE),
                       SIMPLIFY = TRUE)))
            names(means) = names(x)
          } else {
            means = x
          }
          vars = t(apply(attributes(x)$postVar, 3, identity, simplify = TRUE))
          x = rbind(data.frame(id_szkoly = as.integer(rownames(x)),
                               examOut = FALSE,
                               `_predRand_id_szkoly` = means[, "(Intercept)"],
                               `_var_id_szkoly` =
                                 vars[, ifelse(names(x)[1] == "(Intercept)", 1, 4)],
                               check.names = FALSE),
                    data.frame(id_szkoly = as.integer(rownames(x)),
                               examOut = TRUE,
                               `_predRand_id_szkoly` = rowSums(means),
                               `_var_id_szkoly` = rowSums(vars),
                               check.names = FALSE))
          return(data.frame(x, check.names = FALSE))
        } else {
          stop("Obsługiwane są wyłącznie efekty grupowania po 'id_obserwacji' lub 'id_szkoly'.")
        }
      },
      groupRandEffs,
      names(groupRandEffs),
      rand = method != "predFixedAllVars",
      SIMPLIFY = FALSE)

    for (e in seq_along(groupRandEffs)) {
      mLvData = inner_join(mLvData,
                           groupRandEffs[[e]],
                           by = intersect(names(mLvData),
                                          names(groupRandEffs[[e]])))
    }
    if (method == "predRandResVar") {
      return(mLvData %>%
               mutate(priorMean = rowSums(pick(starts_with("_predRand_"))),
                      priorSD = .data$`_var_indError`^0.5) %>%
               select("id_obserwacji", "grupa", "examOut", "priorMean", "priorSD"))
    } else {
      return(mLvData %>%
               mutate(priorMean = rowSums(pick(starts_with("_predRand_"))),
                      priorSD = rowSums(pick(starts_with("_var_")))^0.5) %>%
               select("id_obserwacji", "grupa", "examOut", "priorMean", "priorSD"))
    }
  }
}
#' @importFrom stats rnorm sd
#' @importFrom dplyr %>% .data any_of group_by left_join mutate n select
rescale_priors = function(priors, groups) {
  priors %>%
    mutate(randTheta = rnorm(n(), .data$priorMean, .data$priorSD)) %>%
    left_join(groups,
              by = "grupa") %>%
    group_by(.data$grupa) %>%
    mutate(priorMean = (.data$priorMean - mean(.data$randTheta) *
                              (.data$sd / sd(.data$randTheta)) + .data$mean),
           priorSD = .data$priorSD * (.data$sd / sd(.data$randTheta))) %>%
    ungroup() %>%
    select(-any_of(c("randTheta", setdiff(names(groups), "grupa")))) %>%
    return()
}
#' @importFrom dplyr %>% .data group_by summarise
compute_group_theta_pars = function(responses) {
  responses %>%
    group_by(.data$grupa) %>%
    summarise(mean = mean(.data$estTheta),
              sd = sd(.data$estTheta),
              .groups = "drop") %>%
    as.data.frame() %>%
    return()
}
#
get_schools_blups = function(model) {
  blups = lme4::ranef(model, condVar = TRUE, whichel = "id_szkoly")$id_szkoly
  stopifnot(all(names(blups) == c("(Intercept)", "examOutTRUE")))
  names(blups) = sub("^\\(Intercept\\)$", "mean_schl", names(blups))
  names(blups) = sub("^examOutTRUE$", "eva_schl", names(blups))
  covs = t(apply(attributes(blups)$postVar, 3, as.vector, simplify = TRUE)[c(1, 4, 2), ])
  return(cbind(id_szkoly = as.integer(rownames(blups)),
               blups,
               mean_schl_se = covs[, 1]^0.5,
               eva_schl_se = covs[, 2]^0.5,
               cov_mean_eva_schl = covs[, 3]))
}
#' @importFrom dplyr %>% .data mutate select
calculate_school_reff = function(blups) {
  if (length(blups) == 1) {
    return(blups[[1]])
  }
  results = list(
    id_szkoly = blups[[1]]$id_szkoly,
    mean_schl_imputVar =
      do.call(cbind, args = lapply(blups, function(x) x$mean_schl)),
    eva_schl_imputVar =
      do.call(cbind, args = lapply(blups, function(x) x$eva_schl)),
    cov_mean_eva_schl =
      rowMeans(do.call(cbind, args = lapply(blups, function(x) x$cov_mean_eva_schl))),
    mean_schl_sampVar =
      rowMeans(do.call(cbind, args = lapply(blups, function(x) x$mean_schl_se))^2),
    eva_schl_sampVar =
      rowMeans(do.call(cbind, args = lapply(blups, function(x) x$eva_schl_se)))^2)
  results$mean_schl = rowMeans(results$mean_schl_imputVar)
  results$eva_schl = rowMeans(results$eva_schl_imputVar)
  results$mean_schl_imputVar =
    rowMeans((results$mean_schl_imputVar - results$mean_schl)^2)
  results$eva_schl_imputVar =
    rowMeans((results$eva_schl_imputVar - results$eva_schl)^2)
  results %>%
    as.data.frame() %>%
    mutate(mean_schl_se =
             (.data$mean_schl_sampVar +
                (1 + 1/length(blups))*.data$mean_schl_imputVar)^0.5,
           eva_schl_se =
             (.data$eva_schl_sampVar +
                (1 + 1/length(blups))*.data$eva_schl_imputVar)^0.5) %>%
    select("id_szkoly", "mean_schl", "mean_schl_se", "eva_schl", "eva_schl_se",
           "cov_mean_eva_schl") %>%
    return()
}
