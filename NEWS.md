# Do zrobienia

## Na teraz

- Poprawić dokumentację pakietu.
- Obłożyć funkcje większą ilością asercji.
- Dodać sprawdzanie zgodności plików z danymi już znalezionych na dysku z nowo
  przygotowanymi w przypadku, gdy `nadpisz=FALSE` (w tym wartości parametrów!).
- Dopisać tworzenie raportu z krótkim podsumowaniem własności skalowań i wskaźników.

## Na być może kiedyś

- Dodać wywoływanie `pvreg` z wewnątrz R za pośrednictwem pakietu *reticulate*,
  (pamiętać, że to musi zostać zrobione korzystając z odpowiedniego środowiska
  Pythona, które nie jest domyślnym uzywanym przez *reticulate*!) co stało się
  możliwe, od kiedy Bartek opublikowała `pvreg` jako pakiet Pythona.
- Doprowadzić do poprawnego działania część kodu będącą portem `pvreg` do R
  (w tej chwili ta część pakietu jest wyłączona z użytku).

# EWDbk 0.1.999 (14.06.2024)

- Poprawiono błąd w kryterium określającym, czy dany zdający egzamin na wejściu
  jest osobą o wydłużonym toku kształcenia w `okresl_grupe()`, dzięki czemu
  znów działa przygotowywanie danych dla zdających maturę w 2022 roku
  (zamieszanie z likwidacją gimnazjów).
- Funkcja `znajdz_skale()` zwraca oddzielnie maksymalny numer skalowania
  i maksymalny numer skalowania z wyłączeniem tych spośród nich, które opisują
  wynik działania *pvreg* (a więc będących *efektem ubocznym* obliczania
  wskaźników EWD szkół). Dzięki temu pierwszy z nich może być w ramach
  `oblicz_ewd_bk()` wykorzystany do przypisania numeru skalowania, w którym
  zostaną zapisane wyniki działania *pvreg*, a drugi do pobrania z bazy
  wcześniej wyskalowanych wartości parametrów.
- Pakiet wciąż cierpi na dramatyczny deficyt dokumentacji.

# EWDbk 0.1.99 (10.12.2023)

- Dopracowanie całego procesu w toku obliczania wskaźników dla lat 2020-2022
  oraz 2023.
- Pakiet cierpi jeszcze na dramatyczny deficyt dokumentacji.

# EWDbk 0.1 (14.12.2022)

- Pierwsza wersja pakietu.
