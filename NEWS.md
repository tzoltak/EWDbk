# Do zrobienia

## W pierwszej kolejności

- Dodać sprawdzanie zgodności plików z danymi już znalezionych na dysku z nowo
  przygotowanymi w przypadku, gdy `nadpisz=FALSE` (w tym wartości parametrów!).

## Na być może kiedyś

- Dodać możliwość obliczania zagregowanych wskaźników EWD w sposób traktujący
  agregowane wskaźniki *jednoroczne* jako skorelowane zmienne losowe
  (kowariancje do obliczenia empirycznie z danych).
- Dodać wywoływanie `pvreg` z wewnątrz R za pośrednictwem pakietu *reticulate*,
  (pamiętać, że to musi zostać zrobione korzystając z odpowiedniego środowiska
  Pythona, które nie jest domyślnym używanym przez *reticulate*!) co stało się
  możliwe, od kiedy Bartek opublikowała `pvreg` jako pakiet Pythona.
- Doprowadzić do poprawnego działania część kodu będącą portem `pvreg` do R
  (w tej chwili ta część pakietu jest wyłączona z użytku).


# EWDbk 0.2.1 (04.11.2024)

- Dodano sprawdzanie przez `pobierz_parametry_egzaminow()`, czy wśród kolumn
  zwróconych przez `ZPD::pobierz_parametry()` jest też *id_elementu* (po
  tajemniczej przygodzie ze zmianami definicji widoków w bazie, która
  doprowadziła do braku tejże kolumny i w konsekwencji niemożliwości pobrania
  z bazy kowariancji, co jednak wywoływało błąd dopiero dużo dale w procesie).

# EWDbk 0.2 (14.10.2024)

- Nowa funkcja `agreguj_ewd()` pozwala obliczyć *wieloletnie* wskazniki EWD
  poprzez agregację wskaźników *jednorocznych* (czy to wczytanych z bazy danych,
  czy z plików .RData zapisanych na dysku).
- Nowa funkcja `podsumuj_wlasnosci_wskaznikow_ewd()` przygotowuje proste
  zestawienie najważniejszych własności wskaźników EWD zapisanych w obiekcie
  klasy *listaWskaznikowEWD*.
- Poprawiono błąd w kryterium określającym, czy w danym skalowaniu trzeba
  utworzyć zmienne opisujące wybór tematów (`typ_pytania` nie jest dobrym
  kryterium ze względu na fakt istnienia pseudokryteriów oceny).
- Format argumentu `kryteriaUsuwaniaZadan` w `skaluj_matura_bk()`
  i `skaluj_we_bk()` zmieniono z listy na wektor liczb.
- Gruntownie uzupełniono dokumentację. Przy okazji wprowadzono kilka uspójnień
  w nazwach analogicznych argumentów w różnych funkcjach.
- Gruntownie uzupełniono asercje, choć ich szczegółowość nie rozkłada się
  równomiernie w ramach pakietu (ale też złożoność struktur danych będących
  argumentami jest bardzo różna pomiędzy różnymi funkcjami).
- W obliczeniach jednorocznych wskaźników EWD wprowadzono przypisywanie nowych
  kodów problemów związanych z liczbą uczniów (o wartościach powyżej 300),
  których opisy w bazie odnoszą się do egzaminu ósmoklasisty, dla wskaźników
  obliczanych na podstawie wyników egzaminu ósmoklasisty.

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
