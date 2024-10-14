# EWDbk

Pakiet zawiera funkcje automatyzujące proces tworzenia i edycji skal, opisujących sposób łączenia i ew. przetwarzania kryteriów oceny przed przystąpieniem do skalowania. 

## Instalacja / aktualizacja

Pakiet nie jest wypchnięty na CRAN-a, więc instalować trzeba ze źródeł.

Ponieważ jednak zawiera jedynie kod w R, nie ma potrzeby zaopatrywać się w kompilatory, itp.

Instalacja możliwa jest w dwóch wariantach **(aby zaktualizować pakiet do najnowszej wersji należy zrobić dokładnie to samo)**:

1) Z użyciem pakietu devtools:
```r
install.packages('devtools') # potrzbne tylko, gdy nie jest jeszcze zainstalowany
devtools::install_github('tzoltak/EWDbk')
```

**Jeśli podczas instalacji napotkasz na błąd, a używasz linuksa** sprawdź, czy nie dotyczy Cię [ten problem](https://github.com/hadley/devtools/issues/650) lub przeprowadź "uczciwą instalację ze źródeł" (patrz niżej).

2) "Uczciwa instalacja ze źródeł":

   * Pobrać z sieci i zainstalować [narzędzia GIT-a dla linii komend](http://git-scm.com/downloads) 
   
   * W konsoli wywołać:
```r
git clone https://github.com/tzoltak/EWDbk.git
R CMD INSTALL EWDskale
```

Aby zainstalować pakiet Pythona *pvreg*:

- zainstaluj Pythona 3.10.10, wykonaj aktualizację PIPa, zainstaluj pakiet pozwalający tworzyć środowiska wirtualne Pythona, a następnie utwórz środowisko, w którym będzie działał pvreg (nie gryząc się wersjami swoich zależności z niczym innym):

```
py -m pip install --upgrade pip
py -m pip install --user virtualenv
py -m venv py31010pvreg
```

Zainicjalizuj utworzone środowisko wirtualne, zaktualizuj w nim PIPa i zainstaluj przy jego pomocy pakiet *pvreg* z GitHuba:

```
.\py31010pvreg\Scripts\activate
py -m pip install --upgrade pip
pip install git+https://github.com/bkondratek/pvreg
```

Aby zaktualizować *pvreg*:

```
.\py31010pvreg\Scripts\activate
pip uninstall pvreg
pip install git+https://github.com/bkondratek/pvreg
```
