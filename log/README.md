# Riešenie rubikovej kostky v jazyku PROLOG
##### Login: xorman00
##### Autor: Adam Ormandy

## Popis

Implementácia programu ktorý rieši 3x3 rubikovu kostku. Algoritmus použitý na riešenie je prehladávanie grafu do šírky, kde základom je stav kocky ktorý sme dostali na vstup.

Testy ukazujúce funkčnosť sa nachádzajú v priečinku `tests`. Program bol otestovaný pomocou SWI Prolog 7.6.4-1.

## Trvanie testov

Všetky testy by mali skončiť do 30 sekúnd. Testy `simple*.in` a oficiálny príklad by mali vrátiť výsledok v podstate okamžite.

## Obmedzenie

Z časových dôvodov sa neodporúča riešiť kocky, ktorých riešenie trvá viac ako 6 ťahov, kedže kvoli explózií priestorú riešení resp. grafu môže riešenie trvať aj niekoľko hodín. Program sa eventuálne dopracuje k riešeniu ale komu sa chce čakať pol dňa.

## Použitie

Príklad spustenia:
```bash
make
./flp18-log <tests/zadanie.in >vysledok.out
```

Príklad vstupného súboru:
```
011
011
011
355 222 335 444 
355 222 335 444 
222 335 444 355 
000
000
111
```

Príklad riešenia:
```
011
011
011
355 222 335 444
355 222 335 444
222 335 444 355
000
000
111

011
011
011
355 222 335 444
355 222 335 444
355 222 335 444
100
100
100

511
511
511
055 222 331 444
055 222 331 444
055 222 331 444
300
300
300

111
111
111
555 222 333 444
555 222 333 444
555 222 333 444
000
000
```