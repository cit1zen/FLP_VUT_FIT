# FLP 2018 - funkcionálny projekt

## Popis

Aplikácia napísaná v jazyku Haskell, ktorá prevádza práve lineárne gramatika na nedeterministické konečné automaty. 

## Použitie

Aplikáciu je pred použitím nutné vybuildovať, to je možne spraviť pomocou:
```bash
make
```

Výsledný súbor `plg-2-nka` je potom možné spustiť pomocou:
```bash
plg-2-nka <-i | -1 | -2> [input-file] 
```

Pre vymazanie aplikácie a súborov vzniknutých pri buildovaní použite:
```bash
make clean
```

## Možné problémy

Poradie stavov a pravidiel nemusí zodpovedať poradiu z oficiálnych príkladov.
Výsledné gramatike alebo automaty by však mali byt ekvivalentné tým z príkladov v dátovom sklade.