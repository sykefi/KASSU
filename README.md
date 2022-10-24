# KASSU: väestö- ja asuntokantaennustetyökalu

## Taustaa
KASSU-työkalu on kehitetty kuntien asumisen suunnittelun tueksi. Työkalu koostuu väestöennuste- ja asuntokantaennustemallista ja on tulosta Asumisen rahoitus- ja kehittämiskeskuksen (ARA) ja Suomen ympäristäkeskuksen (SYKE) yhteishankkeesta. Työkalu on kehitetty SYKEssä.

## Väestöennuste

Väestöennuste on mallinnettu aluekohtaisesti 250 m x 250 m ruudukolla 1-vuotisikäryhmille sukupuolittain kohorttikomponentti-menetelmällä, joka vanhentaa väestöä vuosittain huomioiden eri komponenttien vaikutuksen. Mallin komponentteina toimivat kuntien välinen muuttoliike, maahanmuutto, kuolleisuus ja syntyvyys. Malli mahdollistaa ennusteen sekä kuntakohtaisesti, että halutulle aluejaolle kunnan sisällä (esim. pienaluejako). Ennustekausi on tällä hetkellä vuoteen 2040 asti. Koska ennuste on kuntapohjainen, se ei huomioi väestönkehitystä kunnan ulkopuolella, kuten lähtömuuttoalueiden väestönkehitystä.


### Väestöennusteessa käytetyt aineistot

- Rakennus- ja huoneistorekisteri, RHR (Digi -ja väestötietovirasto)
- YKR-muuttoliikeaineisto (Tilastokeskus)

Avoimet Tilastokeskuksen aineistot:
- Väestöennusteen kunnittaiset hedelmällisyyskertoimet iän mukaan 
- Väestöennusteen kunnittaiset kuolleisuuskertoimet iän ja sukupuolen mukaan 
- Kunnittainen maahanmuutto iän ja sukupuolen mukaan

### Väestöennustemallin toiminta
Väestöennustemalli koostuu yhdestä pääfunktiosta, joka jakautuu useampaan alifunktioon.

