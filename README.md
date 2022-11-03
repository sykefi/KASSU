# KASSU: väestö- ja asuntokantaennustetyökalu

## Taustaa
KASSU-työkalu on kehitetty kuntien asumisen suunnittelun tueksi. Työkalu koostuu väestö- ja asuntokantaennustemallista ja on tulosta Asumisen rahoitus- ja kehittämiskeskuksen (ARA) ja Suomen ympäristäkeskuksen (SYKE) yhteishankkeesta. Työkalu on kehitetty SYKEssä.

<br/><br/>

<p align="center">
<img src= "/kuvat/KASSU_malli_kuva.png" alt="KASSU-mallin vaiheet kaavio" width="700" height="300">
<figcaption><strong>Kuva 1. KASSU-mallin vaiheet</strong></figcaption>
</p>

## Väestöennuste

- Väestöennuste on mallinnettu aluekohtaisesti 250 m x 250 m ruudukolla 1-vuotisikäryhmille sukupuolittain kohorttikomponentti-menetelmällä, joka vanhentaa väestöä vuosittain huomioiden eri komponenttien vaikutuksen. 
- Mallin komponentteina toimivat kuntien välinen muuttoliike, maahanmuutto, kuolleisuus ja syntyvyys. 
- Malli mahdollistaa ennusteen sekä kuntakohtaisesti, että halutulle aluejaolle kunnan sisällä (esim. pienaluejako). 
- Koska ennuste on kuntapohjainen, se ei huomioi väestönkehitystä kunnan ulkopuolella, kuten lähtömuuttoalueiden väestönkehitystä.
- Ennustekausi on tällä hetkellä vuoteen 2040 asti. 


### Väestöennusteessa käytetyt aineistot

| Aineiston nimi| Lähde | Aikasarja | Aluetaso | Lisätiedot |
|---------------|-------|-----------|----------|------------|
| RHR-rakennuspisteet | RHR (Digi- ja väestötietovirasto) | 2019 | Piste | 1-v. ikäryhmät |
| YKR-muuttoliikeaineisto | YKR (Tilastokeskus) | 2015-2019 | Ruutu | Ikäryhmät 3 vuoden välein välillä 0-74 sekä yli 75-vuotiaat |
| Väestöennusteen hedelmällisyyskertoimet iän mukaan | StatFIn (Tilastokeskus) | 2019-2040 | Kunta | Laskettu 2015-2019 tietojen perusteella |
| Väestöennusteen kuolleisuuskertoimet iän ja sukupuolen mukaan | StatFIn (Tilastokeskus) | 2019-2040 | Kunta | Laskettu 2008-2014 tietojen perusteella |
| Maahanmuutto iän ja sukupuolen mukaan | StatFin (Tilastokeskus) | 2015-2019 | Kunta | Ikäryhmät 4 vuoden välein välillä 0-101 |

### Väestöennustemallin toiminta
Väestöennustemalli koostuu yhdestä pääfunktiosta, joka jakautuu useampaan alifunktioon.

### Output esimerkit

## Asuntokunta- ja asuntokantaennuste
Laskenta pohjautuu väestöennusteeseen. Ennusteessa ei erotella väestöä sukupuolen mukaan. 

#### Laskennan vaiheet tiivistetysti:
1. Väestön jaotellaan yhdeksään ikäryhmään
2. Jaoteltu väestö jaetaan ikäryhmittäin kolmeen luokkaan perhekoon nykytilakertoimien perusteella (yhden, kahden tai kolmen ja sitä suuremmat asuntokunnat). 
3. Väestön muuntaminen asuntokunniksi asuntokuntien muodostuskertoimien perusteella. Näin saadaan asuntokuntien kokonaismäärät eri kokoluokissa (1 hlöä, 2 hlöä ja 3+ henkilön asuntokunnat).

### Output esimerkit
