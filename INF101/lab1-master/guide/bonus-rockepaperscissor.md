[forrige](07-twodimentionallists.md) • [oversikt](../README.md) • neste

# Bonusoppgave: Rock Paper Scissors
Dette programmet samler alle konseptene fra de andre oppgavene. Hvis du klarer å løse denne har du god kontroll på Java-syntaksen 👏 🙌

**Denne oppgaven må IKKE gjøres for å få godkjent på lab1. Den er helt frivillig.**

Det vellkjente spillet *Stein, Saks, Papir* var et eksamensspørsmål i [INF100 vår 2020](https://mitt.uib.no/courses/29690/files/3490285/download?wrap=1) (oppgave 14). I denne labben vil vi implmenetere det samme programmet, men istedenfor det velkjente Python-språket vil vi bruke Java.

Målet med denne labben er å lære dere Java-syntaks. Hvis du ikke husker alle programmeringskonseptene fra INF100 og trenger en oppfriskning så har vi her implementert [Stein Saks Papir i Python](https://git.app.uib.no/ii/inf101/22v/students/lab1/-/blob/master/RockPaperScissors.py) for deg.

### Oppgave
Skriv et Java-program hvor et menneske spiller mot datamaskinen. Mennesket skal bli bedt om deres valg av stein, saks eller papir i konsollen, mens datamaskinen skal velge tilfeldig mellom de tre valgene. Mennesket skal spørres om de ønsker å spille videre etter hver runde. Scoren til begge spillerne skal lagres og presenteres gjennom alle rundene. All input fra mennesket skal valideres og det skal ikke tillates at programmet kræsjer ved feil input (se runde 3 i eksempelkjøring).

**Viktig**: Den eksiterende koden må fobli slik den er ved kloning. Implementer programmet i `run`. Du kan opprette hjelpemetoder (anbefalt) etter dine behov. Bruk metoden `readInput` for innlesning av input fra brukeren.

Programmet skal printe ut det følgende i konsollen (inkludert input fra brukeren):
```
Let's play round 1
Your choice (Rock/Paper/Scissors)?
rock
Human chose rock, computer chose paper. Computer wins!
Score: human 0, computer 1
Do you wish to continue playing? (y/n)?
y
Let's play round 2
Your choice (Rock/Paper/Scissors)?
paper
Human chose paper, computer chose paper. It's a tie!
Score: human 0, computer 1
Do you wish to continue playing? (y/n)?
y
Let's play round 3
Your choice (Rock/Paper/Scissors)?
cardboard
I do not understand cardboard. Could you try again?
Your choice (Rock/Paper/Scissors)?
scissors
Human chose scissors, computer chose paper. Human wins!
Score: human 1, computer 1
Do you wish to continue playing? (y/n)?
n
Bye bye :)
```

✅ Kjør `RockPaperScissorsTest.java` for å teste om du har løst denne oppgaven riktig.

Testene sjekker hvorvidt output'en i konsollen er **akkurat** slik som i eksempel-output ovenfor.