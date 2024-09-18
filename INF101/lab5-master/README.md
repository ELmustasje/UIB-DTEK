# Lab 5

## Læringsmål

- Bruke `generics`
- Kunne oversette ikke-generiske klasser til å bli generisk
- Bruke `exceptions`

## 5.0 Gjør deg kjent med koden

I denne oppgaven skal vi ta utgangspunkt i to forskjellige grid. Ett rutenett fyllt med verdier av typen `CellState` og ett fyllt med verdier av typen `LabyrinthTile`.

Uavhengig av hvilke datatype gridet består av vil oppførselen være lik. Rutenettet sin oppgave er bare å holde cellen på den riktige plassen i datastrukturen. Med andre ord: Grid trenger ikke å vite noe om det elementet den holder.

I lab5 har vi introdusert flere ferdig-implementerte celleautomater. Vi har også lagt inn et (relativt enkelt) labyrinth-spill som bruker en grid til å representere en spillbane.

En [*celleautomat*](https://en.wikipedia.org/wiki/Cellular_automaton) er et rutenett hvor hvert felt har en cellstate. Feltene oppdateres basert på et sett med regler. Hvilken cellstate en celle skal ha bestemmes av hvilke celler som befinner seg rundt den. Det er **mye** å lære om celleautomater, men det er pensum for et annet emne. Her skal vi bare lett bruke dem.

![game-of-life](http://upload.wikimedia.org/wikipedia/commons/e/e5/Gospers_glider_gun.gif "Game Of Life")

**5.0.1)** Kjør de tre celleautomatene ved å kjøre `Main` i pakken `cellular` og bytte på hvilke linje som er kommentert ut i main-metoden. Alle tre automatene skal fungere.

**5.0.2)** Kjør `Main` i `labyrinth`-pakken og sjekk at du kan navigere spilleren i begge versjoner.

**5.0.3)** Kjør testene i prosjektet og forsikre deg om at alle testene untatt LabyrinthTest blir grønne. **`GenericGridTest` vil også feile pga. syntax-errors. Disse må du fikse opp i etter du har fullført oppgave 5.1**.

![](img/junitex.gif)

✅ Du kan gå videre når du har sjekket at alle programmene kjører og testene i `GridTest`, `CellAutomatonTest` og `CellStateTest` passerer.


PS til Intellij -brukere: ingen tester vil kjøre så lenge det finnes kompileringsfeil i koden, uansett om kompileringsfeilen er i den samme filen du ønsker å kjøre eller ikke. I den utleverte klassen `GenericGridTest` er det kompileringsfeil. For å kjøre de *andre* testene, kan du kommentere bort testene i GenericGridTest midlertidig.

> 🤔 _Hvorfor starter vi oppgaven med et ferdigskrevet program som fungerer? Mye av objekt-orientert programmering handler om å gjenbruke kode, redusere duplikasjon, og å fikse bugs. Dette er tema i denne labben._

## 5.1 Generics

_I denne oppgaven skal vi bruke Java generics til å erstatte `CellStateGrid` og `LabyrinthTileGrid` med én felles Grid-klasse._

**5.1.1)** Sammenlign de to grid-klassene `CellStateGrid` og `LabyrinthTileGrid`. Sammenlign også interfacene de implementerer.

🤔 Har de noe til felles? Har de noen ulikheter?

I dette tilfellet er det både unødvendig og upraktisk å ha to grid-klasser som fungerer likt. Hvis vi skal gjøre en endring i hvordan et grid fungerer så må endringen gjøres flere plasser. Hvis vi skal legge til mer funksjonalitet til griddet vårt (f.eks. en metode `getNumberOfNonNullElements()` som teller antall elementer som ikke er `null`) må vi også gjøre dette to ganger. I tillegg _skalerer_ dette dårlig: Hva skjer når vi skal bruke grid-datastrukturen vår for andre typer, som f.eks. en matrise med tall (`Integer`/`Double`) eller farger i et bilde?

For å rydde opp må vi gjøre griddet generisk, som vil si at gridet kan brukes for helt arbitrære `datatyper`. Dette lar seg gjøre nettopp fordi griddet ikke trenger å vite noe om typen elementer den skal holde. Dette gjelder generelt for objekter som fungerer som en beholder for andre ting: lister, mengder, oppslagsverk etc.

Vi skal nå gjøre `CellStateGrid` generisk, slik at vi ikke har bruk for å flere grid-klasser (med mindre rutenettene skal _oppføre_ seg annerledes, selvfølgelig).

**5.1.2)** Gi nytt navn til klassen `CellStateGrid` slik at den bare heter `Grid`. Her kan du bruke det meget nyttige refaktoreringsverktøyet til Eclipse/Intellij ved å høyreklikke på klassenavnet -> `Refactor` -> `Rename`. Ikke bare vil dette endre filnavnet og klassenavnet i filen, men alle referansene inne i de andre klassene også. I VSCode kan du markere klassenavnet og trykke `F2` eller høyreklikke og velge *rename*.

Vi kan også endre `ICellStateGrid` til å bli et generisk interface med navn `IGrid` på samme måte som du gjorde med `Grid`.
`(add-commit-push)`

**5.1.3)** Gjør klassen `Grid` og grensesnittet `IGrid` generisk ved å legge til typeparameteren `<T>` bak klassenavnet. Gridet sier nå at "jeg vet ikke hvilken type jeg kommer til å holde på, men la oss kalle den for `T`". (Merk at du kunne ha brukt andre navn enn `T`, og det trenger ikke å bare være én bokstav selv om det ofte er vanlig). Forvent at du får en del feilmeldinger og/eller advarsler når du gjør slike endringer. `(add-commit-push)`

(Merk at om du committer endringer som gjør at programmet midlertidig ikke kompilerer kan det være irriterende for andre som jobber på prosjektet. Siden du jobber på ditt eget prosjekt og ikke på et stort prosjekt der mange samarbeider går det helt fint å kommitte med feil. Når du skal jobbe på større prosjekter bør du lære deg om git branching. ).

**5.1.4)** Gå gjennom `Grid` og `IGrid` og endre alle `CellState`/`ICellState` til `T`. `(add-commit-push)`

**5.1.5)** Nå skal du få feilmelding i Grid-klassen på

```java
Grid<T> implements IGrid
```

(Hvorfor det?).  Endre kodelinjen som var feil til

```java
Grid<T> implements IGrid<T>
```

Forsikre deg om at du forstår hva denne linjen betyr. Grip gjerne tak i sidemannen, sidedamen, naboen sin hund, eller gruppelederen din og forklar det til dem.

> 🤔 Hva er forskjellen på betydningen av den første `T`-en og den andre `T`-en i linjen?

**5.1.6)** Dersom en skal bruke `Grid`-klassen som et grid som holder `ICellState`, så kan vi nå skrive `Grid<ICellState>`. Det samme går for interfacet `IGrid<ICellState>`. Gå gjennom celleautomat-klassene og endre hver referanse til `CellStateGrid` til å bruke den nye generiske `Grid`-klassen. Sjekk at celleautomatene fremdeles fungerer fungerer.

Husk rød strek under betyr feil som ikke vil kompilere, mens gul strek under betyr warning. Å innføre generiske typer slik vi har gjort nå vil gi noen warnings rundt om kring i koden; disse advarslene bør du rette opp i selv om koden *kan* kjøre med dem.
`(add-commit-push)`

✅ Nå skal alt kompilere og celleautomatene skal fungere med den generiske grid-klassen.

**5.1.7)** Gå nå gjennom Labyrint-klassene (hovedsaklig `Labyrinth` og `Labyrinth-helper`) og la de bruke `Grid` og `IGrid` i stedet for `LabyrinthTileGrid`. `(add-commit-push)`

**5.1.8)** `LabyrinthTileGrid` og `ILabyrinthTileGrid` er nå overflødig. Kjenn på den gode følelsen av å slette disse filene. Rett opp i eventuelle feil og sjekk at begge labyrint-programmene fungerer.

Merk at testene for celleautomatene og labyrinth tidligere brukte `CellStateGrid` og `LabyrinthTileGrid`. Disse må nå også endres til `Grid` og `IGrid`.

(Hvis du glemte noen steder i oppgave 5.1.7 vil du få feilmeldinger når du sletter filene. Fiks disse feilmeldingene før du går videre.)
`(add-commit-push)`

✅ Du er ferdig når både celleautomatene og labyrint-spillet bruker den samme generiske Grid-klassen. Alle programmene skal fortsatt fungere og de samme testene skal passere (ikke de som omhandler Labyrinth).

## 5.2 Exceptions

_I denne oppgaven skal du bruke debugging, tester og exceptions til å rette opp i en bug._

Labyrinth-spillet lar deg styre en spiller som går rundt i en labyrinth og plukker opp gull. Dersom spilleren går inn på en rute med gull (gule ruter), skal gullet forsvinne og ruten bli grå. Labyrinten består av steinblokker (sorte ruter) som spilleren må gå rundt, og kantene på kartet.

**5.2.0)** Kjør spillet og prøv å styre spilleren. Du kjører spillet via `labyrinth.Main`.
![](img/lab1.gif)

Spillet har en bug som gjør at spilleren kan gå gjennom steiner.

Hvis spilleren prøver å gå utenfor brettet skjer det ingenting i spill-vinduet, men programmer kaster exception og _stack-trace_ til konsollen:

```text
Exception in thread "AWT-EventQueue-0" java.lang.IndexOutOfBoundsException: Row and column indices must be within bounds
    at datastructure.Grid.checkCoordinate(Grid.java:??)
    at datastructure.Grid.set(Grid.java:??)
    at labyrinth.Labyrinth.movePlayer(Labyrinth.java:??)
    at labyrinth.gui.LabyrinthGUI.keyPressed(LabyrinthGUI.java:??)
```

Gjenskap den beskrevne oppførselen på ditt eget program:

- Kjør programmet.
- Sjekk at spilleren går gjennom sorte ruter.
- Sjekk at du får exception i konsollen når du prøver å gå utenfor brettet.

Merk at **??** i feilmeldingen over vil være linjenummer som avhenger litt av hva du har gjort med koden.

**5.2.1)** Finn ut hvor i koden problemet ligger. Du kan gå til stedet som exception ble kastet fra ved å trykke på `Grid.java:??` i stack-tracet under feilmeldingen (i noen IDE'er må man holde inne command/ctrl eller noe sånt når man trykker).

Legg merke til at feilmeldingen kastes av forkravs-sjekken i `Grid-klassen`. Forkravet virker fornuftig: den sjekker om argumentene til `set` er utenfor størrelsen til gridden, og kaster exception når de er utenfor brettet. Dette er i tråd med dokumentasjonen i `IGrid`:

```text
 * The row index must be greater than or equal to 0 and less than numRows()
 * The column index must be greater than or equal to 0 and less than numColumns()
```

Spesifikasjonen til `IGrid`-interfacet inneholder altså _forkrav_ som Labyrinth-koden ikke respekterer. `set`-metoden implementerer interfacet riktig, men Labyrinth-koden bruker det feil. Vi må rette opp i argumentene vi kaller `set` med.

🤔 Selv om feilmeldingen sendte oss til `Grid`-klassen er det altså ikke her feilen _egentlig_ ligger.

For å finne ut hvor `set`-metoden er kalt fra kan du trykke på neste linje i stack-tracet: `Labyrinth.java:???`. Les over metoden du kommer til slik at du skjønner hvordan den virker. Spesielt viktig er det at du skjønner hva `@Override` betyr.

Sjekk de neste par linkene i stacktracet (movePlayer() og keyPressed()) også, og prøv å forstå hvordan koden henger sammen.

Finn enten penn og papir eller et skrive-program på pcen din og skriv ned

- hvordan `movePlayer` metoden virker,
- hva det vil si at den er `@Override`, og
- hva du syns spillet burde gjøre i stedet for å kaste exception,
- hvordan du tror du kan fikse feilen.

✅ Lagre det, eller legg det til siden, før du går videre.

![](img/lab2.gif)

**5.2.2)** Skriv forkrav til `movePlayer`-metoden. Forkrav-sjekker består typisk av en `if`-setning som sjekker egenskaper ved argument-verdiene. `(add-commit-push)`

```java
if(<argument-egenskap>){
  // <feilhåndtering>
}
// ...
// resten av metoden
```

🤔Hva tror du er fornuftige egenskaper å sjekke på argumentet? Se om du finner noen hjelpe-metoder i Labyrinth som du kan bruke her.

`playerCanGo` sjekker om et flytt er gyldig. Du kan bruke den som hjelpemetode til å sjekke argumentet ditt.

Til å starte med kan du bare returnere dersom spilleren prøver å gå til en rute den ikke får lov til. Senere vil vi skrive ut en beskjed til skjermen.

```java
if(<argument-egenskap>){
  return;
}
// ...
// resten av metoden
```

✅ Du kan gå videre når koden ikke lenger kaster exceptions og spilleren ikke går gjennom steiner, og du sjekker forkrav i `movePlayer` ved hjelp av `playerCanGo` og returnerer dersom forkravet ikke holder.

**5.2.3)** Legg inn feilhåndtering i `ILabyrinth`. `(add-commit-push)`

I metoden `LabyrinthGUI.keyPressed` så du at `movePlayer` ble kalt med den retningen spilleren skal gå i. Siden `LabyrinthGUI` har tilgang til å endre grafikken og skrive ut beskjeder, er det naturlig at feilmeldingen til brukeren av spillet håndteres her ved å for eksempel skrive ut en beskjed til skjermen.

Vi har altså rettet opp i buggen ved å sjekke _forkrav_ i `movePlayer`. Nå skal vi endre feilhåndteringen i `movePlayer`til å kaste et `MovePlayerException`. Vi skal ta imot det i `LabyrinthGUI` og skrive ut en beskjed til skjermen som forteller spilleren at den har gått feil, f.eks. `"BAD MOVE!";`

Begynn med å legge til en `@throws`-deklarasjon i `ILabyrinth`.

```java
void movePlayer(GridDirection dir) throws MovePlayerException;
```

Oppdater javadoc til metoden slik at den inneholder `@throws MovePlayerException <beskrivelse av oppførselen>`. Legg inn din egen beskrivelse.

Nå får du sikkert feilmeldinger andre steder i programmet. Disse kommer av to forskjellige grunner:

- metoder som _implementerer_ `movePlayer` burde også deklarere `@throws`, på samme måte som i interfacet.
- metoder som _kaller_ `movePlayer` må _håndtere_ exception-typen. Dette gjøres ved å legge til `try-catch` og legge inn lokal feilhåndtering i `catch`.

✅ Gå videre når du har lagt inn @throws i ILabyrinth. Vi skal rette opp i feilmeldingene i neste steg.

**5.2.4)** Legg inn `try-catch` i `LabyrinthGUI`. Du kan legge det inn på følgende måte:

```java
public void metode() {
  try {
    // ...
    // resten av metodekroppen
  } catch (<exception>) {
    // <feilhåndtering>
  }
}
```

Du må legge det inn i alle metoder som kaller `movePlayer`. `catch`-blokken skal ta imot et exception av typen `MovePlayerException` (du må kanskje importere det), og skrive ut en feilmelding til skjermen, f.eks. "BAD MOVE!". Du kan skrive ut meldinger ved å kalle `setText`-metoden på `message`-objektet. `(add-commit-push)`

✅ Gå videre når du har lagt inn én `try-catch` i hver LabyrinthGUI-metode som kaller `movePlayer` og feilhåndering.

_NB: Hvis du bruker autogenerering i denne oppgaven får du fire-fem try-catcher per metodekall, og risikerer å introdusere nye bugs. Kun én try-catch per metode._

**5.2.5)** Legg inn `@throws`i `Labyrinth.movePlayer`-metoden og oppdatering feilhåndteringen til å kaste exception. `(add-commit-push)`

```java
@Override
public void movePlayer(Direction d) throws MovePlayerException {
  // ...
}
```

Gå tilbake til forkrav-sjekken din. Hvis forkravet ikke holder skal du kaste et nytt `MovePlayerException` med en fornuftig feilmelding.

✅ Du er ferdig når `movePlayer` kaster exception hvis du prøver å gå inn i steiner eller utenfor brettet, og `LabyrinthGUI` tar imot det, og skriver en fornuftig beskjed til skjermen.

![](img/bugfixed.gif)

✅✅ For å bestå labben må alle tester passere, inkludert `GenericGridTest`. Husk å lever på Codegrade!

## 5.3 Valgfritt

### 5.3.1 Gå tilbake til notatet

Gå tilbake og se på notet du skrev i 5.2.1. Stemmer det du skrev overens med måten du løste oppgaven på? Hva var likt? Hva var annerledes?

- `@Override` vil si at `movePlayer` i Labyrinth er en implementasjon av `movePlayer` i `ILabyrinth`. Det vil si at selv om `movePlayer` kalles på en variabel som har deklarert type `ILabyrinth`, så er den _konkrete_ metoden som blir kalt under kjøring i `Labyrinth`.

- Vi kunne løst feilhånderingen på andre måter. Hvis beskrev en annen løsning enn vi valgte, tror du din ville fungert også?

### 5.3.2 Utvid labyrint-koden

Labyrinthspillet inneholder støtte for å samle gull og slåss med monster. Du kan legge inn funksjonalitet for dette hvis du vil.

### 5.3.3 Lag Spiller-AI

I stedet for å styre spilleren ved hjelp av tastetrykk kan du implementere en algoritme som styrer spilleren rundt. Start ved å lage en metode eller klasse som kan tilfeldig velge retninger å gå i; deretter kan du prøve å finne en lur måte å velge retning på. F.eks. hvis jeg har gull i en nabo-rute, gå mot det.
