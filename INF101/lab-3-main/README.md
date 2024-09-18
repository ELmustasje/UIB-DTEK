# Shell lab

I denne lab'en skal vi utforske *grensesnitt*, og fortsette med å benytte oss av klasser og objekter.


Samtidig skal vi benytte anledningen til å bli bedre kjent med terminalen, som er et svært viktig verktøy for alle som jobber teknisk med datamaskiner. Både utviklere, system-adminstratorer og vanlige brukere som ønsker å benytte spesialisert software andre har laget vil ha stor glede av den.

I denne lab'en skal vi modifisere et enkelt shell for å navigere filsystemet som er løst inspirert av *bash*. Kommandoene vi bruker vil også fungere omtrent på samme måte i zsh og PowerShell.

* [Anbefalte forberedelser](#anbefalte-forberedelser)
* [Ordbok: shell og terminal](#ordbok--shell-og-terminal)
* [Bli kjent med eksisterende kildekode](#bli-kjent-med-eksisterende-kildekode)
* [Kjøre SimpleShell](#kjøre-simpleshell)
  * [Vis SimpleShell gjennom GUI](#vis-simpleshell-gjennom-gui)
* [Et grensesnitt for kommandoer](#et-grensesnitt-for-kommandoer)
  * [Opprette grensesnittet Command](#opprette-grensesnittet-command)
  * [Definere metoder i Command](#definere-metoder-i-command)
  * [Vår første kommando: echo](#vår-første-kommando--echo)
* [Installere kommandoer i SimpleShell](#installere-kommandoer-i-simpleshell)
* [Anbefalte forbedringer](#anbefalte-forbedringer)
  * [exit: en annen enkel kommando](#exit--en-annen-enkel-kommando)
  * [pwd, cd og ls: kommandoer som trenger kontekst](#pwd--cd--og-ls--kommandoer-som-trenger-kontekst)
  * [man: kommando for å lese manualen](#man--kommando-for-å-lese-manualen)
  * [Trykk backspace for å fjerne bakerste bokstav i kommandoen](#trykk-backspace-for-å-fjerne-bakerste-bokstav-i-kommandoen)
  * [Starte Java-programmer fra SimpleShell](#starte-java-programmer-fra-simpleshell)
* [Flere frivillige forbedringer](#flere-frivillige-forbedringer)
  * [Forbedret ls: ls -l, ls -a, ls path/to/folder](#forbedret-ls--ls--l--ls--a--ls-pathtofolder)
  * [Flere kommandoer som trenger kontekst: touch, mkdir, cat, rm, mv, cp](#flere-kommandoer-som-trenger-kontekst--touch-mkdir-cat-rm-mv-cp)
  * [grep: søking i filer](#fgrep--søking-i-filer)

## Anbefalte forberedelser

Les og forstå kursnotater som er spesielt relevante for denne lab'en:
* Kursnotater om [klasser og objekter](https://inf101.ii.uib.no/notat/objekter/) utgjør grunnlaget vi bygger videre på.
* Kursnotatene om [grensesnitt](https://inf101.ii.uib.no/notat/grensesnitt/) frem til og med avsnittet om polymorfisme er særlig relevante for denne lab'en.
* En skumlesning av avsnittet om [innfødte array'er](https://inf101.ii.uib.no/notat/array/#innf%C3%B8dt-array) i notatene om array og lister kan være greit også.

For de frivillige oppgavene på slutten er også følgende relevant:
* Avsnittet om [Filer](https://inf101.ii.uib.no/notat/inputoutput/#filer) i notatene om input og output.

## Ordbok: shell og terminal

Kjært barn har mange navn. Under lister vi opp noen synonymer til «terminal» med litt ulike opphav, og som har litt ulike konnotasjoner. I praksis bruker vi disse ordene litt om hverandre.
* **Terminal**. Opprinnelig brukt om kombinasjonen av en fysisk skjerm og et tastatur. Disse kunne være et annet sted enn selve datamaskinen. I senere tid har begrepet blitt brukt om programmer som gir et grafisk brukergrensesnitt (et vindu) til et shell. En **konsoll** var en spesiell terminal som var en integrert del av datamaskinen, og som ikke kunne skilles fra den slik man kunne med andre terminaler.
* **Shell**. Programmet som vanligvis vises i en terminal. Dette er et rent software-konsept. Et shell *tolker* kommandoer og kan starte andre prosesser. Opprinnelig var dette det dataprogrammet som startet først når oppstarten var ferdig og datamaskinen var klar til bruk. Det ble kalt et shell fordi det «omgav» kjernen i operativsystemet som et skall, og ble for brukerne det eneste de trengte å forholde seg til ved bruk av maskinen. Ordet kommer naturligvis fra tiden før vinduer og mus var vanlig. Det finnes mange ulike shell: for eksempel er **bash** og **zsh** i dag vanlige shell for Linux/Mac mens **cmd** og **PowerShell** er vanlige shell i Windows.
* **CLI** (kommandolinjen). Et CLI (*command line interface*) er et program med et tekstbasert grensesnitt, for eksempel et shell. Men en kommandolinje er ikke nødvendigvis knyttet til å navigere et operativsystem og starte andre programmer. For eksempel kan vi lage et tekstbasert spill; dette spillet vil da ha en kommandolinje selv om vi ikke ville kalt det for et shell.

## Bli kjent med eksisterende kildekode

- [ ] Kjør *main*-metoden i `Main` ([link](./src/main/java/no/uib/inf101/terminal/Main.java)) og se at du får opp et vindu du kan skrive i. Sjekk at det virker å skrive, og at du får en ny linje når du trykker enter.
- [ ] I *main*-metoden, bytt ut *DummyShell* med *EchoShell*. Sjekk at når du nå kjører programmet, vil du få et ekko av hver linje du skriver.

Dette du nå har bevitnet kalles *polymorfisme*, og innebærer at to objekter med forskjellig *klasse* begge har samme *type*, og at det finnes kode som fungerer bra med objekter fra begge klassene selv om metodene deres er implementert ulikt. Dette gir oss en høy grad av *modularitet*, siden vi enkelt kan bytte ut objekter i én klasse med objekter i en annen klasse med en litt annen oppførsel.

- [ ] Besvar spørsmålene i `TextAnswers` ([link](./src/main/java/no/uib/inf101/terminal/TextAnswers.java)).


✅ Du er klar til å gå videre når testene i `TestTextAnswers` ([link](./src/test/java/no/uib/inf101/terminal/TestTextAnswers.java)) passerer

## Kjøre SimpleShell

I resten av oppgaven skal vi modifisere klassen [SimpleShell](./src/main/java/no/uib/inf101/terminal/SimpleShell.java) og senere opprette noen andre klasser og grensesnitt som vi skal bruke for å forbedre SimpleShell.

- [ ] Kjør testene i `TestSimpleShellStarter` ([link](./src/test/java/no/uib/inf101/terminal/TestSimpleShellStarter.java)) og se at de allerede passerer.

> **Målet med laben.** Før vi gjør noen endringer i det hele tatt, støtter SimpleShell tre kommandoer: pwd, ls og cd. I løpet av laben skal vi oppdatere SimpleShell slik at
> * det virker sammen med GUI'en vår «Terminal», og
> * shellet virker med ubegrenset mange kommandoer.
> 
> Når laben er gjennomført skal vi kunne installere så mange kommandoer vi vil i SimpleShell uten at vi trenger å endre ytterligere på kildekoden i SimpleShell-klassen utover det vi gjorde i løpet av denne lab'en.

### Vis SimpleShell gjennom GUI
Vi ønsker at det grafiske brukergrensesnittet vårt [Terminal](./src/main/java/no/uib/inf101/terminal/Terminal.java) skal virke sammen med SimpleShell. Konsturktøren til Terminal-klassen aksepterer alle shell og objekter som implementerer *CommandLineInterface*.

- [ ] La `SimpleShell` implementere grensesnittet `CommandLineInterface`.
- [ ] Opprett de metodene som må implementeres for å oppfylle kontrakten *CommandLineInterface* beskriver. Du kan enten
  - gi nytt navn til metodene *aKeyIsPressed* og *whatTheScreenLooksLike* slik at de stemmer overens med metodenavnene som er angitt i CommandLineInterface. Når du endrer navnet, bruk din IDE sin «refactor» -funksjonalitet: høyreklikk på metodenavnet og velg *rename symbol* (VS Code) eller *refactor -> rename* (Eclipse/Intellij) eller tilsvarende. Hvis du glemmer dette, må du endre navnet manuelt i alle testene i TestSimpleShellStarter som bruker disse metodenavnene. Alternativt kan du:
  - opprette metodene som kreves av CommandLineInterface og la dem enkelt og greit kalle på *aKeyIsPressed* og *whatTheScreenLooksLike*.
- [ ] Husk i *SimpleShell* å bruke `@Override` foran de metodene som opprinnelig er definert i et grensesnitt.
- [ ] I `Main::main`-metoden: Bytt ut *EchoShell/DummyShell* med *SimpleShell*. Kjør programmet og se at kommandoene `pwd`, `cd` og `ls` virker. Prøv også noe helt annet og se at det kommer en feilmelding uten at java-programmet krasjer.

> Våre kommandoer er forenklede versjoner av de samme kommandoene i bash.
>  * `pwd` (print working directory) skriver ut hvilken mappe i filsystemet som er **cwd** (current working directory), altså den mappen man «er» i.
>  * `ls` skriver ut alle filene og mappene som befinner seg i cwd.
>  * `cd foo` endrer cwd til å bli mappen «foo». Mappen «foo» må ligge i cwd.
>  * `cd ..` endrer cwd til å bli mappen over seg i mappehierarkiet.
>  * `cd` endrer cwd til å bli «home» -mappen. Det er ikke så viktig hvilken mappe dette er, men det er den samme hver gang (hvis man ikke manipulerer hvilken mappe dette er (slik vi må gjøre i testene), vil dette være cwd for programmet som startet java-prosessen).

✅ Du er klar til å gå videre når testene i `TestSimpleShell` ([link](./src/test/java/no/uib/inf101/terminal/TestSimpleShell.java)) passerer.

## Et grensesnitt for kommandoer

- [ ] Les over kildekoden for metoden `executeCommand` i `SimpleShell`. Det er i denne metoden at vi bestemmer at ulike ting skal skje basert på hvilken kommando som blir gitt (se sekvensen med if og else if).

Vi ønsker å endre executeCommand slik at vi ikke bruker en lang sekvens med if og else if, men i stedet slår opp på `commandName` i et oppslagsverk og finner hvilken kommando som skal utføres der. Vi ønsker altså at koden skal se omtrent slik ut i stedet:

```java
Command command = this.allCommands.get(commandName);
if (command != null) {
  String result = command.run(args);
  return result;
} else {
  return "Command not found: \"" + commandName + "\"";
}
```
Som pseudokode kan koden over tolkes slik:
* Basert på verdien av `commandName`, finn ut hvilken kommando som skal utføres
* Dersom vi fant en gyldig kommando, utfør den og returner resultatet.

Du trenger ikke å ender denne koden helt enda, men dette illustrer hvor vi er på vei. For å komme oss hit, mangler vi en del ting. For eksempel finnes det ikke en type som heter `Command`. La oss definere den nå:

### Opprette grensesnittet `Command`

- [ ] Opprett et nytt grensesnitt som heter `Command` i pakken *no.uib.inf101.terminal*.
  * VS Code: høyreklikk på «terminal» der det står no/uib/inf101/terminal i filutforskeren. ![klikk her i VSCode](./img/vscode-terminalfolder.png) Velg så *new file* og skriv *Command.java*. I den nye filen kan du opprette grensesnittet, og skrive inn koden som vises under.
  * Eclipse: høyreklikk «no.uib.inf101.terminal» i prosjekt-navigatoren, og velg *new -> Interface*. I boksen som kommer opp, skriv *Command*. Koden som vises under skal nå ha blitt generert automatisk for deg.
  * Intellij: høyreklikk «no.uib.inf101.terminal» i prosjekt-navigatoren, og velg *new -> Java class*. I boksen som kommer opp, skriv *Command* og velg *Interface* i menyen under. Koden som vises under skal nå ha blitt generert automatisk for deg.

Filen skal se nå se slik ut:

```java
package no.uib.inf101.terminal;

public interface Command {
  
}
```

### Definere metoder i `Command`

Planen er at vi skal kunne kalle på  `run` på *Command* -objekter. Nærmere bestemt ønsker vi at følgende skal være mulig hvis *command* er en variabel som har typen *Command*:
```java
String[] args = new String[] { "foo", "bar" };
String result = command.run(args);
```
For at dette skal være lov, må grensesnittet *Command* definere en metodesignatur `String run(String[])`. La oss gjøre det nå. 
- [ ] Legg til følgende metodesignatur i `Command`:

```java
String run(String[] args);
```

I tillegg trenger vi å vite hva navnet til kommandoen er.
- [ ] Legg til følgende metodesignatur i `Command`:

```java
String getName();
```

### Vår første kommando: echo

Echo er en enkel kommando. Hvis man skriver kommandoen `echo foo bar`, så blir resultatet `foo bar `. Denne kommandoen vil du også finne i bash og lignende shell, og det eneste den gjør er å spytte tilbake argumentene den fikk som input.

* [ ] Opprett en klasse `CmdEcho` i pakken *no.uib.inf101.terminal*
* [ ] La `CmdEcho` implementere grensesnittet `Command`
* [ ] La `CmdEcho` implementere metodene som er definert i *Command* -gresesnittet. Husk å annotere hver av dem med `@Override`.
* [ ] La `getName()` alltid returnere `"echo"`.
* [ ] La `run(String[])` returnere en streng hvor hvert av argumentene i String[] -arrayen er limt sammen med mellomrom. Gå gjennom args med en foreach-løkke, og legg til et mellomrom etter hver av strengene. For eksempel, hvis input er `new String[] {"foo", "bar"}`, skal returverdien være `"foo bar "` (legg merke til ekstra mellomrom etter *bar*).
* [ ] I filen `TestCmdEcho` ([link](./src/test/java/no/uib/inf101/terminal/TestCmdEcho.java)), fjern kommentarene slik at testene blir aktivert.

✅ Du kan gå videre når testene i `TestCmdEcho` er aktivert og passerer.

## Installere kommandoer i SimpleShell

Vi skal nå installere kommandoer i SimpleShell. Til dette trenger vi å lagre forskjellige Command-objekter. Dette gjør vi i et oppslagsverk

- [ ] Opprett en instansvariabel `allCommands` i SimpleShell:

```java
private final HashMap<String, Command> allCommands = new HashMap<>();
```
Koden over oppretter en variabel som
* heter `allCommands`, og
* har typen `HashMap<String, Command>`. Det betyr at typen er et oppslagsverk hvor nøklene er av typen `String`, og verdiene er av typen `Command`.

I tillegg opprettes det et nytt objekt i klassen `HashMap` som ved opprettelse er et tomt oppslagsverk uten noen nøkler eller verdier enda. Det neste vi skal gjøre er å gjøre det mulig å legge til kommandoer i oppslagsverket.

- [ ] Opprett en metode i SimpleShell med signatur `public void installCommand(Command command)`. La metoden hente ut navnet fra Command -objektet og opprett et nytt nøkkel-verdi -par i *allCommands* med navnet som nøkkel og Command-objektet som verdi. For eksempel:

```java
this.allCommands.put(command.getName(), command);
```

- [ ] I `Main::main` -metoden, kall `installCommand` på SimpleShell -objektet med et nytt `CmdEcho` -objekt som argument. Merk at du må endre typen til shell-variabelen slik at den har typen SimpleShell, ellers vil du ikke ha tilgang til installCommand -metoden.

```java
SimpleShell shell = new SimpleShell();
shell.installCommand(new CmdEcho());

Terminal gui = new Terminal(shell);
gui.run();
```


Vi har nå installert kommandoen *echo* i shellet vi bruker i Main-metoden. Det eneste som gjenstår før vi kan bruke echo-kommandoen er at `executeCommand` faktisk slår opp i oppslagsverket og kjører kommandoen.

- [ ] Modifiser `executeCommand` slik at den slår opp på kommandoen og kaller run dersom den fant en kommando med det gitte kommandonavnet. Siden vi ikke har laget og installert kommandoer for pwd, cd og ls enda lar vi den gamle implementasjonen stå for disse kommandoene.
```java
Command command = this.allCommands.get(commandName);
if (command != null) {
  return command.run(args);
} else if (/* ... */) {
  // ...
} else {
  return "Command not found: \"" + commandName + "\"";
}
```

- [ ] Kjør `Main` og sjekk at echo-kommandoen virker i praksis.
- [ ] Fjern kommenteringen fra `TestSimpleShellEcho` ([link](./src/test/java/no/uib/inf101/terminal/TestSimpleShellEcho.java)) slik at testen blir aktiv.



✅ Du er ferdig med den obligatoriske delen av laben når testen i `TestSimpleShellEcho` er aktivert og passerer (i tillegg til at testene i `TestCmdEcho`, `TestSimpleShell` og `TestTextAnswers` fremdeles passerer).


## Anbefalte forbedringer

Forbedringene i dette avsnittet er frivillige med tanke på å bestå laben, men er sterkt anbefalt å prøve seg på.

### `exit`: en annen enkel kommando

- [ ] Opprett en ny klasse `CmdExit` etter samme mønster som `CmdEcho`, som også implementerer *Command* -grensesnittet.
- [ ] I run-metoden, gjør et kall til `System.exit(0);`. Denne metoden kan returnere for eksempel null.
- [ ] Installer kommandoen i `Main::main` på samme måte som tidligere. 
- [ ] Sjekk at det fungerer å lukke programmet ved å gi kommandoen `exit`.

### `pwd`, `cd`, og `ls`: kommandoer som trenger kontekst

Vi skal opprette egne klasser for hver av kommandoene `pwd`, `cd` og `ls`. Dette er SimpleShell sine tre kjerne-kommandoer som alltid må være tilgengelig, og som allerede var implementert; men vi ønsker å benytte oss av samme struktur som de andre kommandoene (jo færre ting som håndteres på en spesiell måte, jo bedre).

Det som skiller disse kommandoene fra echo og exit, er at de har behov for å vite *konteksten* shellet kjører i. Vi løser dette ved at SimpleShell skal gi kommandoene som ønsker det tilgang til Context-objektet når de blir installert.

 - [ ] I `Command`: legg til en metode-signatur for å sette kontekst.
```java
default void setContext(Context context) { /* do nothing */ };
```
Denne linjen oppretter en metode i grensesnittet med en *standard* (default) implementasjon (som altså her er å ikke gjøre noen ting som helst dersom metoden blir kalt). Dette innebærer imidlertid at vi nå kan kalle på `setContext` dersom vi har å gjøre med et `Command`-objekt.

- [ ] I `SimpleShell::installCommand`, gjør et kall til *setContext*:

```java
command.setContext(this.context);
```

Vi er nå klare til å opprette klassene `CmdLs`, `CmdPwd` og `CmdCd` som alle implementerer *Command* -grensesnittet.
- [ ] La disse klassene ha en instansvariabel for kontekst, og la dem overskrive `setContext` -metoden slik at denne instansvariabelen bli initiert med en verdi når metoden kalles.
- [ ] Kopier funksjonaliteten fra de tilsvarende metodene i SimpleShell inn i run-metodene i de nye klassene.
- [ ] Fjern metodene fra SimpleShell (inkludert sekvensen med else-if i executeCommand), og installer i stedet kommandoene i *konstruktøren til SimpleShell*. Dette gjøres på samme måte som vi gjorde for echo -kommandoen i Main, men objektet vi kaller installCommand på heter *this* i stedet for *shell*: for eksempel `this.installCommand(new CmdLs())`.
- [ ] Sjekk at testene fremdeles passerer og at de fremdeles virker i terminal-gui'en


### `man`: kommando for å lese manualen

* `man command` gir oss manualen til kommandoen `command`.

- [ ] La `Command` -grensesnittet ha en metode `String getManual()` som returnerer instruksjoner for hvordan kommandoen brukes. Du må nå oppdatere alle klassene som implementerer Command slik at de implementerer metoden og returnerer en hensiktsmessig hjelpetekst.
- [ ] La `Command` -grensesnittet ha en default-metode `default void setCommandContext(Map<String, Command>)` med en standard-implementasjon som ikke gjør noen ting.
- [ ] I `SimpleShell::installCommand`, gjør et kall til setCommandContext med this.allCommands som argument.
- [ ] I `CmdMan`, la det være en instansvariabel av typen `Map<String, Command>`, og initer denne i `setCommandContext`.
- [ ] I `run` -metoden til `CmdMan`, hent ut riktig Command-objekt fra nevnte instansvariabel og returner resultatet av getManual() kalt på dette objektet.

### Trykk backspace for å fjerne bakerste bokstav i kommandoen

Undersøk metoden `SimpleShell::keyPressed` og se hvordan ulike taster håndteres. Legg til funskjonalitet slik at når backspace trykkes fjernes bakerste bokstav i kommandoen.

> Hint: sjekk hvilken verdi som kommer når du trykker på backspace ved å legge til
`System.out.println("Keycode pressed: '" + ((int) key) + "'");` i begynnelsen av keyPressed-metoden. Start programmet, trykk backspace og se hva som skrives ut. Dette vil være et tall, siden vi *castet* key-variabelen til en `int`.
>
> Når du senere ønsker å legge til en betingelse i programflyten, kan du også sammenligne castet verdi, slik: `((int) key) == /* tallet her */`

### Starte java-programmer fra SimpleShell

Det neste steget for å få et fullt funksjonelt shell er muligheten til å starte og avslutte andre programmer, kontrollere hva disse programmene får som input, og la dem gi oss output i sanntid. Hva som skal til for å klare alle disse oppgavene går utenfor pensum i INF101, og er ikke støttet av Terminal-klassen vår.

Likevel kan vi gi en liten smakebit, nemlig hvordan vi kan kjøre andre *java* -programmer med den begrensning at de ikke er basert på interaktiv input i konsollen.

#### Hello World

- [ ] Opprett en klasse `HelloWorld` som skriver ut `"Hello World!"` til skjermen. Denne skal du kjenne igjen fra tidligere:

```java
package no.uib.inf101.terminal;

public class HelloWorld {
  public static void main(String[] args) {
    System.out.println("Hello World!");
  }
}
```

- [ ] Opprett en klasse `CmdJavaProgram` og kopier inn koden under (avhengig av hvor mange metoder du har i `Command` -grensesnittet kan det være du må legge til en metode). Du trenger ikke å forstå alt koden gjør.

```java
package no.uib.inf101.terminal;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;

public class CmdJavaProgram implements Command {

  private final Class<?> mainClass;
  private final String commandName;

  /**
   * Create a command from a Java program. Programs that read from
   * standard input are not supported, but printing to standard
   * output (i.e. using System.out to print) is ok.
   *
   * @param commandName  The name of the command
   * @param mainClass  The class containing a main(String[]) method
   */
  public CmdJavaProgram(String commandName, Class<?> mainClass) {
    this.mainClass = mainClass;
    this.commandName = commandName;
  }
  
  @Override
  public String getName() {
    return this.commandName;
  }

  @Override
  public String run(String[] args) {
    // Remember original System.out and System.err
    final PrintStream orgOut = System.out;
    final PrintStream orgErr = System.err;

    try {
      // Prepare for capturing the output of the program
      ByteArrayOutputStream out = new ByteArrayOutputStream();
      PrintStream newOut = new PrintStream(out, true, StandardCharsets.UTF_8);
      System.setOut(newOut);
      System.setErr(newOut);

      // Invoke  main method of the class for which this command was created
      Method main = this.mainClass.getMethod("main", String[].class);
      main.invoke(null, new Object[] {args});

      // Convert the captured output to a string and return it
      return out.toString(StandardCharsets.UTF_8);
    } catch (NoSuchMethodException e) {
      return "No main method found in class " + this.mainClass.getName();
    } catch (Exception e) {
      return e.toString();
    } finally {
      // Restore the original System.out and System.err
      System.setOut(orgOut);
      System.setErr(orgErr);
    }
  }
}
```

Så gjenstår det å installere HelloWorld som en kommando `"hello"`.

- [ ] I `Main::main`, installer HelloWorld som en kommando:

```java
shell.installCommand(new CmdJavaProgram("hello", HelloWorld.class));
```

✅ Sjekk at du kan kjøre programmet, og at kommandoen `"hello"` skriver ut `Hello World!` i GUI'en.

PS: Prøv også:

```java
shell.installCommand(new CmdJavaProgram("new", Main.class));
```

Hva er det som skjer?


## Flere frivillige forbedringer

Forbedringene under krever at du setter deg inn i `File` -klassen fra Java sitt standardbibliotek, og bruker `Context` -klassen som følger med dette prosjektet aktivt. Du kan lese litt om `File` i [kursnotatene](https://inf101.ii.uib.no/notat/inputoutput/#filer) eller slå opp i den [offisielle dokumentasjonen](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/File.html).

### Forbedret `ls`: `ls -l`, `ls -a`, `ls path/to/folder`

I bash har kommandoen `ls` flere muligheter enn det vi så langt har laget. For eksempel:
* elementene vises i sortert rekkefølge
* `ls -l` vil vise elementene med linjeskift `'\n'` mellom seg i stedet for mellomrom `' '`
* `ls -a` vil vise skjulte filer, mens vanlig `ls` kun viser filer som ikke er skjulte.
* `ls -la` er en kombinasjon av de to over
* `ls path/to/folder` vil vise filene i mappen `cwd/path/to/folder`.

Det kan være en fin øvelse å gjenskape dette for vår `ls` -kommando.

### Flere kommandoer som trenger kontekst: touch, mkdir, cat, rm, mv, cp

* `touch filename` oppretter en ny tom fil med det gitte navnet (hvis den ikke finnes fra før)
* `mkdir dirname` oppretter en ny mappe med det gitte navnet (hvis den ikke finnes fra før)
* `cat filename` returnerer tekst-innholdet i den gitte filen som en streng
* `rm filname` sletter en fil eller en tom mappe (forsiktig når du tester med denne!)
* `rm -r foldername` sletter en mappe og alt innholdet i den (veldig forsiktig når du tester med denne!)
* `mv source target` endrer navn på filen eller mappen *source* til filnavnet/mappenavnet *target*. Dette kan innebære flytting hvis target er en sti til en annen mappe. Dersom *target* er en mappe som finnes fra før, flyttes *source* -filen/mappen til *target* -mappen og beholder sitt opprinnelige navn.
* `cp source target` det samme som mv, men lager en kopi i stedet for å flytte.

### `fgrep`: søking i filer

*Grep* og lillebroren *fgrep* er på mange måter *cat* med et søkefilter inkludert. Den skriver kun ut de linjene som har innhold som matcher søkeordet. Standard bruk:

`fgrep searchword filename`

Vi anbefaler alle å bli kjent med hvordan grep/fgrep virker i bash. De ambisiøse kan også implementere det de klarer av fgrep til bruk i vårt eget SimpleShell:

* `fgrep searchword filename -n` viser linjenummer
* `fgrep searchword filename -i` ignorerer case, dvs søkeordet `abc` vil få treff på `aBC`
* `fgrep searchword filename -v` skriver ut alle linjene som *ikke* matcher søkeordet
* `fgrep searchword filename -c` skriver ut hvor mange linjer som matchet søkeordet
* `fgrep searchword *` kjører grep på alle filer i current working directory. Hvis kombinert med `-n` viser utskriften navnet på filen ut først, og så linjenummer i den filen.
* `fgrep searchword * -l` skriver kun ut navnet på filene med innhold som matcher, uten å skrive ut selve treffene
* `fgrep searchword * -L` skriver kun ut navnet på filene *uten* innhold som matcher
* `fgrep searchword foldername -r` kjører grep på alle filer i mappen *og* på alle filer i alle undermapper i uendelig dypde.

Flagg (delen av kommandoene som begynner med `-`) kan ofte kombineres, for eksempel vil `-ni` både vise linjenummer og ignorere case. Du trenger ikke håndtere søkeord som inneholder mellomrom i første omgang, det blir eventuelt en superbonusoppgave.

Prøv deg frem og sammenlign med hva som skjer i bash. Les mer om grep i introduksjonen til [ryanstutorials](https://ryanstutorials.net/linuxtutorial/grep.php), [opensourceforu](https://www.opensourceforu.com/2012/06/beginners-guide-gnu-grep-basics/) eller [danielmiessler](https://danielmiessler.com/study/grep/).
