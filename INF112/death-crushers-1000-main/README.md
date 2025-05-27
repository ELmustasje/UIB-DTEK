# INF112 Project - Warden of Høytek

- Team: _Death Crushers 1000_ (Gruppe 5):
  - Jesper Hammer
  - Zeno Elio Leonardi
  - Thomas Barth
  - Izaak Sarnecki
  - Johanne Blikberg Herheim.
- Repository på GitLab finnes [her](https://git.app.uib.no/inf112/25v/proj/death-crushers-1000).
- Link til Trello finnes [her](https://trello.com/b/ZCyL9prn/death-crushers-1000).
- Møtereferater: [Google Doc](https://docs.google.com/document/d/1WfEtzExT4xF1IWqi6dMqeTZf7Evc8MFy4sm8Yu5cxas/edit?usp=sharing)

## Om spillet

Warden of Høytek er et rougelike kortspill hvor man kjemper seg gjennom flere fiender for å slå den siste og farligste bossen; Eduroam.

## Credits

All grafikk og lyd er enten laget selv av Death Crushers 1000 Studios (c) 2025 All rights reserved, eller generert av AI. Alt er under MIT lisens.

## Prosjektstruktur

Spillet er delt inn i ulike `State`s, og hver state håndterer sin egen data. Vi bruker en variant av MVC hvor `Canvas` oppfører seg som en slags View og tar inn alle objekter som skal tegnes, og tegner dem. Slik er grafikken adskilt fra logikken. Input er bare musepeker. `Mouse` klassen brukes i `Controller` for å håndtere dette. Hver state er en Model. De har alle objekter og data som er relatert til den delen av spillet og styrer all logikk selv.

Vi bruker også et Event system for all synkronlogikk, som animasjoner og tur-basert kamp. Dette håndteres av en `EventQueue` klasse som scheduler ulike ting til å skje etter hverandre, og med en `WaitGroup` klasse som kjører flere oppgaver samtidig.

### Mappestruktur

- `/core` - engine/spillets kjernelogikk
  - `/time` - timer, eventqueue
  - `/state` - istate og statemachine
- `/gfx` - abstraksjonslag over libGDX
- `/util` - utility som ikke er spill-spesifikt (matte, fil-lasting, lyd, debug osv)
- `/game` - selve spill-logikken

## Kjøring

- Kompileres med `mvn package`.
- Kjøres med `java -jar target/slayday-1.0-SNAPSHOT-fat.jar`
- Krever Java 21 eller senere

## Formattering

- Formatter prosjektet med `mvn spotless:apply`
- Sjekk om prosjektet er riktig formattert med `mvn spotless:check`

### Oppsett av formattering på

- VS Code:

  - Gå inn på settings
  - Søk etter java > format > settings: URL
  - Lim inn: file://${workspaceFolder}/eclipse-formatter.xml

## Testing

Vi fikk ikke til å ignorere `/gfx` (som ikke kan testes fordi det bare er grafisk) så test coverage ble mindre enn ønskelig.

