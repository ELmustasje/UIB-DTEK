[forrige](02-if.md) • [oversikt](../README.md) • [neste](05-lists.md)

## INF100 Lab3 - Loop'er
De følgende oppgavene er tatt fra lab3 INF100h22. Du finner de opprinnelige oppgavene her:
https://inf100h22.stromme.me/lab/3/


### Oppgave 1 - `multiplesOfSeven`
Implementer metoden `Lab3::multiplesOfSeven`. Metoden har et parameter `n`. Skriv kode for å skrive ut alle heltall mindre eller lik `n` som er delelig på 7.

Når metoden kalles med argument `n = 49`: `multiplesOfSeven(49)` så skal det skrives ut:
```
7
14
21
28
35
42
49
```

### Oppgave 2 - `multiplicationTable`
Implementer metoden `Lab3::multiplicationTable`. Metoden har et parameter `n`. Skriv kode for å skrive ut gangetabellen for alle tall fra og med `1` til og med `n`. Utskriften skal følge møsnteret vist under.

Eksempelkjøringer:
`multiplicationTable(3)`
```
1: 1 2 3
2: 2 4 6
3: 3 6 9
```
`multiplicationTable(5)`
```
1: 1 2 3 4 5
2: 2 4 6 8 10
3: 3 6 9 12 15
4: 4 8 12 16 20
5: 5 10 15 20 25
```

### Oppgave 3 - `crossSum`
Implementer metoden `Lab3::crossSum`. Metoden har et parameter `n` og skal returnere tverrsummen av `n`. 
Tverrsummen er summen av sifferene i tallet, for eksempel er tverrsummen av 12 lik 3, siden 1 + 2 = 3.

Koden skal skrive ut de følgende verdiene gitt de forskjellige argumentene:
```java
int sum = crossSum(1);
System.out.println(sum); // 1

int sum = crossSum(12);
System.out.println(sum); // 3

int sum = crossSum(123);
System.out.println(sum); // 6

int sum = crossSum(1234);
System.out.println(sum); // 10

int sum = crossSum(4321);
System.out.println(sum); // 10
```


✅ Kjør `Lab3Test.java` for å teste om du har klart alle oppgavene i denne seksjonen.