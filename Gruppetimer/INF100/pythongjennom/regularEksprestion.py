"""
regular expression = "regexes" kort:
 - beskriver et mønster i tekst
eks - en \d i en regexe står for siffertegn 0-9
eks på en regexe: \d\d\d-\d\d\d-\d\d\d\d ville funnet eks 123-456-7890
du kan si hvor mange ganger regexen skal finne målet med å putte ({antall})
eks \d\d\d-\d\d\d-\d\d\d\d = \d{3}-\d{3}-\d{4}
dokumentasjon: https://docs.python.org/3/library/re.html
hvordan lage:
1. import re
2. re.compile(mønster), gir tilbake et regexe mønster som du kan bruke, eks tlfNum = re.compile(\d\d\d-\d\d\d-\d\d\d\d)
3. re.search(mønster,tekst), søker etter mønsteret du ga i steg 2 og retunerer matchen i formatet <re.Match object; span=(start, slutt), match='123-333-3333', finner den ingenting retunerer den None
4. re.search har en .group() som bare retunerer teksten den fant, 123-333-3333
"""
import re


tlfNum = re.compile("\d\d\d-\d\d\d-\d\d\d\d")
tekst = "mitt telefon number er 123-333-3333"
##1 måte å skrive på
a = re.search(tlfNum, tekst)

##2.måte
b = tlfNum.search(tekst)
##print(b.group())

##eks med group
tlfNum = re.compile("(\d\d\d)-(\d\d\d)-(\d\d\d\d)")  ##deler mønsteret inn i grupper
a = re.search(tlfNum, tekst)
print(a.group(1))
print(a.group(2))
print(a.group(3))
print(a.group(0))
print(a.groups())
sted, main, slutt = a.groups()  ##kan gi delene egne objekter
print(sted)
print(main)
print(slutt)
"""
spesielle karakterer:
skal du finne .  ^  $  *  +  ?  {  }  [  ]  \  |  (  ) må du sette \ forran siden de alene har er spesielle og ikke blir lest som en string
eks: \.  \^  \$  \*  \+  \?  \{  \}  \[  \]  \\  \|  \(  \)

| er kalt en pipe og er som or:
eks: re.compile("thomas|markus") da finner den en av de, hvis den finner begge gir den bare tilbake den første
re.compile("bat(man|mobile|girl)")//
"""
tekst1 = "jeg heter thomas og min bror heter markus"
navn = re.compile("thomas|markus")
print(navn.search(tekst1).group())

tekst2 = "batgirl mistet matmobilen sin"
bat = re.compile("bat(girl|mobile|man)")
print(bat.search(tekst2).group(1, 0))

"""
bruk av spørsmåltegn ?, eks 'Bat(wo)?man', da ser den for både batwoman og batman
bruker du * eks "bat(wo)*man" kan du gi den så mange tilfeller av wo, eks batwowowowoman vil retunere batwowowowoman
bruker du + eks bat(wo)+man må du wo+man iallefall komme en gang, batman = None, batwoman og batwowowoman vil fungere

karakterklasser:
\d = tall
\D = ikke tall
\w = bokstaver, tall. Tenk på at du bruker denne til å finne ord
\W = mellomrom osv
\s = mellomrom, tab, newline 
\S = som ikke er mellomrom, tab og newline

bruk eks \d+,\w+ for å ta hele ordet/tallet

(^) og ($) brukes for å indikere at matchen må være på begynnelsen eller slutten av teksten du gir den

startermedHello = re.compile(^Hello) - må begynne med Hello
sluttermedHello = re.compile($Hello) - må slutte med Hello

. og .* er wildcards, . kan være hva som helst unntatt et mellomrom og .* kan være sammenhengene wildcards
.at på flat vil go lat mens .*at på flat vil gi flat

det finnes en grådig og ikkegråde måte for python å gå finne på 
bruker du et spørsmålstegn blir den ikkegårdig og uten spm tegn blir den grådig. 
den grådige vil alltid ta det siste den kan for å få med seg mest mulig tekst mens den ikkegådige vil gjør motsatt

nongreedyRegex = re.compile(r'<.*?>') på <To serve man> for dinner.> = '<To serve man>'
greedyRegex = re.compile(r'<.*>') på <To serve man> for dinner.>  = '<To serve man> for dinner.>'

for å overse om det er stor eller liten bokstav kan du gi re.I som andreargument.
reg = re.compile("robot",re.I)
da blir alt fra Robot, robot, RoBOt godkjent
"""
