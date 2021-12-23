# FAS1002_projet-final

Ce répertoire présente un rapport fonctionnel, conçu dans le cadre du cours [FAS1002 - Initiation à la programmation en sciences sociales](https://admission.umontreal.ca/cours-et-horaires/cours/fas-1002/) qui
est offert dans le cadre du [Microprogramme de 1er cycle en analyse des mégadonnées en sciences humaines et sociales](https://admission.umontreal.ca/programmes/microprogramme-de-1er-cycle-en-analyse-des-megadonnees-en-sciences-humaines-et-sociales/structure-du-programme/).


## L'analyse

Afin de répondre à notre question de recherche portant sur les inégalités en matière de vaccination, ce répertoire contient les fichiers permettant d'accéder au site web dédié, mais aussi de retracer l'ensemble de ma démarche. 

L'ensemble des démarches et des analyses effectuées pour établir des groupes de 
pays comparables selon leur profil de vaccination sont présentées dans les 
différentes sections de ce site. 

* Importation : Cet onglet présente l'entierté de la démarche et des manipulation qui sont associées au « nettoyage » de nos données.

*  Description I - Analyses univariées : Cette section décrit en détail et de façon univariée les banques de données (disponibles dans 'data')  que nous manipulons de le cadre de nos analyses. 

*  Description II et analyses bivariées et multivariées : Cette section fournit la question de recherche exploratoire, nos hypothèses et conduit les analyses statistiques. 




*    Cette section présente les données 
utilisées dans ce rapport ainsi que les manipulations qui ont été nécessaires 
pour préparer les données aux analyses. 
- **Exploration des données**. Cette section présente les données contenues dans 
le jeu de données. Elle permet notamment de prendre connaissance des statistiques 
relatives aux différentes variables d’intérêt et de visualiser leur distribution.
- **Établir des groupes de pays comparables** (ou analyses). Cette section 
présente les analyses réalisées pour établir des groupes de pays comparables 
selon leur profil de vaccination.
- **Visualisation des données**. Cette section présente quelques visualisations 
des données à partir des groupes de pays comparables établis. 


#####################################################################

Packages à installer : 
* require(knitr)
* require(tidyverse)
* require(readxl)
* require(lubridate)
* require(kableExtra)
* require(skimr) 
* require(moderndive)
* require(modelsummary)
* require(DT)
* require(dagitty)
* require(stats)
