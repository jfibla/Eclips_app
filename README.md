# 🌘 Eclips’app

Aplicació Shiny per planificar i visualitzar l’observació de l’eclipsi solar del **12 d’agost de 2026** des de Catalunya.

---

## 🌐 Accés a l’aplicació

Pots utilitzar l’app directament des del navegador:

👉 https://genometools.shinyapps.io/Eclips_app/

---

## 🎯 Objectiu

Eclips’app permet als usuaris:
- Localitzar la posició exacta del Sol durant l’eclipsi
- Conèixer l’orientació (azimut) i l’alçada del Sol
- Validar si una ubicació o fotografia és adequada per a l’observació
- Estimar l’hora local de màxima ocultació

---

## 🧭 Funcionalitats principals

- 📍 Selecció de posició geogràfica (manual o amb clic al mapa)
- 🧮 Càlcul d’azimut i elevació del Sol
- 🖼️ Visualització de la trajectòria solar sobre fotografies
- ⏱️ Introducció de l’hora local d’observació
- 📌 Validació de punts d’observació
- 🧭 Guia orientativa per observar l’eclipsi a Catalunya

---

## 🕒 Hora de màxima ocultació

A Catalunya, l’eclipsi tindrà lloc aproximadament entre:

**20:15:00 i 20:30:00**

Per consultar punts d’observació i horaris detallats:
👉 https://eclipsicatalunya.cat/punts-d-observacio/

---

## 🚀 Execució local

Per executar l’app en local:

```r
shiny::runApp()
```

---

## 🗂️ Estructura del projecte

```
.
├── app.R
├── R/
├── www/
└── README.md
```

---

## ⚠️ Notes

- L’app està pensada per a ús orientatiu i educatiu.
- La precisió depèn de la ubicació i dades introduïdes per l’usuari.
- Es recomana validar sempre sobre el terreny.

---

## ☀️ Recomanacions d’observació

- Utilitza **ulleres homologades per eclipsi solar**
- No miris el Sol directament sense protecció
- Planifica la ubicació amb antelació
- Tingues en compte obstacles com muntanyes o edificis

---

## 👤 Autor

Desenvolupat per **Joan Fibla**

---

## 📜 Llicència MIT

Aquest projecte està sota la llicència MIT.

Copyright (c) 2026 Joan Fibla

Es concedeix permís, gratuïtament, a qualsevol persona que obtingui una còpia d'aquest programari i dels fitxers de documentació associats (el "Programari"), per utilitzar el Programari sense restriccions, incloent-hi, sense limitació, els drets d'ús, còpia, modificació, fusió, publicació, distribució, sublicència i/o venda de còpies del Programari, i permetre a les persones a qui es proporcioni el Programari fer-ho, subjecte a les següents condicions:

El copyright anterior i aquest avís de permís s'han d'incloure en totes les còpies o parts substancials del Programari.

EL PROGRAMARI ES PROPORCIONA "TAL QUAL", SENSE GARANTIA DE CAP TIPUS, EXPRESSA O IMPLÍCITA, INCLOENT-HI, PERÒ SENSE LIMITAR-S'HI, LES GARANTIES DE COMERCIALITZACIÓ, ADEQUACIÓ A UN PROPÒSIT PARTICULAR I NO INFRACCIÓ. EN CAP CAS ELS AUTORS O ELS TITULARS DEL COPYRIGHT SERAN RESPONSABLES DE CAP RECLAMACIÓ, DANYS O ALTRES RESPONSABILITATS, JA SIGUI EN UNA ACCIÓ DE CONTRACTE, RESPONSABILITAT CIVIL O ALTRE, DERIVADA DE, FORA DE O EN RELACIÓ AMB EL PROGRAMARI O L'ÚS O ALTRES TRACTES AMB EL PROGRAMARI.
