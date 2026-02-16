# 🏕️ DJH-Karte – Jugendherbergen & Umgebung

Diese kleine Web-App zeigt alle Jugendherbergen auf einer interaktiven Karte.
Zu jeder Herberge können zusätzlich interessante Orte in der Umgebung angezeigt werden – zum Beispiel Museen, Naturziele oder Spielplätze.

Die Karte funktioniert im Browser – auch auf dem Handy.
Es ist **keine App-Installation nötig**.

---

## 🌍 Was kann die Karte?

* 🔵 **Jugendherbergen als Marker** (automatisch gruppiert, wenn viele nah beieinander liegen)
* 📍 Klick auf eine Jugendherberge lädt die passenden **POIs (Points of Interest)** in der Umgebung
* 🎨 POIs sind farbig nach Kategorie sortiert (z. B. Natur, Kultur, Kinder & Jugend)
* 🗺️ Umschalten zwischen **Straßenkarte** und **Satellitenbild**
* 🏠 „Home“-Button bringt dich jederzeit zurück zur Gesamtübersicht
* 📖 „How-to“-Button erklärt die Bedienung

---

## 📱 Für wen ist das gedacht?

* Eltern, die Ausflüge planen
* Schulklassen oder Gruppenleitungen
* Alle, die wissen möchten, was rund um eine Jugendherberge interessant ist

---

## 🚀 So benutzt man die Karte

1. Karte öffnen (Browser auf dem Handy oder Computer)
2. In die Region zoomen
3. Auf eine Jugendherberge klicken
4. Die Umgebungspunkte werden automatisch geladen
5. Auf einzelne Punkte klicken → weitere Infos erscheinen

---

## 🧭 Navigationstipps

* Mit zwei Fingern zoomen (Handy)
* Mit dem Mausrad zoomen (PC)
* Marker mit Zahlen zeigen mehrere Herbergen in der Nähe
* Farben zeigen unterschiedliche Themenbereiche

---

## 🛠️ Technischer Hintergrund (kurz & einfach erklärt)

* Die Karte basiert auf **OpenStreetMap**
* Die Umgebungspunkte stammen aus offenen Geodaten
* Daten werden nur geladen, wenn man sie wirklich braucht
  → dadurch bleibt die Karte schnell und mobil-freundlich

Es werden **keine persönlichen Daten gesammelt**.

---

## 📂 Projektstruktur (vereinfacht)

```
mobile-jh/
 ├── index.html
 ├── data/
 │    ├── jh_only.geojson
 │    ├── jh_13.geojson
 │    ├── jh_578.geojson
 │    └── ...
```

---

## 🧑‍💻 Für Entwickler (optional)

* Frontend: Leaflet + MarkerCluster
* Datenformat: GeoJSON
* Lazy Loading pro Jugendherberge
* GitHub Pages kompatibel
* Fingerübung für eine gemischte Pipline

---

## ❤️ Idee

Die Karte soll Familien helfen, Aufenthalte bei Jugendherbergen besser zu planen – mit einem schnellen Überblick über Bildungs-, Natur- und Freizeitangebote in der Nähe.

