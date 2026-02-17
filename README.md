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

Hier die zusammengeführte, klare und einfache Fassung:

---

## 🛠️ Technischer Hintergrund

* Die Karte basiert auf **OpenStreetMap**
* Umgebungspunkte stammen aus **offenen Geodaten**
* Daten werden im **GeoJSON-Format** bereitgestellt
* Das Frontend nutzt **Leaflet** mit **MarkerCluster**
* POIs werden per **Lazy Loading** pro Jugendherberge nachgeladen
  → dadurch bleibt die Karte performant und mobil-freundlich
* Die Verarbeitung erfolgt in einer modularen Pipeline (R, GDAL, Python, Shell) mit klarer Trennung von Datenaufbereitung, Konvertierung, Aggregation und Veröffentlichung
* Die Anwendung ist vollständig **GitHub Pages kompatibel** (statisches Hosting)

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
 ├── tools/ 
```

Transformation, Konvertierung, Aggregation und Veröffentlichung


Hier eine inhaltlich geschärfte und konzeptionell ergänzte Fassung:

---

## 🏕️ Idee

Die Karte soll Familien bei der Planung von Aufenthalten in Jugendherbergen unterstützen, indem sie einen strukturierten Überblick über Bildungs-, Natur- und Freizeitangebote in der näheren Umgebung bietet.

Konzeptionell basiert die Anwendung auf einer räumlichen Pufferlogik:
Für jede Jugendherberge wird ein definierter Umkreis (Buffer) gebildet, innerhalb dessen relevante Punkte aus offenen Geodaten extrahiert und kategorisiert werden. Dadurch entsteht kein beliebiges Sammelsurium an POIs, sondern eine systematisch abgegrenzte, vergleichbare Umgebungsperspektive.

Ziel ist es, die lokale Angebotsstruktur transparent zu machen – nicht durch Vollständigkeit, sondern durch eine konsistente, reproduzierbare räumliche Auswahl.

