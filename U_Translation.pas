unit U_Translation;

{
Copyright (C) 2012,2021 by Frank Szemkus
 website: https://www.stecknitz-astronomie.de
 email: albireo@stecknitz-astronomie.de

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

{2012-11-11/fs
Unit for text translations (german (DE) <-> englisch (EN))
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, ExtCtrls, ComCtrls, StdCtrls, StrUtils,
  Menus, Buttons;

procedure IniText(Form: TForm; sLANG_ID: string);
function TranslateTextTo(sLANG_ID, sText: string): string;
function GermanChars(sText: string): string;

implementation

function GermanChars(sText: string): string;
// Replaces AE by Ä, ae by ä, OE by ö a.s.o
begin
  Result := sText;
  if(AnsiContainsStr(sText,'uenos')) then exit;
  if(AnsiContainsStr(sText,'srael')) then exit;
  if(AnsiContainsStr(sText,'que')) then exit;
  if(AnsiContainsStr(sText,'uert')) then exit;

  Result := AnsiReplaceStr(Result,'AE','Ä');
  Result := AnsiReplaceStr(Result,'ae','ä');
  Result := AnsiReplaceStr(Result,'OE','Ö');
  Result := AnsiReplaceStr(Result,'oe','ö');
  Result := AnsiReplaceStr(Result,'UE','Ü');
  Result := AnsiReplaceStr(Result,'ue','ü');
end;

function TranslateTextTo(sLANG_ID, sText: string): string;
begin
  Result := '';

  if(sLANG_ID = 'EN') then
  begin
    if(sText = '(50% Fullwell-Sättigung)') then begin Result := '(50% full well saturation)'; exit; end;
    if(sText = '[F1]: Zeitsteuerung anzeigen') then begin Result := '[F1]: Show Time Control'; exit; end;
    if(sText = '# Sterne') then begin Result := '# Stars'; exit; end;
    if(sText = 'A0V-Stern') then begin Result := 'A0V star'; exit; end;
    if(sText = 'Abbildung in Winkel und Pixel') then begin Result := 'Image in angle and pixel'; exit; end;
    if(sText = 'Abbrechen') then begin Result := 'Cancel'; exit; end;
    if(sText = 'Abschnitt') then begin Result := 'Section'; exit; end;
    if(sText = 'Administrieren') then begin Result := 'Administrate'; exit; end;
    if(sText = 'AE') then begin Result := 'AU'; exit; end;
    if(sText = 'Albireo Datenbank') then begin Result := 'Albireo Database'; exit; end;
    if(sText = 'Alle') then begin Result := 'All'; exit; end;
    if(sText = 'Alle visuellen') then begin Result := 'All visual'; exit; end;
    if(sText = 'Altitude Angle') then begin Result := 'Höhenwinkel'; exit; end;
    if(sText = 'Animationen') then begin Result := 'Animations'; exit; end;
    if(sText = 'Andere') then begin Result := 'Other'; exit; end;
    if(sText = 'Ändern') then begin Result := 'Change'; exit; end;
    if(sText = 'Ändern...') then begin Result := 'Change...'; exit; end;
    if(sText = 'Andromedagalaxie') then begin Result := 'Andromeda Galaxy'; exit; end;
    if(sText = 'Ansicht') then begin Result := 'View'; exit; end;
    if(sText = 'Anzahl Frames') then begin Result := 'Number of frames'; exit; end;
    if(sText = 'Äquator') then begin Result := 'Equator'; exit; end;
    if(sText = 'Äquatoriales System') then begin Result := 'Equatorial System'; exit; end;
    if(sText = 'Artikelnummer') then begin Result := 'Article No'; exit; end;
    if(sText = 'Asteroiden') then begin Result := 'Asteroids'; exit; end;
    if(sText = 'Astrogeräte') then begin Result := 'Astro Devices'; exit; end;
    if(sText = 'Astrometrie') then begin Result := 'Astrometry'; exit; end;
    if(sText = 'Astrofotografie') then begin Result := 'Astrophotography'; exit; end;
    if(sText = 'Astrofotografie: Pixel & Co.') then begin Result := 'Astrophotography: Pixels & Co.'; exit; end;
    if(sText = 'Astronomische Zeiten') then begin Result := 'Astronomical Times'; exit; end;
    if(sText = 'Auf Himmelsobjekt ausrichten') then begin Result := 'Align to starry object'; exit; end;
    if(sText = 'Auf Objekt ausrichten') then begin Result := 'Algin to object'; exit; end;
    if(sText = 'Aufgang') then begin Result := 'Rise'; exit; end;
    if(sText = 'Auflösungsvermögen') then begin Result := 'Resolving Capability'; exit; end;
    if(sText = 'Auflösung Atmosph.: 1 ''''') then begin Result := 'Resolving Atmosph.: 1'''''; exit; end;
    if(sText = 'Auflösung Auge: 1 ''') then begin Result := 'Resolving Eye: 1'''; exit; end;
    if(sText = 'Auflösung Hubble: 0.05 ''''') then begin Result := 'Resolving Hubble: 0.05 '''''; exit; end;
    if(sText = 'Auflösung Kamera:') then begin Result := 'Resolving Camera:'; exit; end;
    if(sText = 'Ausblenden') then begin Result := 'Suppress'; exit; end;
    if(sText = 'Ausleserauschen [e-]') then begin Result := 'Read noise [e-]'; exit; end;
    if(sText = 'Austrittspupille') then begin Result := 'Exit Pupil'; exit; end;
    if(sText = 'Auswahl Sternbild') then begin Result := 'Constellation Selection'; exit; end;
    if(sText = 'Äußeres') then begin Result := 'Outer'; exit; end;
    if(sText = 'Äußeres (Asteroiden - Pluto)') then begin Result := 'Outer (Asteroids - Pluto)'; exit; end;
    if(sText = 'Azimut') then begin Result := 'Azimuth'; exit; end;
    if(sText = 'Azimutal') then begin Result := 'Azimuthal'; exit; end;
    if(sText = 'Azimutale Montierung') then begin Result := 'Azimuthal Mount'; exit; end;
    if(sText = 'Bahnberechnung') then begin Result := 'Orbital Calculation'; exit; end;
    if(sText = 'Beenden') then begin Result := 'Close'; exit; end;
    if(sText = 'Beobachtung') then begin Result := 'Observation'; exit; end;
    if(sText = 'Beobachtungsempfehlungen') then begin Result := 'Observation Recommendations'; exit; end;
    if(sText = 'Beginn astronomische Dämmerung') then begin Result := 'Start astrononical twilight'; exit; end;
    if(sText = 'Beginn bürgerliche Dämmerung') then begin Result := 'Start civil twilight'; exit; end;
    if(sText = 'Beginn nautische Dämmerung') then begin Result := 'Start nautical twilight'; exit; end;
    if(sText = 'Beispiele') then begin Result := 'Examples'; exit; end;
    if(sText = 'Benutzerdefiniert') then begin Result := 'User defined'; exit; end;
    if(sText = 'Benutzerdefiniert...') then begin Result := 'Customer...'; exit; end;
    if(sText = 'Benutze dB (Dezibel)') then begin Result := 'Use dB (Decibel)'; exit; end;
    if(sText = 'Beobachtungszeit') then begin Result := 'Observation Time'; exit; end;
    if(sText = 'Berechne') then begin Result := 'Calculate'; exit; end;
    if(sText = 'Berechne Objektgröße') then begin Result := 'Calculate object size'; exit; end;
    if(sText = 'Berechne SNR') then begin Result := 'Calculate SNR'; exit; end;
    if(sText = 'Berechne # Pixel') then begin Result := 'Calculate # Pixel'; exit; end;
    if(sText = 'Beschreibung') then begin Result := 'Description'; exit; end;
    if(sText = 'Beschriftungen') then begin Result := 'Labels'; exit; end;
    if(sText = 'Bewegungsbahn') then begin Result := 'Trace'; exit; end;
    if(sText = 'Bild') then begin Result := 'Picture'; exit; end;
    if(sText = 'Bildeigenschaften') then begin Result := 'Image properties'; exit; end;
    if(sText = 'Bildbetrachter') then begin Result := 'Pictureviewer'; exit; end;
    if(sText = 'Bild hinzufügen') then begin Result := 'Add picture'; exit; end;
    if(sText = 'Bitte lesen') then begin Result := 'Please read'; exit; end;
    if(sText = 'Bitte Sternbild auswählen') then begin Result := 'Please select constellation'; exit; end;
    if(sText = 'Bittiefe') then begin Result := 'Bit depth'; exit; end;
    if(sText = 'Blende') then begin Result := 'Diaphragm'; exit; end;
    if(sText = 'Braune Zwerge') then begin Result := 'Brown Dwarfs'; exit; end;
    if(sText = 'Brennweite') then begin Result := 'Focal length'; exit; end;
    if(sText = 'Brennweite (mm)') then begin Result := 'Focal length (mm)'; exit; end;
    if(sText = 'Coma-Haufen') then begin Result := 'Coma Cluster'; exit; end;
    if(sText = 'DA') then begin Result := 'TA'; exit; end;
    if(sText = 'DN') then begin Result := 'TN'; exit; end;
    if(sText = 'DB') then begin Result := 'TC'; exit; end;
    if(sText = 'Datei') then begin Result := 'File'; exit; end;
    if(sText = 'Datenbank') then begin Result := 'Database'; exit; end;
    if(sText = 'Datenbank-Information') then begin Result := 'Database Information'; exit; end;
    if(sText = 'Datenbank Suchfilter') then begin Result := 'Database Search Filter'; exit; end;
    if(sText = 'Datenschutz') then begin Result := 'Privacy Statement'; exit; end;
    if(sText = 'Datum') then begin Result := 'Date'; exit; end;
    if(sText = 'Datum, Zeit nach Dezimaljahr (date of perihelion transit)') then begin Result := 'Date, Time to Year decimal (date of perihelion transit)'; exit; end;
    if(sText = 'Dein Vorname') then begin Result := 'Your firstname'; exit; end;
    if(sText = 'Dein Zuname') then begin Result := 'Your surname'; exit; end;
    if(sText = 'Deklination') then begin Result := 'Declination'; exit; end;
    if(sText = 'Deutsch') then begin Result := 'German'; exit; end;
    if(sText = 'Diagramm') then begin Result := 'Diagram'; exit; end;
    if(sText = 'Die gleichen Einheiten benutzen!') then begin Result := 'Use the same units!'; exit; end;
    if(sText = 'Dokumentation (PDF)') then begin Result := 'Manual (PDF)'; exit; end;
    if(sText = 'Dunkelstrom 1/2-Temp. [°C]') then begin Result := 'Dark current 1/2-Temp. [°C]'; exit; end;
    if(sText = 'Dunkelstrom Ref.-Temp. [°C]') then begin Result := 'Dark current Ref.-Temp. [°C]'; exit; end;
    if(sText = 'Dunkelstromsignal') then begin Result := 'Dark current signal'; exit; end;
    if(sText = 'Durchmesser') then begin Result := 'Diameter'; exit; end;
    if(sText = 'Durchmesser Milchstraße') then begin Result := 'Diameter Milkyway'; exit; end;
    if(sText = 'Durchmesser Mond') then begin Result := 'Diameter Moon'; exit; end;
    if(sText = 'Echtzeit START') then begin Result := 'Real-Time START'; exit; end;
    if(sText = 'Echtzeit STOP') then begin Result := 'Real-Time STOP'; exit; end;
    if(sText = 'Eigenschaften') then begin Result := 'Properties'; exit; end;
    if(sText = 'Einblenden') then begin Result := 'Fade in'; exit; end;
    if(sText = 'Eingabemaske leeren') then begin Result := 'Clear Input Mask'; exit; end;
    if(sText = 'Einheit') then begin Result := 'Unit'; exit; end;
    if(sText = 'Einzelframe [s]') then begin Result := 'Single frame [s]'; exit; end;
    if(sText = 'Ekliptik') then begin Result := 'Ecliptic'; exit; end;
    if(sText = 'Ende astronomische Dämmerung') then begin Result := 'End of astronomical twilight'; exit; end;
    if(sText = 'Ende bürgerliche Dämmerung') then begin Result := 'End of civil twilight'; exit; end;
    if(sText = 'Ende nautische Dämmerung') then begin Result := 'End of nautical twilight'; exit; end;
    if(sText = 'Englisch') then begin Result := 'English'; exit; end;
    if(sText = 'Entfernen') then begin Result := 'Remove'; exit; end;
    if(sText = 'Entfernung') then begin Result := 'Distance'; exit; end;
    if(sText = 'Entfernung Andromedagalaxie') then begin Result := 'Distance Andromeda Galaxy'; exit; end;
    if(sText = 'Entfernung New Horizons') then begin Result := 'Distance New Horizons'; exit; end;
    if(sText = 'Entfernung Virgohaufen') then begin Result := 'Distance Virgo Cluster'; exit; end;
    if(sText = 'Entfernungsrechner') then begin Result := 'Distance Calculator'; exit; end;
    if(sText = 'Entfernungsumrechnung') then begin Result := 'Distance Calculation'; exit; end;
    if(sText = 'Entfernungen') then begin Result := 'Distances'; exit; end;
    if(sText = 'Erde') then begin Result := 'Earth'; exit; end;
    if(sText = 'Ergebnis') then begin Result := 'Result'; exit; end;
    if(sText = 'Ermittelt') then begin Result := 'Investigated'; exit; end;
    if(sText = 'Erscheinungsjahr') then begin Result := 'Launch year'; exit; end;
    if(sText = 'Erscheinungsjahr Sensor') then begin Result := 'Launch year sensor'; exit; end;
    if(sText = 'Exakte Übereinstimmung') then begin Result := 'Exact Match'; exit; end;
    if(sText = 'Finsternisse') then begin Result := 'Eclipses'; exit; end;
    if(sText = 'Foto') then begin Result := 'Photo'; exit; end;
    if(sText = 'Fotos') then begin Result := 'Pictures'; exit; end;
    if(sText = 'Foto 1') then begin Result := 'Photo 1'; exit; end;
    if(sText = 'Foto 2') then begin Result := 'Photo 2'; exit; end;
    if(sText = 'Foto 3') then begin Result := 'Photo 3'; exit; end;
    if(sText = 'Fullwell-Kapazität [e-]') then begin Result := 'Full well capacity [e-]'; exit; end;
    if(sText = 'Funktion nur für lizensierte Software verfügbar') then begin Result := 'Function only available for licensed software'; exit; end;
    if(sText = 'Galaktische Ebene') then begin Result := 'Galactic Plane'; exit; end;
    if(sText = 'Galaktische Nebel') then begin Result := 'Nebula'; exit; end;
    if(sText = 'Galaxie') then begin Result := 'Galaxy'; exit; end;
    if(sText = 'Galaxien') then begin Result := 'Galaxies'; exit; end;
    if(sText = 'Geburtsjahr') then begin Result := 'Year of Birth'; exit; end;
    if(sText = 'Geographische Breite') then begin Result := 'Latitude'; exit; end;
    if(sText = 'Geographische Koordinaten des Benutzers') then begin Result := 'Geographical User Location'; exit; end;
    if(sText = 'Geogr. Breite') then begin Result := 'Geog. Latitude'; exit; end;
    if(sText = 'Geogr. Länge') then begin Result := 'Geog. Longitude'; exit; end;
    if(sText = 'Gerät') then begin Result := 'Device'; exit; end;
    if(sText = 'Geräte') then begin Result := 'Devices'; exit; end;
    if(sText = 'Geräteeigenschaften') then begin Result := 'Device Properties'; exit; end;
    if(sText = 'Gerätename') then begin Result := 'Device Name'; exit; end;
    if(sText = 'Geringe PC-Ressourcen') then begin Result := 'Low PC Resources'; exit; end;
    if(sText = 'Grad') then begin Result := 'Degree'; exit; end;
    if(sText = 'Grad - Minuten - Sekunden') then begin Result := 'Degree - Minutes - Seconds'; exit; end;
    if(sText = 'Grad (dezimal)') then begin Result := 'Degree (decimal)'; exit; end;
    if(sText = 'Greenwich Zeitverschiebung bei Winterzeit') then begin Result := 'Greenwich Delay at Winter Time'; exit; end;
    if(sText = 'Grenzgröße') then begin Result := 'Max. Mag.'; exit; end;
    if(sText = 'Größenvergleich') then begin Result := 'Size comparison'; exit; end;
    if(sText = 'Hersteller') then begin Result := 'Manufacturer'; exit; end;
    if(sText = 'Hervorheben') then begin Result := 'Highlight'; exit; end;
    if(sText = 'Heute') then begin Result := 'Today'; exit; end;
    if(sText = 'Hilfe') then begin Result := 'Help'; exit; end;
    if(sText = 'Hilfslinien') then begin Result := 'Auxiliary Lines'; exit; end;
    if(sText = 'Himmelshintergrund + Dunkelstrom') then begin Result := 'Background + dark current'; exit; end;
    if(sText = 'Auf Himmelsobjekt ausrichten') then begin Result := 'Align to object of the sky'; exit; end;
    if(sText = '(Hintergrundlimitierung)') then begin Result := '(Background limitation)'; exit; end;
    if(sText = 'Hintergrundsignal') then begin Result := 'Background signal'; exit; end;
    if(sText = 'Hinzufügen') then begin Result := 'Add'; exit; end;
    if(sText = 'Höhe') then begin Result := 'Height'; exit; end;
    if(sText = 'Höhenwinkel') then begin Result := 'Altitude Angle'; exit; end;
    if(sText = 'Horizont') then begin Result := 'Horizon'; exit; end;
    if(sText = 'Horizontansicht') then begin Result := 'Horizon View'; exit; end;
    if(sText = 'Horizontdesigner') then begin Result := 'Horizon Designer'; exit; end;
    if(sText = 'Horizonte auswählen') then begin Result := 'Select Horizons'; exit; end;
    if(sText = 'Horizontsystem') then begin Result := 'Horizontal System'; exit; end;
    if(sText = 'Horizonttyp') then begin Result := 'Horizon Type'; exit; end;
    if(sText = 'Horizont-Dunst') then begin Result := 'Horizon Vapor'; exit; end;

    if(sText = 'Ich akzeptiere die Lizenzbedingungen') then begin Result := 'I accept the terms of licensing'; exit; end;
    if(sText = 'Immer sichtbar') then begin Result := 'Always visible'; exit; end;
    if(sText = 'Inneres') then begin Result := 'Inner'; exit; end;
    if(sText = 'Inneres (Merkur - Mars)') then begin Result := 'Inner (Mercury - Mars)'; exit; end;
    if(sText = 'Intervall') then begin Result := 'Interval'; exit; end;
    if(sText = 'Jetzt') then begin Result := 'Now'; exit; end;
    if(sText = 'Julianische Zeit') then begin Result := 'Julian Time'; exit; end;
    if(sText = 'Kamera öffnen') then begin Result := 'Open Camera'; exit; end;
    if(sText = 'Kameramodell') then begin Result := 'Camera Model'; exit; end;
    if(sText = 'Kataloge') then begin Result := 'Catalogs'; exit; end;
    if(sText = 'Kein Bild verfügbar') then begin Result := 'No Picture available'; exit; end;
    if(sText = 'Kohlenstoffklassen') then begin Result := 'Carbon Classes'; exit; end;
    if(sText = 'Kometen') then begin Result := 'Comets'; exit; end;
    if(sText = 'Kommentar') then begin Result := 'Comment'; exit; end;
    if(sText = 'Kommentar...') then begin Result := 'Comment...'; exit; end;
    if(sText = 'Kugelsternhaufen') then begin Result := 'Globular Cluster'; exit; end;
    if(sText = 'Kulmination') then begin Result := 'Culmination'; exit; end;
    if(sText = 'Länge östl. Greenwich') then begin Result := 'Longitude east of Greenwich'; exit; end;
    if(sText = 'Leuchtkraftklassen') then begin Result := 'Luminosity Classes'; exit; end;
    if(sText = 'Lichtjahre') then begin Result := 'Lightyears'; exit; end;
    if(sText = 'Lichtminuten') then begin Result := 'Lightminutes'; exit; end;
    if(sText = 'Lichtsekunden') then begin Result := 'Lightseconds'; exit; end;
    if(sText = 'Lichtstunden') then begin Result := 'Lighthours'; exit; end;
    if(sText = 'Linienstärke') then begin Result := 'Line Thickness'; exit; end;
    if(sText = 'Lizenzschlüssel') then begin Result := 'License key'; exit; end;
    if(sText = 'Lizenzschlüssel übernehmen') then begin Result := 'Process license key'; exit; end;
    if(sText = 'Lizensierung') then begin Result := 'License Form'; exit; end;
    if(sText = 'Lizensierungsassistent') then begin Result := 'License Assistant'; exit; end;
    if(sText = 'Löschen') then begin Result := 'Delete'; exit; end;
    if(sText = 'Manuelle Sensorauswahl') then begin Result := 'Manual sensor selection'; exit; end;
    if(sText = 'Manuelles GoTo') then begin Result := 'Manual GoTo'; exit; end;
    if(sText = 'Maske neu') then begin Result := 'Form new'; exit; end;
    if(sText = 'Maximale Belichtungszeit') then begin Result := 'Maximum exposure time'; exit; end;
    if(sText = 'Maximale Magnitude') then begin Result := 'Maximum Magnitude'; exit; end;
    if(sText = 'Meine Kennung') then begin Result := 'My ID'; exit; end;
    if(sText = 'Meine Teleskope') then begin Result := 'My Telescopes'; exit; end;
    if(sText = 'Messier-Objekte') then begin Result := 'Messier-Objects'; exit; end;
    if(sText = 'Messierobjekte') then begin Result := 'Messier Objects'; exit; end;
    if(sText = 'Meteorschauer') then begin Result := 'Meteor Shower'; exit; end;
    if(sText = 'Minuten') then begin Result := 'Minutes'; exit; end;
    if(sText = 'Maske leeren') then begin Result := 'Clear Mask'; exit; end;
    if(sText = 'Milchstraße') then begin Result := 'Milkyway'; exit; end;
    if(sText = 'Millisekunden') then begin Result := 'Milliseconds'; exit; end;
    if(sText = 'Minimale Belichtungszeit') then begin Result := 'Minimum exposure time'; exit; end;
    if(sText = 'mittel') then begin Result := 'medium'; exit; end;
    if(sText = 'Modell') then begin Result := 'Model'; exit; end;
    if(sText = 'Monat') then begin Result := 'Month'; exit; end;
    if(sText = 'Mond') then begin Result := 'Moon'; exit; end;
    if(sText = 'Mondaufgang') then begin Result := 'Moonrise'; exit; end;
    if(sText = 'Mondfinsternisse') then begin Result := 'Lunar Eclipses'; exit; end;
    if(sText = 'Mondposition') then begin Result := 'Moon Position'; exit; end;
    if(sText = 'Monduntergang') then begin Result := 'Moonset'; exit; end;
    if(sText = 'Mond(e)') then begin Result := 'Moon(s)'; exit; end;
    if(sText = 'Nacht') then begin Result := 'Night'; exit; end;
    if(sText = 'Neptun') then begin Result := 'Neptune'; exit; end;
    if(sText = 'Newton Spiegelteleskop') then begin Result := 'Newton Reflector'; exit; end;
    if(sText = 'Neues Teleskop erfassen') then begin Result := 'New Telescope'; exit; end;
    if(sText = 'Neue Kamera erfassen') then begin Result := 'New Camera'; exit; end;
    if(sText = 'Nord') then begin Result := 'North'; exit; end;
    if(sText = 'Nur Messierobjekte anzeigen') then begin Result := 'Only display Messier objects'; exit; end;
    if(sText = 'Nur Objekte über dem Horizont anzeigen') then begin Result := 'Display objects above the horizon only'; exit; end;
    if(sText = 'Nur sichtbare anzeigen') then begin Result := 'Only display visible'; exit; end;
    if(sText = 'Nyquist-Kriterium') then begin Result := 'Nyquist Criterion'; exit; end;
    if(sText = 'Öffnungsverhältnis') then begin Result := 'Aperture Ratio'; exit; end;
    if(sText = 'Öffnen') then begin Result := 'Open'; exit; end;
    if(sText = 'Objekt') then begin Result := 'Object'; exit; end;
    if(sText = 'Objektausrichtung') then begin Result := 'Object Alignment'; exit; end;
    if(sText = 'Objektausrtg.') then begin Result := 'Objectalign'; exit; end;
    if(sText = 'Objekte über dem Horizont anzeigen') then begin Result := 'Display objects above the horizon'; exit; end;
    if(sText = 'Objektposition') then begin Result := 'Object Location'; exit; end;
    if(sText = 'Objekt-Koordinaten') then begin Result := 'Object Coordinates'; exit; end;
    if(sText = 'Offene Sternhaufen') then begin Result := 'Open Clusters'; exit; end;
    if(sText = 'Öffnung') then begin Result := 'Aperture'; exit; end;
    if(sText = 'Öffnung (mm)') then begin Result := 'Aperture (mm)'; exit; end;
    if(sText = 'Öffnungsverhältnis') then begin Result := 'Focal Ratio'; exit; end;
    if(sText = 'Okularbrennweiten') then begin Result := 'Focal Widths of Ocular'; exit; end;
    if(sText = 'Ok.-Brennw.') then begin Result := 'Oc.-FW'; exit; end;
    if(sText = 'Optionen') then begin Result := 'Options'; exit; end;
    if(sText = 'Orbitalberechnungen') then begin Result := 'Orbital Calculations'; exit; end;
    if(sText = 'Orbitalparameter') then begin Result := 'Orbital Parameter'; exit; end;
    if(sText = 'Orion-Nebel (M42)') then begin Result := 'Orion Nebula (M42)'; exit; end;
    if(sText = 'Ost') then begin Result := 'East'; exit; end;
    if(sText = 'Output Verzeichnis') then begin Result := 'Output Directory'; exit; end;
    if(sText = 'Parallaktisch') then begin Result := 'Equatorial'; exit; end;
    if(sText = 'Parallaktische Montierung') then begin Result := 'Equatorial Mount'; exit; end;
    if(sText = 'Persönliche Daten') then begin Result := 'Personal Data'; exit; end;
    if(sText = 'Perspektive: Ekliptikprojektion') then begin Result := 'Perspektive: Ecliptic Projection'; exit; end;
    if(sText = 'Perspektive: Neigungsansicht') then begin Result := 'Perspektive: Inclination view'; exit; end;
    if(sText = 'Pixelgröße') then begin Result := 'Pixel Size'; exit; end;
    if(sText = 'Pixelgröße (µm)') then begin Result := 'Pixel Size (µm)'; exit; end;
    if(sText = 'Planetarischer Nebel') then begin Result := 'Planetary Nebula'; exit; end;
    if(sText = 'Planetarische Nebel') then begin Result := 'Planetary Nebulas'; exit; end;
    if(sText = 'Planeten') then begin Result := 'Planets'; exit; end;
    if(sText = 'Planeten anzeigen') then begin Result := 'Show Planets'; exit; end;
    if(sText = 'Planeten, Zwergplaneten && Asteroiden') then begin Result := 'Planets, Dwarf Planets && Asteroids'; exit; end;
    if(sText = 'Polaris - POLAUSRICHTUNG - Kochab') then begin Result := 'Polaris - POLAR ALIGNMENT - Kochab'; exit; end;
    if(sText = 'Polpräzession') then begin Result := 'Pole Precession'; exit; end;
    if(sText = 'Quanteneffizienz V-Band [%]') then begin Result := 'Quantum efficiency V-band [%]'; exit; end;
    if(sText = 'Quasare') then begin Result := 'Quasars'; exit; end;
    if(sText = 'RA-Skala') then begin Result := 'RA-Scale'; exit; end;
    if(sText = 'Realistisch') then begin Result := 'Realistic'; exit; end;
    if(sText = 'Referenzdunkelstrom [e-/s]') then begin Result := 'Dark current reference [e-/s]'; exit; end;
    if(sText = 'Refreshrate [Minuten]') then begin Result := 'Refreshrate [Minutes]'; exit; end;
    if(sText = 'Registrierungsformular') then begin Result := 'Registration Form'; exit; end;
    if(sText = 'Rektaszension') then begin Result := 'Right Ascension'; exit; end;
    if(sText = 'Scheinbare Größe') then begin Result := 'Apparent Size'; exit; end;
    if(sText = 'Scheinbare Helligkeit') then begin Result := 'Apparent Magnitude'; exit; end;
    if(sText = 'schwach') then begin Result := 'faint'; exit; end;
    if(sText = 'Saison/Jahreszeit') then begin Result := 'Season'; exit; end;
    if(sText = 'Sekunden') then begin Result := 'Seconds'; exit; end;
    if(sText = 'Sensordynamik') then begin Result := 'Sensor Dynamics'; exit; end;
    if(sText = 'Sensorformat') then begin Result := 'Sensor Format'; exit; end;
    if(sText = 'Sensortyp') then begin Result := 'Sensor Type'; exit; end;
    if(sText = 'Sichern') then begin Result := 'Save'; exit; end;
    if(sText = 'Sichtbares Universum') then begin Result := 'Visible Universe'; exit; end;
    if(sText = 'Siderische Zeit') then begin Result := 'Siderial Time'; exit; end;
    if(sText = 'Signalpegel & Belichtung') then begin Result := 'Signal level & Exposure'; exit; end;
    if(sText = 'Signalwerte') then begin Result := 'Signal Values'; exit; end;
    if(sText = 'Skalierung') then begin Result := 'Scaling'; exit; end;
    if(sText = 'Sommerzeit') then begin Result := 'Daylight Saving Time'; exit; end;
    if(sText = 'Sommerzeit  ') then begin Result := 'Daylight Saving Time  '; exit; end;
    if(sText = 'Sommerzeit/Winterzeit-Zeitverschiebung in Stunden') then begin Result := 'DST Delay in Hours'; exit; end;
    if(sText = 'Sonne') then begin Result := 'Sun'; exit; end;
    if(sText = 'Sonne und Mond') then begin Result := 'Sun and Moon'; exit; end;
    if(sText = 'Sonne-Merkur') then begin Result := 'Sun-Mercury'; exit; end;
    if(sText = 'Sonnenaufgang') then begin Result := 'Sunrise'; exit; end;
    if(sText = 'Sonnenfinsternisse') then begin Result := 'Solar Eclipses'; exit; end;
    if(sText = 'Sonnensystem') then begin Result := 'Solar System'; exit; end;
    if(sText = 'Sonnenuntergang') then begin Result := 'Sunset'; exit; end;
    if(sText = 'Speichern unter...') then begin Result := 'Save as...'; exit; end;
    if(sText = 'Spektralklassen') then begin Result := 'Spectral Classes'; exit; end;
    if(sText = 'Spektralklasse && Scheinbare Helligkeit') then begin Result := 'Spectral Class && Apparent Magnitude'; exit; end;
    if(sText = 'Spektraltyp') then begin Result := 'Spectral Type'; exit; end;
    if(sText = 'Spenden') then begin Result := 'Donate'; exit; end;
    if(sText = 'Spezifische Klassen') then begin Result := 'Specific Classes'; exit; end;
    if(sText = 'Sprache') then begin Result := 'Language'; exit; end;
    if(sText = 'Staat oder Land') then begin Result := 'State or Province'; exit; end;
    if(sText = 'Stadt oder Ort') then begin Result := 'City or Place'; exit; end;
    if(sText = 'Standardkamera für Teleskop') then begin Result := 'Telescope default camera'; exit; end;
    if(sText = 'Standardteleskop') then begin Result := 'Default Telescope'; exit; end;
    if(sText = 'Standort') then begin Result := 'Location'; exit; end;
    if(sText = 'stark') then begin Result := 'strong'; exit; end;
    if(sText = 'Stern') then begin Result := 'Star'; exit; end;
    if(sText = 'Stern  -  Sonne') then begin Result := 'Star  -  Sun'; exit; end;
    if(sText = 'Sterne') then begin Result := 'Stars'; exit; end;
    if(sText = '(Sternsättigung)') then begin Result := '(Star saturation)'; exit; end;
    if(sText = 'Sternkarte') then begin Result := 'Stellar Map'; exit; end;
    if(sText = 'Sternbild') then begin Result := 'Constellation'; exit; end;
    if(sText = 'Sternbilder') then begin Result := 'Constellations'; exit; end;
    if(sText = 'Sternbild und Umgebung') then begin Result := 'Constellation and Neighbourhood'; exit; end;
    if(sText = 'Sternbildgrenzen') then begin Result := 'Constellation Boundaries'; exit; end;
    if(sText = 'Sternbildnamen') then begin Result := 'Constellation Names'; exit; end;
    if(sText = 'Sternhaufen') then begin Result := 'Cluster'; exit; end;
    if(sText = 'Stundenwinkel') then begin Result := 'Hour Angle'; exit; end;
    if(sText = 'Stunden') then begin Result := 'Hours'; exit; end;
    if(sText = 'Stunden - Minuten - Sekunden') then begin Result := 'Hours - Minutes - Seconds'; exit; end;
    if(sText = 'Stunden (dezimal)') then begin Result := 'Hours (decimal)'; exit; end;
    if(sText = 'Suche') then begin Result := 'Search'; exit; end;
    if(sText = 'Süd') then begin Result := 'South'; exit; end;
    if(sText = 'Tabellen') then begin Result := 'Spreadsheets'; exit; end;
    if(sText = 'Tag') then begin Result := 'Day'; exit; end;
    if(sText = 'Teleskop') then begin Result := 'Telescope'; exit; end;
    if(sText = 'Teleskop löschen') then begin Result := 'Delete Telescope'; exit; end;
    if(sText = 'Teleskopausrichtung') then begin Result := 'Telescope Orientation'; exit; end;
    if(sText = 'Tiefe Magnituden') then begin Result := 'Deep Magnitudes'; exit; end;
    if(sText = 'Trace') then begin Result := 'Bewegungsbahn'; exit; end;
    if(sText = 'Typ') then begin Result := 'Type'; exit; end;
    if(sText = 'Übernehmen') then begin Result := 'Accept'; exit; end;
    if(sText = 'Umrechnung') then begin Result := 'Conversion'; exit; end;
    if(sText = 'Umrechnung Orbitalparameter') then begin Result := 'Conversion of Orbital Parameters'; exit; end;
    if(sText = 'Update prüfen') then begin Result := 'Update Request'; exit; end;
    if(sText = 'Untergang') then begin Result := 'Set'; exit; end;
    if(sText = 'Validität der Sensorwerte') then begin Result := 'Sensor value validity'; exit; end;
    if(sText = 'Verbergen') then begin Result := 'Hide'; exit; end;
    if(sText = 'Vergrößerungen') then begin Result := 'Magnifications'; exit; end;
    if(sText = 'Verzeichnis') then begin Result := 'Directory'; exit; end;
    if(sText = 'Virgohaufen') then begin Result := 'Virgo Cluster'; exit; end;
    if(sText = 'Visualisierung') then begin Result := 'Visualisation'; exit; end;
    if(sText = 'Voreinstellungen') then begin Result := 'Preferences'; exit; end;
    if(sText = 'Viel') then begin Result := 'High'; exit; end;
    if(sText = 'Vis. Horizonthöhe') then begin Result := 'Vis. Horizon Limit'; exit; end;
    if(sText = 'Wenig') then begin Result := 'Low'; exit; end;
    if(sText = 'Willkommen bei Albireo!') then begin Result := 'Welcome to Albireo!'; exit; end;
    if(sText = 'Winkel') then begin Result := 'Angles'; exit; end;
    if(sText = 'Winkelrechner') then begin Result := 'Angle Calculator'; exit; end;
    if(sText = 'Zeige Optionen') then begin Result := 'Show Options'; exit; end;
    if(sText = 'Zeit') then begin Result := 'Time'; exit; end;
    if(sText = 'Zeiteinstellungen') then begin Result := 'Time Settings'; exit; end;
    if(sText = 'Zeitzone') then begin Result := 'Time Zone'; exit; end;
    if(sText = 'Zeitsteuerung') then begin Result := 'Time Control'; exit; end;
    if(sText = 'Zurücksetzen') then begin Result := 'Reset'; exit; end;

    // Longer Text Phrases
    if(sText = 'Willkommens- und Übersichtsseite') then begin Result := 'Welcome Page & Overview'; exit; end;
    if(sText = 'Tabellarische Auflistung astronomischer Objekte in Echtzeit') then begin Result := 'Real-Time Listings of astronomical objects'; exit; end;
    if(sText = 'Manuelle Ausrichtung des Teleskops auf ein Himmelsobjekt') then begin Result := 'Manual telescope alignment to a celestial object'; exit; end;
    if(sText = 'Darstellung des aktuellen Sternenhimmels') then begin Result := 'Visualisation of the starry sky'; exit; end;
    if(sText = 'Verwaltung und Analyse meiner Teleskope') then begin Result := 'Administration and analysis of my telecopes'; exit; end;
    if(sText = 'Zeige Sternaufbau') then begin Result := 'Show star structure'; exit; end;

  end;

  if(sLANG_ID = 'DE') then
  begin
    if(sText = '(50% full well saturation)') then begin Result := '(50% Fullwell-Sättigung)'; exit; end;
    if(sText = '[F1]: Show Time Control') then begin Result := '[F1]: Zeitsteuerung anzeigen'; exit; end;
    if(sText = '# Stars') then begin Result := '# Sterne'; exit; end;
    if(sText = 'A0V star') then begin Result := 'A0V-Stern'; exit; end;
    if(sText = 'Accept') then begin Result := 'Übernehmen'; exit; end;
    if(sText = 'Add') then begin Result := 'Hinzufügen'; exit; end;
    if(sText = 'Add picture') then begin Result := 'Bild hinzufügen'; exit; end;
    if(sText = 'Administrate') then begin Result := 'Administrieren'; exit; end;
    if(sText = 'Albireo Database') then begin Result := 'Albireo Datenbank'; exit; end;
    if(sText = 'Algin to object') then begin Result := 'Auf Objekt ausrichten'; exit; end;
    if(sText = 'Align to starry object') then begin Result := 'Auf Himmelsobjekt ausrichten'; exit; end;
    if(sText = 'All') then begin Result := 'Alle'; exit; end;
    if(sText = 'All visual') then begin Result := 'Alle visuellen'; exit; end;
    if(sText = 'Altitude Angle') then begin Result := 'Höhenwinkel'; exit; end;
    if(sText = 'Always visible') then begin Result := 'Immer sichtbar'; exit; end;
    if(sText = 'Andromeda Galaxy') then begin Result := 'Andromedagalaxie'; exit; end;
    if(sText = 'Angles') then begin Result := 'Winkel'; exit; end;
    if(sText = 'Angle Calculator') then begin Result := 'Winkelrechner'; exit; end;
    if(sText = 'Animations') then begin Result := 'Animationen'; exit; end;
    if(sText = 'Aperture') then begin Result := 'Öffnung'; exit; end;
    if(sText = 'Aperture (mm)') then begin Result := 'Öffnung (mm)'; exit; end;
    if(sText = 'Apparent Magnitude') then begin Result := 'Scheinbare Helligkeit'; exit; end;
    if(sText = 'Apparent Size') then begin Result := 'Scheinbare Größe'; exit; end;
    if(sText = 'Article No') then begin Result := 'Artikelnummer'; exit; end;
    if(sText = 'Asteroids') then begin Result := 'Asteroiden'; exit; end;
    if(sText = 'Astrophotography') then begin Result := 'Astrofotografie'; exit; end;
    if(sText = 'Astrophotography: Pixels & Co.') then begin Result := 'Astrofotografie: Pixel & Co.'; exit; end;
    if(sText = 'Astrometry') then begin Result := 'Astrometrie'; exit; end;
    if(sText = 'Astronomical Times') then begin Result := 'Astronomische Zeiten'; exit; end;
    if(sText = 'AU') then begin Result := 'AE'; exit; end;
    if(sText = 'Auxiliary Lines') then begin Result := 'Hilfslinien'; exit; end;
    if(sText = 'Azimuth') then begin Result := 'Azimut'; exit; end;
    if(sText = 'Azimuthal') then begin Result := 'Azimutal'; exit; end;
    if(sText = 'Azimuthal Mount') then begin Result := 'Azimutale Montierung'; exit; end;
    if(sText = 'Background + dark current') then begin Result := 'Himmelshintergrund + Dunkelstrom'; exit; end;
    if(sText = '(Background limitation)') then begin Result := '(Hintergrundlimitierung)'; exit; end;
    if(sText = 'Background signal') then begin Result := 'Hintergrundsignal'; exit; end;
    if(sText = 'Bit depth') then begin Result := 'Bittiefe'; exit; end;
    if(sText = 'Brown Dwarfs') then begin Result := 'Braune Zwerge'; exit; end;
    if(sText = 'Calculate') then begin Result := 'Berechne'; exit; end;
    if(sText = 'Calculate object size') then begin Result := 'Berechne Objektgröße'; exit; end;
    if(sText = 'Calculate SNR') then begin Result := 'Berechne SNR'; exit; end;
    if(sText = 'Calculate # Pixel') then begin Result := 'Berechne # Pixel'; exit; end;
    if(sText = 'Camera Model') then begin Result := 'Kameramodell'; exit; end;
    if(sText = 'Cancel') then begin Result := 'Abbrechen'; exit; end;
    if(sText = 'Carbon Classes') then begin Result := 'Kohlenstoffklassen'; exit; end;
    if(sText = 'Catalogs') then begin Result := 'Kataloge'; exit; end;
    if(sText = 'Change') then begin Result := 'Ändern'; exit; end;
    if(sText = 'Change...') then begin Result := 'Ändern...'; exit; end;
    if(sText = 'City or Place') then begin Result := 'Stadt oder Ort'; exit; end;
    if(sText = 'Clear Input Mask') then begin Result := 'Eingabemaske leeren'; exit; end;
    if(sText = 'Clear Mask') then begin Result := 'Maske leeren'; exit; end;
    if(sText = 'Close') then begin Result := 'Beenden'; exit; end;
    if(sText = 'Cluster') then begin Result := 'Sternhaufen'; exit; end;
    if(sText = 'Coma Cluster') then begin Result := 'Coma-Haufen'; exit; end;
    if(sText = 'Comets') then begin Result := 'Kometen'; exit; end;
    if(sText = 'Comment') then begin Result := 'Kommentar'; exit; end;
    if(sText = 'Comment...') then begin Result := 'Kommentar...'; exit; end;
    if(sText = 'Constellation') then begin Result := 'Sternbild'; exit; end;
    if(sText = 'Constellations') then begin Result := 'Sternbilder'; exit; end;
    if(sText = 'Constellation and Neighbourhood') then begin Result := 'Sternbild und Umgebung'; exit; end;
    if(sText = 'Constellation Boundaries') then begin Result := 'Sternbildgrenzen'; exit; end;
    if(sText = 'Constellation Names') then begin Result := 'Sternbildnamen'; exit; end;
    if(sText = 'Constellation Selection') then begin Result := 'Auswahl Sternbild'; exit; end;
    if(sText = 'Conversion') then begin Result := 'Umrechnung'; exit; end;
    if(sText = 'Conversion of Orbital Parameters') then begin Result := 'Umrechnung Orbitalparameter'; exit; end;
    if(sText = 'Culmination') then begin Result := 'Kulmination'; exit; end;
    if(sText = 'Customer...') then begin Result := 'Benutzerdefiniert...'; exit; end;
    if(sText = 'Dark current 1/2-Temp. [°C]') then begin Result := 'Dunkelstrom 1/2-Temp. [°C]'; exit; end;
    if(sText = 'Dark current Ref.-Temp. [°C]') then begin Result := 'Dunkelstrom Ref.-Temp. [°C]'; exit; end;
    if(sText = 'Dark current reference [e-/s]') then begin Result := 'Referenzdunkelstrom [e-/s]'; exit; end;
    if(sText = 'Dark current signal') then begin Result := 'Dunkelstromsignal'; exit; end;
    if(sText = 'Database') then begin Result := 'Datenbank'; exit; end;
    if(sText = 'Database Information') then begin Result := 'Datenbank-Information'; exit; end;
    if(sText = 'Database Search Filter') then begin Result := 'Datenbank Suchfilter'; exit; end;
    if(sText = 'Date') then begin Result := 'Datum'; exit; end;
    if(sText = 'Date, Time to Year decimal (date of perihelion transit)') then begin Result := 'Datum, Zeit nach Dezimaljahr (date of perihelion transit)'; exit; end;
    if(sText = 'Day') then begin Result := 'Tag'; exit; end;
    if(sText = 'Daylight Saving Time  ') then begin Result := 'Sommerzeit  '; exit; end;
    if(sText = 'Declination') then begin Result := 'Deklination'; exit; end;
    if(sText = 'DEC-Scale') then begin Result := 'DEC-Skala'; exit; end;
    if(sText = 'Deep Magnitudes') then begin Result := 'Tiefe Magnituden'; exit; end;
    if(sText = 'Default Telescope') then begin Result := 'Standardteleskop'; exit; end;
    if(sText = 'Degree') then begin Result := 'Grad'; exit; end;
    if(sText = 'Degree - Minutes - Seconds') then begin Result := 'Grad - Minuten - Sekunden'; exit; end;
    if(sText = 'Degree (decimal)') then begin Result := 'Grad (dezimal)'; exit; end;
    if(sText = 'Delete') then begin Result := 'Löschen'; exit; end;
    if(sText = 'Delete Telescope') then begin Result := 'Teleskop löschen'; exit; end;
    if(sText = 'Description') then begin Result := 'Beschreibung'; exit; end;
    if(sText = 'Device') then begin Result := 'Gerät'; exit; end;
    if(sText = 'Devices') then begin Result := 'Geräte'; exit; end;
    if(sText = 'Device Name') then begin Result := 'Gerätename'; exit; end;
    if(sText = 'Device Properties') then begin Result := 'Geräteeigenschaften'; exit; end;
    if(sText = 'Diagram') then begin Result := 'Diagramm'; exit; end;
    if(sText = 'Diameter') then begin Result := 'Durchmesser'; exit; end;
    if(sText = 'Diaphragm') then begin Result := 'Blende'; exit; end;
    if(sText = 'Directory') then begin Result := 'Verzeichnis'; exit; end;
    if(sText = 'Display objects above the horizon only') then begin Result := 'Nur Objekte über dem Horizont anzeigen'; exit; end;
    if(sText = 'Distance') then begin Result := 'Entfernung'; exit; end;
    if(sText = 'Distance Andromeda Galaxy') then begin Result := 'Entfernung Andromedagalaxie'; exit; end;
    if(sText = 'Distance Calculator') then begin Result := 'Entfernungsrechner'; exit; end;
    if(sText = 'Distance New Horizons') then begin Result := 'Entfernung New Horizons'; exit; end;
    if(sText = 'Distance Virgo Cluster') then begin Result := 'Entfernung Virgohaufen'; exit; end;
    if(sText = 'Distances') then begin Result := 'Entfernungen'; exit; end;
    if(sText = 'Distance Calculation') then begin Result := 'Entfernungsumrechnung'; exit; end;
    if(sText = 'Donate') then begin Result := 'Spenden'; exit; end;
    if(sText = 'DST Delay in Hours') then begin Result := 'Sommerzeit/Winterzeit-Zeitverschiebung in Stunden'; exit; end;
    if(sText = 'Earth') then begin Result := 'Erde'; exit; end;
    if(sText = 'East') then begin Result := 'Ost'; exit; end;
    if(sText = 'Eclipses') then begin Result := 'Finsternisse'; exit; end;
    if(sText = 'Ecliptic') then begin Result := 'Ekliptik'; exit; end;
    if(sText = 'End of astronomical twilight') then begin Result := 'Ende astronomische Dämmerung'; exit; end;
    if(sText = 'End of civil twilight') then begin Result := 'Ende bürgerliche Dämmerung'; exit; end;
    if(sText = 'End of nautical twilight') then begin Result := 'Ende nautische Dämmerung'; exit; end;
    if(sText = 'English') then begin Result := 'Englisch'; exit; end;
    if(sText = 'Equator') then begin Result := 'Äquator'; exit; end;
    if(sText = 'Equatorial') then begin Result := 'Parallaktisch'; exit; end;
    if(sText = 'Equatorial Mount') then begin Result := 'Parallaktische Montierung'; exit; end;
    if(sText = 'Equatorial System') then begin Result := 'Äquatoriales System'; exit; end;
    if(sText = 'Exact Match') then begin Result := 'Exakte Übereinstimmung'; exit; end;
    if(sText = 'Examples') then begin Result := 'Beispiele'; exit; end;
    if(sText = 'Exit Pupil') then begin Result := 'Austrittspupille'; exit; end;
    if(sText = 'Fade in') then begin Result := 'Einblenden'; exit; end;
    if(sText = 'faint') then begin Result := 'schwach'; exit; end;
    if(sText = 'File') then begin Result := 'Datei'; exit; end;
    if(sText = 'Focal length') then begin Result := 'Brennweite'; exit; end;
    if(sText = 'Focal length (mm)') then begin Result := 'Brennweite (mm)'; exit; end;
    if(sText = 'Focal Ratio') then begin Result := 'Öffnungsverhältnis'; exit; end;
    if(sText = 'Focal Widths of Ocular') then begin Result := 'Okularbrennweiten'; exit; end;
    if(sText = 'Form new') then begin Result := 'Maske neu'; exit; end;
    if(sText = 'Full well capacity [e-]') then begin Result := 'Fullwell-Kapazität [e-]'; exit; end;
    if(sText = 'Function only available for licensed software') then begin Result := 'Funktion nur für lizensierte Software verfügbar'; exit; end;
    if(sText = 'Galactic Plane') then begin Result := 'Galaktische Ebene'; exit; end;
    if(sText = 'Galaxies') then begin Result := 'Galaxien'; exit; end;
    if(sText = 'Galaxy') then begin Result := 'Galaxie'; exit; end;
    if(sText = 'Geographical User Location') then begin Result := 'Geographische Koordinaten des Benutzers'; exit; end;
    if(sText = 'Geog. Latitude') then begin Result := 'Geogr. Breite'; exit; end;
    if(sText = 'Geog. Longitude') then begin Result := 'Geogr. Länge'; exit; end;
    if(sText = 'German') then begin Result := 'Deutsch'; exit; end;
    if(sText = 'Globular Cluster') then begin Result := 'Kugelsternhaufen'; exit; end;
    if(sText = 'Greenwich Delay at Winter Time') then begin Result := 'Greenwich Zeitverschiebung bei Winterzeit'; exit; end;
    if(sText = 'Help') then begin Result := 'Hilfe'; exit; end;
    if(sText = 'Hide') then begin Result := 'Verbergen'; exit; end;
    if(sText = 'High') then begin Result := 'Viel'; exit; end;
    if(sText = 'Highlight') then begin Result := 'Hervorheben'; exit; end;
    if(sText = 'Horizon') then begin Result := 'Horizont'; exit; end;
    if(sText = 'Horizon Designer') then begin Result := 'Horizontdesigner'; exit; end;
    if(sText = 'Horizon Type') then begin Result :='Horizonttyp'; exit; end;
    if(sText = 'Horizon Vapor') then begin Result := 'Horizont-Dunst'; exit; end;
    if(sText = 'Horizon View') then begin Result := 'Horizontansicht'; exit; end;
    if(sText = 'Horizontal System') then begin Result := 'Horizontsystem'; exit; end;
    if(sText = 'Hour Angle') then begin Result := 'Stundenwinkel'; exit; end;
    if(sText = 'Hours') then begin Result := 'Stunden'; exit; end;
    if(sText = 'Hours - Minutes - Seconds') then begin Result := 'Stunden - Minuten - Sekunden'; exit; end;
    if(sText = 'Hours (decimal)') then begin Result := 'Stunden (dezimal)'; exit; end;
    if(sText = 'Image properties') then begin Result := 'Bildeigenschaften'; exit; end;
    if(sText = 'Image in angle and pixel') then begin Result := 'Abbildung in Winkel und Pixel'; exit; end;
    if(sText = 'Inner') then begin Result := 'Inneres'; exit; end;
    if(sText = 'Inner (Mercury - Mars)') then begin Result := 'Inneres (Merkur - Mars)'; exit; end;
    if(sText = 'Investigated') then begin Result := 'Ermittelt'; exit; end;
    if(sText = 'Interval') then begin Result := 'Intervall'; exit; end;
    if(sText = 'I accept the terms of licensing') then begin Result := 'Ich akzeptiere die Lizenzbedingungen'; exit; end;
    if(sText = 'Julian Time') then begin Result := 'Julianische Zeit'; exit; end;
    if(sText = 'Labels') then begin Result := 'Beschriftungen'; exit; end;
    if(sText = 'Latitude') then begin Result := 'Geographische Breite'; exit; end;
    if(sText = 'Language') then begin Result := 'Sprache'; exit; end;
    if(sText = 'Launch year') then begin Result := 'Erscheinungsjahr'; exit; end;
    if(sText = 'Launch year sensor') then begin Result := 'Erscheinungsjahr Sensor'; exit; end;
    if(sText = 'Let time go') then begin Result := 'Zeit laufen lassen'; exit; end;
    if(sText = 'License Assistant') then begin Result := 'Lizensierungsassistent'; exit; end;
    if(sText = 'License Form') then begin Result := 'Lizensierung'; exit; end;
    if(sText = 'License key') then begin Result := 'Lizenzschlüssel'; exit; end;
    if(sText = 'Lighthours') then begin Result := 'Lichtstunden'; exit; end;
    if(sText = 'Lightminutes') then begin Result := 'Lichtminuten'; exit; end;
    if(sText = 'Lightseconds') then begin Result := 'Lichtsekunden'; exit; end;
    if(sText = 'Lightyears') then begin Result := 'Lichtjahre'; exit; end;
    if(sText = 'Line Thickness') then begin Result := 'Linienstärke'; exit; end;
    if(sText = 'Location') then begin Result := 'Standort'; exit; end;
    if(sText = 'Longitude east of Greenwich') then begin Result := 'Länge östl. Greenwich'; exit; end;
    if(sText = 'Low') then begin Result := 'Wenig'; exit; end;
    if(sText = 'Low PC Resources') then begin Result := 'Geringe PC-Ressourcen'; exit; end;
    if(sText = 'Luminosity Classes') then begin Result := 'Leuchtkraftklassen'; exit; end;
    if(sText = 'Lunar Eclipses') then begin Result := 'Mondfinsternisse'; exit; end;
    if(sText = 'Manual GoTo') then begin Result := 'Manuelles GoTo'; exit; end;
    if(sText = 'Manual sensor selection') then begin Result := 'Manuelle Sensorauswahl'; exit; end;
    if(sText = 'Manual (PDF)') then begin Result := 'Dokumentation (PDF)'; exit; end;
    if(sText = 'Manufacturer') then begin Result := 'Hersteller'; exit; end;
    if(sText = 'Magnifications') then begin Result := 'Vergrößerungen'; exit; end;
    if(sText = 'Max. Mag.') then begin Result := 'Grenzgröße'; exit; end;
    if(sText = 'Maximum exposure time') then begin Result := 'Maximale Belichtungszeit'; exit; end;
    if(sText = 'Maximum Magnitude') then begin Result := 'Maximale Magnitude'; exit; end;
    if(sText = 'medium') then begin Result := 'mittel'; exit; end;
    if(sText = 'Messier-Objects') then begin Result := 'Messier-Objekte'; exit; end;
    if(sText = 'Messier Objects') then begin Result := 'Messierobjekte'; exit; end;
    if(sText = 'Meteor Shower') then begin Result := 'Meteorschauer'; exit; end;
    if(sText = 'Milliseconds') then begin Result := 'Millisekunden'; exit; end;
    if(sText = 'Milkyway') then begin Result := 'Milchstraße'; exit; end;
    if(sText = 'Minimum exposure time') then begin Result := 'Minimale Belichtungszeit'; exit; end;
    if(sText = 'Minutes') then begin Result := 'Minuten'; exit; end;
    if(sText = 'Model') then begin Result := 'Modell'; exit; end;
    if(sText = 'Moon') then begin Result := 'Mond'; exit; end;
    if(sText = 'Moonrise') then begin Result := 'Mondaufgang'; exit; end;
    if(sText = 'Moonset') then begin Result := 'Monduntergang'; exit; end;
    if(sText = 'Moon Position') then begin Result := 'Mondposition'; exit; end;
    if(sText = 'Moon(s)') then begin Result := 'Mond(e)'; exit; end;
    if(sText = 'Month') then begin Result := 'Monat'; exit; end;
    if(sText = 'My ID') then begin Result := 'Meine Kennung'; exit; end;
    if(sText = 'My Telescopes') then begin Result := 'Meine Teleskope'; exit; end;
    if(sText = 'Nebula') then begin Result := 'Galaktische Nebel'; exit; end;
    if(sText = 'Neptune') then begin Result := 'Neptun'; exit; end;
    if(sText = 'Newton Reflector') then begin Result := 'Newton Spiegelteleskop'; exit; end;
    if(sText = 'New Camera') then begin Result := 'Neue Kamera erfassen'; exit; end;
    if(sText = 'New Telescope') then begin Result := 'Neues Teleskop erfassen'; exit; end;
    if(sText = 'Night') then begin Result := 'Nacht'; exit; end;
    if(sText = 'North') then begin Result := 'Nord'; exit; end;
    if(sText = 'Now') then begin Result := 'Jetzt'; exit; end;
    if(sText = 'No Picture available') then begin Result := 'Kein Bild verfügbar'; exit; end;
    if(sText = 'Number of frames') then begin Result := 'Anzahl Frames'; exit; end;
    if(sText = 'Nyquist Criterion') then begin Result := 'Nyquist-Kriterium'; exit; end;
    if(sText = 'Oc.-FW') then begin Result := 'Ok.-Brennw.'; exit; end;
    if(sText = 'Object') then begin Result := 'Objekt'; exit; end;
    if(sText = 'Objectalign') then begin Result := 'Objektausrtg.'; exit; end;
    if(sText = 'Object Alignment') then begin Result := 'Objektausrichtung'; exit; end;
    if(sText = 'Object Location') then begin Result := 'Objektposition'; exit; end;
    if(sText = 'Object Coordinates') then begin Result := 'Objekt-Koordinaten'; exit; end;
    if(sText = 'Observation') then begin Result := 'Beobachtung'; exit; end;
    if(sText = 'Observation Recommendations') then begin Result := 'Beobachtungsempfehlungen'; exit; end;
    if(sText = 'Observation Time') then begin Result := 'Beobachtungszeit'; exit; end;
    if(sText = 'Open') then begin Result := 'Öffnen'; exit; end;
    if(sText = 'Open Camera') then begin Result := 'Kamera öffnen'; exit; end;
    if(sText = 'Open Clusters') then begin Result := 'Offene Sternhaufen'; exit; end;
    if(sText = 'Options') then begin Result := 'Optionen'; exit; end;
    if(sText = 'Orbital Calculation') then begin Result := 'Bahnberechnung'; exit; end;
    if(sText = 'Orbital Calculations') then begin Result := 'Orbitalberechnungen'; exit; end;
    if(sText = 'Orbital Parameter') then begin Result := 'Orbitalparameter'; exit; end;
    if(sText = 'Orion Nebula (M42)') then begin Result := 'Orion-Nebel (M42)'; exit; end;
    if(sText = 'Only display Messier objects') then begin Result := 'Nur Messierobjekte anzeigen'; exit; end;
    if(sText = 'Only display visible') then begin Result := 'Nur sichtbare anzeigen'; exit; end;
    if(sText = 'Other') then begin Result := 'Andere'; exit; end;
    if(sText = 'Outer') then begin Result := 'Äußeres'; exit; end;
    if(sText = 'Outer (Asteroids - Pluto') then begin Result := 'Äußeres (Asteroiden - Pluto)'; exit; end;
    if(sText = 'Output Directory') then begin Result := 'Output Verzeichnis'; exit; end;
    if(sText = 'Personal Data') then begin Result := 'Persönliche Daten'; exit; end;
    if(sText = 'Perspektive: Ecliptic Projection') then begin Result := 'Perspektive: Ekliptikprojektion'; exit; end;
    if(sText = 'Perspektive: Inclination view') then begin Result := 'Perspektive: Neigungsansicht'; exit; end;
    if(sText = 'Photo') then begin Result := 'Foto'; exit; end;
    if(sText = 'Pictures') then begin Result := 'Fotos'; exit; end;
    if(sText = 'Pictureviewer') then begin Result := 'Bildbetrachter'; exit; end;
    if(sText = 'Photo 1') then begin Result := 'Foto 1'; exit; end;
    if(sText = 'Photo 2') then begin Result := 'Foto 2'; exit; end;
    if(sText = 'Photo 3') then begin Result := 'Foto 3'; exit; end;
    if(sText = 'Picture') then begin Result := 'Bild'; exit; end;
    if(sText = 'Pixel Size') then begin Result := 'Pixelgröße'; exit; end;
    if(sText = 'Pixel Size (µm)') then begin Result := 'Pixelgröße (µm)'; exit; end;
    if(sText = 'Planetary Nebulas') then begin Result := 'Planetarische Nebel'; exit; end;
    if(sText = 'Planetarische Nebel') then begin Result := ''; exit; end;
    if(sText = 'Planets') then begin Result := 'Planeten'; exit; end;
    if(sText = 'Planets, Dwarf Planets && Asteroids') then begin Result := 'Planeten, Zwergplaneten && Asteroiden'; exit; end;
    if(sText = 'Please read') then begin Result := 'Bitte lesen'; exit; end;
    if(sText = 'Please select constellation') then begin Result := 'Bitte Sternbild auswählen'; exit; end;
    if(sText = 'Polaris - POLAR ALIGNMENT - Kochab') then begin Result := 'Polaris - POLAUSRICHTUNG - Kochab'; exit; end;
    if(sText = 'Pole Precession') then begin Result := 'Polpräzession'; exit; end;
    if(sText = 'Preferences') then begin Result := 'Voreinstellungen'; exit; end;
    if(sText = 'Privacy Statement') then begin Result := 'Datenschutz'; exit; end;
    if(sText = 'Process license key') then begin Result := 'Lizenzschlüssel übernehmen'; exit; end;
    if(sText = 'Properties') then begin Result := 'Eigenschaften'; exit; end;
    if(sText = 'Quantum efficiency V-band [%]') then begin Result := 'Quanteneffizienz V-Band [%]'; exit; end;
    if(sText = 'Quasars') then begin Result := 'Quasare'; exit; end;
    if(sText = 'Read noise [e-]') then begin Result := 'Ausleserauschen [e-]'; exit; end;
    if(sText = 'Realistic') then begin Result := 'Realistisch'; exit; end;
    if(sText = 'Real-Time START') then begin Result := 'Echtzeit START'; exit; end;
    if(sText = 'Real-Time STOP') then begin Result := 'Echtzeit STOP'; exit; end;
    if(sText = 'Refreshrate [Minutes]') then begin Result := 'Refreshrate [Minuten]'; exit; end;
    if(sText = 'Registration Form') then begin Result := 'Registrierungsformular'; exit; end;
    if(sText = 'Remove') then begin Result := 'Entfernen'; exit; end;
    if(sText = 'Reset') then begin Result := 'Zurücksetzen'; exit; end;
    if(sText = 'Resolving Atmosph.: 1 ''''') then begin Result := 'Auflösung Atmosph.: 1 '''''; exit; end;
    if(sText = 'Resolving Eye: 1 ''') then begin Result := 'Auflösung Auge: 1 '''; exit; end;
    if(sText = 'Resolving Hubble: 0.05 ''''') then begin Result := 'Auflösung Hubble: 0.05 '''''; exit; end;
    if(sText = 'Resolving Camera:') then begin Result := 'Auflösung Kamera:'; exit; end;
    if(sText = 'Resolving Capability') then begin Result := 'Auflösungsvermögen'; exit; end;
    if(sText = 'Result') then begin Result := 'Ergebnis'; exit; end;
    if(sText = 'Right Ascension') then begin Result := 'Rektaszension'; exit; end;
    if(sText = 'Rise') then begin Result := 'Aufgang'; exit; end;
    if(sText = 'Save') then begin Result := 'Sichern'; exit; end;
    if(sText = 'Save as...') then begin Result := 'Speichern unter...'; exit; end;
    if(sText = 'Search') then begin Result := 'Suche'; exit; end;
    if(sText = 'Seconds') then begin Result := 'Sekunden'; exit; end;
    if(sText = 'Section') then begin Result := 'Abschnitt'; exit; end;
    if(sText = 'Scaling') then begin Result := 'Skalierung'; exit; end;
    if(sText = 'Season') then begin Result := 'Saison/Jahreszeit'; exit; end;
    if(sText = 'Select Horizons') then begin Result := 'Horizonte auswählen'; exit; end;
    if(sText = 'Sensor Dynamics') then begin Result := 'Sensordynamik'; exit; end;
    if(sText = 'Sensor Format') then begin Result := 'Sensorformat'; exit; end;
    if(sText = 'Sensor Type') then begin Result := 'Sensortyp'; exit; end;
    if(sText = 'Sensor value validity') then begin Result := 'Validität der Sensorwerte'; exit; end;
    if(sText = 'Set') then begin Result := 'Untergang'; exit; end;
    if(sText = 'Show Options') then begin Result := 'Zeige Optionen'; exit; end;
    if(sText = 'Show Planets') then begin Result := 'Planeten anzeigen'; exit; end;
    if(sText = 'Siderial Time') then begin Result := 'Siderische Zeit'; exit; end;
    if(sText = 'Signal level & Exposure') then begin Result := 'Signalpegel & Belichtung'; exit; end;
    if(sText = 'Signal Values') then begin Result := 'Signalwerte'; exit; end;
    if(sText = 'Single frame [s]') then begin Result := 'Einzelframe [s]'; exit; end;
    if(sText = 'Size comparison') then begin Result := 'Größenvergleich'; exit; end;
    if(sText = 'Solar Eclipses') then begin Result := 'Sonnenfinsternisse'; exit; end;
    if(sText = 'Solar System') then begin Result := 'Sonnensystem'; exit; end;
    if(sText = 'South') then begin Result := 'Süd'; exit; end;
    if(sText = 'Specific Classes') then begin Result := 'Spezifische Klassen'; exit; end;
    if(sText = 'Spectral Classes') then begin Result := 'Spektralklassen'; exit; end;
    if(sText = 'Spectral Class && Apparent Magnitude') then begin Result := 'Spektralklasse && Scheinbare Helligkeit'; exit; end;
    if(sText = 'Spectral Type') then begin Result := 'Spektraltyp'; exit; end;
    if(sText = 'Spreadsheets') then begin Result := 'Tabellen'; exit; end;
    if(sText = 'Star saturation') then begin Result := 'Sternsättigung'; exit; end;
    if(sText = 'Star') then begin Result := 'Stern'; exit; end;
    if(sText = 'Star  -  Sun') then begin Result := 'Stern  -  Sonne'; exit; end;
    if(sText = 'Stars') then begin Result := 'Sterne'; exit; end;
    if(sText = 'Start astrononical twilight') then begin Result := 'Beginn astronomische Dämmerung'; exit; end;
    if(sText = 'Start civil twilight') then begin Result := 'Beginn bürgerliche Dämmerung'; exit; end;
    if(sText = 'Start nautical twilight') then begin Result := 'Beginn nautische Dämmerung'; exit; end;
    if(sText = 'State or Province') then begin Result := 'Staat oder Land'; exit; end;
    if(sText = 'Stellar Map') then begin Result := 'Sternkarte'; exit; end;
    if(sText = 'strong') then begin Result := 'stark'; exit; end;
    if(sText = 'Sun') then begin Result := 'Sonne'; exit; end;
    if(sText = 'Sunrise') then begin Result := 'Sonnenaufgang'; exit; end;
    if(sText = 'Sunset') then begin Result := 'Sonnenuntergang'; exit; end;
    if(sText = 'Sun and Moon') then begin Result := 'Sonne und Mond'; exit; end;
    if(sText = 'Sun-Mercury') then begin Result := 'Sonne-Merkur'; exit; end;
    if(sText = 'Suppress') then begin Result := 'Ausblenden'; exit; end;
    if(sText = 'TA') then begin Result := 'DA'; exit; end;
    if(sText = 'TC') then begin Result := 'DB'; exit; end;
    if(sText = 'Telescope') then begin Result := 'Teleskop'; exit; end;
    if(sText = 'Telescope Orientation') then begin Result := 'Teleskopausrichtung'; exit; end;
    if(sText = 'Telescope default camera') then begin Result := 'Standardkamera für Teleskop'; exit; end;
    if(sText = 'Time') then begin Result := 'Zeit'; exit; end;
    if(sText = 'Time Control') then begin Result := 'Zeitsteuerung'; exit; end;
    if(sText = 'Time Settings') then begin Result := 'Zeiteinstellungen'; exit; end;
    if(sText = 'Time Zone') then begin Result := 'Zeitzone'; exit; end;
    if(sText = 'TN') then begin Result := 'DN'; exit; end;
    if(sText = 'Today') then begin Result := 'Heute'; exit; end;
    if(sText = 'Trace') then begin Result := 'Bewegungsbahn'; exit; end;
    if(sText = 'Type') then begin Result := 'Typ'; exit; end;
    if(sText = 'Unit') then begin Result := 'Einheit'; exit; end;
    if(sText = 'Update Request') then begin Result := 'Update prüfen'; exit; end;
    if(sText = 'Use dB (Decibel)') then begin Result := 'Benutze dB (Dezibel)'; exit; end;
    if(sText = 'Use the same units!') then
      begin Result := 'Die gleichen Einheiten benutzen!'; exit; end;
    if(sText = 'User defined') then begin Result := 'Benutzerdefiniert'; exit; end;
    if(sText = 'View') then begin Result := 'Ansicht'; exit; end;
    if(sText = 'Virgo Cluster') then begin Result := 'Virgohaufen'; exit; end;
    if(sText = 'Visible Universe') then begin Result := 'Sichtbares Universum'; exit; end;
    if(sText = 'Visualisation') then begin Result := 'Visualisierung'; exit; end;
    if(sText = 'Vis. Horizon Limit') then begin Result := 'Vis. Horizonthöhe'; exit; end;
    if(sText = 'Welcome to Albireo!') then begin Result := 'Willkommen bei Albireo!'; exit; end;
    if(sText = 'Year of Birth') then begin Result := 'Geburtsjahr'; exit; end;
    if(sText = 'Your firstname') then begin Result := 'Dein Vorname'; exit; end;
    if(sText = 'Your surname') then begin Result := 'Dein Zuname'; exit; end;

    // Longer text phrases
    if(sText = 'Administration and analysis of my telecopes') then begin Result := 'Verwaltung und Analyse meiner Teleskope'; exit; end;
    if(sText = 'Manual telescope alignment to a celestial object') then begin Result := 'Manuelle Ausrichtung des Teleskops auf ein Himmelsobjekt'; exit; end;
    if(sText = 'Real-Time Listings of astronomical objects') then begin Result := 'Tabellarische Auflistung astronomischer Objekte in Echtzeit'; exit; end;
    if(sText = 'Show star structure') then begin Result := 'Zeige Sternaufbau'; exit; end;
    if(sText = 'Visualisation of the starry sky') then begin Result := 'Darstellung des aktuellen Sternenhimmels'; exit; end;
    if(sText = 'Welcome Page & Overview') then begin Result := 'Willkommens- und Übersichtsseite'; exit; end;

  end;

end;

procedure IniText(Form: TForm; sLANG_ID: string);
var
  i: Integer;
  sCompName, sText, sTransText: string;
begin
  try

  sText := Form.Caption;
  if(Trim(sText) <> '') then
  begin
    sTransText := TranslateTextTo(sLANG_ID,sText);
    if(Trim(sTransText) <> '') then
      Form.Caption := sTransText;
  end;

  for i:=0 to Form.ComponentCount-1 do
  begin
    sCompName := Uppercase(Trim(Form.Components[i].Name));

    if(LeftStr(sCompName,6) = 'F__') then
    begin
      sText := (Form.Components[i] as TMenuItem).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TMenuItem).Caption := sTransText;
      end;
    end;

    if(LeftStr(sCompName,5) = 'GBX__') then
    begin
      sText := (Form.Components[i] as TGroupBox).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
        begin
          (Form.Components[i] as TGroupBox).Caption := sTransText;
        end;
      end;
      sText := (Form.Components[i] as TGroupBox).Hint;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
        begin
          (Form.Components[i] as TGroupBox).Hint := sTransText;
        end;
      end;
    end;

    if(LeftStr(sCompName,3) = 'P__') then
    begin
      sText := (Form.Components[i] as TPanel).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TPanel).Caption := sTransText;
      end;
      sText := (Form.Components[i] as TPanel).Hint;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TPanel).Hint := sTransText;
      end;
    end;

    if(LeftStr(sCompName,6) = 'MENU__') then
    begin
      sText := (Form.Components[i] as TMenuItem).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TMenuItem).Caption := sTransText;
      end;
    end;

    if(LeftStr(sCompName,7) = 'PMENU__') then
    begin
      if(Uppercase(Form.Components[i].ClassName) <> 'TPOPUPMENU') then
      begin
        sText := (Form.Components[i] as TMenuItem).Caption;
        if(Trim(sText) <> '') then
        begin
          sTransText := TranslateTextTo(sLANG_ID,sText);
          if(Trim(sTransText) <> '') then
            (Form.Components[i] as TMenuItem).Caption := sTransText;
        end;
      end;
    end;

    if(LeftStr(sCompName,3) = 'B__') then
    begin
      sText := (Form.Components[i] as TButton).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TButton).Caption := sTransText;
      end;
      sText := (Form.Components[i] as TButton).Hint;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TButton).Hint := sTransText;
      end;
    end;

    if(LeftStr(sCompName,5) = 'TGB__') then
    begin
      sText := (Form.Components[i] as TToggleBox).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TToggleBox).Caption := sTransText;
      end;
      sText := (Form.Components[i] as TToggleBox).Hint;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TToggleBox).Hint := sTransText;
      end;
    end;

    if(LeftStr(sCompName,4) = 'BT__') then
    begin
      sText := (Form.Components[i] as TBitBtn).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TBitBtn).Caption := sTransText;
      end;
    end;

    if(LeftStr(sCompName,3) = 'L__') then
    begin
      sText := (Form.Components[i] as TLabel).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TLabel).Caption := sTransText;
      end;
      sText := (Form.Components[i] as TLabel).Hint;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TLabel).Hint := sTransText;
      end;
    end;

    if(LeftStr(sCompName,5) = 'CBX__') then
    begin
      sText := (Form.Components[i] as TCheckBox).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TCheckBox).Caption := sTransText;
      end;
    end;

    if(LeftStr(sCompName,4) = 'RB__') then
    begin
      sText := (Form.Components[i] as TRadioButton).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TRadioButton).Caption := sTransText;
      end;
    end;

    if(LeftStr(sCompName,4) = 'TS__') then
    begin
      sText := (Form.Components[i] as TTabSheet).Caption;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TTabSheet).Caption := sTransText;
      end;
    end;

    if(LeftStr(sCompName,5) = 'IMG__') then
    begin
      sText := (Form.Components[i] as TImage).Hint;
      if(Trim(sText) <> '') then
      begin
        sTransText := TranslateTextTo(sLANG_ID,sText);
        if(Trim(sTransText) <> '') then
          (Form.Components[i] as TImage).Hint := sTransText;
      end;
    end;
  end;
  except
    on e: Exception do
    begin
      ShowMessage('IniText-Err for comp: ' + sCompName + ', Err: ' + e.Message);
    end;
  end;
end;

end.

