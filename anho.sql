-- Viser alle data i 'anho' tabellen
SELECT * FROM miljodata.anho;

-- Viser strukturen af tabellen 'anho'
DESCRIBE anho;

-- Viser SQL-kommandoen, der oprettede tabellen 'anho'
SHOW CREATE TABLE anho;

-- Tilføjer en ny kolonne til tabellen 'anho'
ALTER TABLE anho ADD COLUMN `Målt_datetime` DATETIME;

-- Deaktiverer SQL_SAFE_UPDATES for at tillade opdatering uden WHERE
SET SQL_SAFE_UPDATES = 0;

-- Konverterer datoformatet i 'Målt (starttid)' til 'DATETIME' og gemmer det i 'Målt_datetime'
UPDATE anho
SET `Målt_datetime` = STR_TO_DATE(`Målt (starttid)`, '%d-%m-%Y %H:%i');

-- Reaktiverer SQL_SAFE_UPDATES
SET SQL_SAFE_UPDATES = 1;

-- Fjerner den gamle kolonne og omdøber den nye kolonne
ALTER TABLE anho DROP COLUMN `Målt (starttid)`;
ALTER TABLE anho CHANGE COLUMN `Målt_datetime` `Målt (starttid)` DATETIME;

-- Henter data sorteret efter den nyeste dato først
SELECT * FROM anho ORDER BY `Målt (starttid)` DESC LIMIT 10;

-- Flytter 'Målt (starttid)' til første position
ALTER TABLE anho
MODIFY COLUMN `Målt (starttid)` DATETIME FIRST;

-- Kør denne funktion for at få databasen sorteret hver gang
SELECT * FROM anho
ORDER BY `Målt (starttid)` DESC;