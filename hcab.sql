-- Opretter databasen 'miljodata'
CREATE DATABASE miljodata;

-- Henter alle data fra tabellen 'hcab'
SELECT * FROM hcab;

-- Viser strukturen af tabellen 'hcab'
DESCRIBE hcab;

-- Viser SQL-kommandoen, der oprettede tabellen
SHOW CREATE TABLE hcab;
SELECT * FROM miljodata.hcab;

-- Tilføjer en ny kolonne til tabellen
ALTER TABLE hcab ADD COLUMN `Målt_datetime` DATETIME;

-- Deaktiverer SQL_SAFE_UPDATES for at tillade opdatering uden WHERE
SET SQL_SAFE_UPDATES = 0;

-- Konverterer datoformatet i 'Målt (starttid)' til 'DATETIME' og gemmer det i 'Målt_datetime'
UPDATE hcab
SET `Målt_datetime` = STR_TO_DATE(`Målt (starttid)`, '%d-%m-%Y %H:%i');

-- Reaktiverer SQL_SAFE_UPDATES
SET SQL_SAFE_UPDATES = 1;

-- Fjerner den gamle kolonne og omdøber den nye kolonne
ALTER TABLE hcab DROP COLUMN `Målt (starttid)`;
ALTER TABLE hcab CHANGE COLUMN `Målt_datetime` `Målt (starttid)` DATETIME;

-- Henter data sorteret efter den nyeste dato først
SELECT * FROM hcab ORDER BY `Målt (starttid)` DESC LIMIT 10;

-- Flytter 'Målt (starttid)' til første position
ALTER TABLE hcab
MODIFY COLUMN `Målt (starttid)` DATETIME FIRST;

-- Kør denne funktion for at få databasen sorteret, gør hver gang
SELECT * FROM hcab
ORDER BY `Målt (starttid)` DESC;



