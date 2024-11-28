SELECT * FROM miljodata.aarh3;

-- Viser strukturen af tabellen 'aarh3'
DESCRIBE miljodata.aarh3;

-- Viser SQL-kommandoen, der oprettede tabellen
SHOW CREATE TABLE miljodata.aarh3;

-- Tilføjer en ny kolonne til tabellen
ALTER TABLE miljodata.aarh3 ADD COLUMN `Målt_datetime` DATETIME;

-- Deaktiverer SQL_SAFE_UPDATES for at tillade opdatering uden WHERE
SET SQL_SAFE_UPDATES = 0;

-- Konverterer datoformatet i 'Målt (starttid)' til 'DATETIME' og gemmer det i 'Målt_datetime'
UPDATE miljodata.aarh3
SET `Målt_datetime` = STR_TO_DATE(`Målt (starttid)`, '%d-%m-%Y %H:%i');

-- Reaktiverer SQL_SAFE_UPDATES
SET SQL_SAFE_UPDATES = 1;

-- Fjerner den gamle kolonne og omdøber den nye kolonne
ALTER TABLE miljodata.aarh3 DROP COLUMN `Målt (starttid)`;
ALTER TABLE miljodata.aarh3 CHANGE COLUMN `Målt_datetime` `Målt (starttid)` DATETIME;

-- Henter data sorteret efter den nyeste dato først
SELECT * FROM miljodata.aarh3 ORDER BY `Målt (starttid)` DESC LIMIT 10;

-- Flytter 'Målt (starttid)' til første position
ALTER TABLE miljodata.aarh3
MODIFY COLUMN `Målt (starttid)` DATETIME FIRST;

-- Kør denne funktion for at få databasen sorteret, gør hver gang
SELECT * FROM miljodata.aarh3
ORDER BY `Målt (starttid)` DESC;