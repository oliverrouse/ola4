-- Tester min join statement, for at se om jeg får de nye rækker ud
SELECT sim.*
FROM tesla_sim_stam sim
LEFT JOIN tesla t
ON sim.carid = t.carid
WHERE t.carid IS NULL;

-- Kører join statement med en insert funktion
INSERT INTO tesla (model, description, link, carid, rækkevide_km, årgang, kørtekm, cvr)
SELECT s.model, s.description, s.link, s.carid, s.rækkevide_km, s.årgang, s.kørtekm, s.cvr
FROM tesla_sim_stam s
LEFT JOIN tesla t
ON s.carid = t.carid
WHERE t.carid IS NULL;

-- Indsætter en ny række med den ændrede pris og carid
INSERT INTO tesla_var (carid, price, scrapedate)
SELECT s.carid, s.price, s.scrapedate
FROM tesla_sim_var s
JOIN tesla_var t
ON s.carid = t.carid
WHERE s.price != t.price;

-- Markerer solgte biler som TRUE i "sold"
UPDATE tesla_var t
LEFT JOIN tesla_sim_var s
ON t.carid = s.carid
SET t.sold = TRUE
WHERE s.carid IS NULL;

