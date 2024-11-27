-- Opret tabel til forhandlerdata
CREATE TABLE forhandler (
forhandlernavn VARCHAR(30),
adresse VARCHAR(50),
cvr INT,
PRIMARY KEY (cvr)
);

-- Opret tabel til bildata
CREATE TABLE tesla (
  model VARCHAR(50),
  description TEXT,
  link VARCHAR(150),
  carid INT NOT NULL,
  PRIMARY KEY (carid),
  rækkevide_km SMALLINT,
  årgang VARCHAR(10),
  kørtekm INT,
  cvr INT,
  FOREIGN KEY (cvr) REFERENCES forhandler(cvr)
);

-- Opret tabel til simuleret data
CREATE TABLE tesla_var (
  priceID INT AUTO_INCREMENT,
  PRIMARY KEY (priceID),
  carid INT NOT NULL,
  price INT,
  FOREIGN KEY (carid) REFERENCES tesla(carid),
  scrapedate DATETIME,
  sold BOOLEAN DEFAULT FALSE
);