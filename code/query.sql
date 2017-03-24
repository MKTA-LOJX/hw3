use charity2;

CREATE TABLE contacts_processed (
  id INT UNSIGNED NOT NULL,
  zip_code    VARCHAR(5),
  code_geo VARCHAR(5),
  town_clean  VARCHAR(60),
  nb_contacts	INT,
  PRIMARY KEY (id),
  KEY twn (town_clean)
  )
ENGINE = MyISAM;

INSERT INTO contacts_processed
SELECT id, zip_code, code_geo, town_clean, count(*) as nb_contacts
FROM contacts
GROUP BY code_geo;

UPDATE contacts_processed
SET code_geo = '13055' 
WHERE town_clean = 'MARSEILLE';

UPDATE contacts_processed
SET code_geo = '75056' 
WHERE town_clean = 'PARIS';

UPDATE contacts_processed
SET code_geo = '69123' 
WHERE town_clean = 'LYON';