use charity2;

# Create new table for preprocessing
#DROP TABLE contacts_processed;
CREATE TABLE contacts_processed (
  id INT UNSIGNED NOT NULL,
  zip_code    VARCHAR(5),
  code_geo VARCHAR(5),
  town_clean  VARCHAR(60),
  PRIMARY KEY (id),
  KEY twn (town_clean)
  )
ENGINE = MyISAM;

# Insert data from contacts
INSERT INTO contacts_processed
SELECT id, zip_code, code_geo, town_clean
FROM contacts;

# Correct town_clean with CEDEX
SET SQL_SAFE_UPDATES=0;
UPDATE contacts_processed
SET town_clean = SUBSTRING(town_clean, 1, INSTR(town_clean, '-CEDEX')-1)
WHERE INSTR(town_clean, '-CEDEX') > 0;
SET SQL_SAFE_UPDATES=1;

# Correct code geo for Paris, Lyon and Marseille
UPDATE contacts_processed
SET code_geo = '13055' 
WHERE town_clean = 'MARSEILLE';

UPDATE contacts_processed
SET code_geo = '75056' 
WHERE town_clean = 'PARIS';

UPDATE contacts_processed
SET code_geo = '69123' 
WHERE town_clean = 'LYON';

# Update missing code_geo with a correspondance table from INSEE
#DROP TABLE corr_insee;
CREATE TABLE corr_insee (
  id INT UNSIGNED auto_increment NOT NULL,
  town  VARCHAR(60),
  code_geo VARCHAR(5),
  PRIMARY KEY (id)
  )
ENGINE = MyISAM;

LOAD DATA LOCAL INFILE 'C:/ProgramData/MySQL/MySQL Server 5.7/Data/correspondance-code-insee-code-postal.csv' INTO TABLE corr_insee
FIELDS TERMINATED BY ';' 
LINES TERMINATED BY '\n'
IGNORE 1 LINES 
(@col1,@col2,@col3,@col4,@col5,@col6,@col7,@col8,@col9,@col10,@col11,@col12,@col13,@col14,@col15,@col16,@col17) set town=@col3,code_geo=@col1;

SET SQL_SAFE_UPDATES=0;
UPDATE contacts_processed
INNER JOIN corr_insee 
ON contacts_processed.town_clean = corr_insee.town
SET contacts_processed.code_geo = corr_insee.code_geo
WHERE contacts_processed.code_geo IS NULL;
SET SQL_SAFE_UPDATES=1;
