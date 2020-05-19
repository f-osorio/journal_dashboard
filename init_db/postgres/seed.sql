DROP TABLE IF EXISTS bibliometrics;
CREATE TABLE bibliometrics(
    handle varchar(500) NOT NULL PRIMARY KEY,
    year int DEFAULT NULL,
    cites int DEFAULT NULL,
    impact_factor numeric DEFAULT 0,
    if_5 numeric DEFAULT 0,
    documents_published int DEFAULT NULL,
    h_index int DEFAULT NULL,
    type varchar(500) DEFAULT NULL,
    issn1 varchar(500) DEFAULT NULL,
    issn2 varchar(500) DEFAULT NULL,
    type2 varchar(500) DEFAULT NULL,
    year3 int DEFAULT NULL,
    scimago_id bigint DEFAULT NULL,
    sjr numeric DEFAULT 0,
    type4 varchar(500) DEFAULT NULL,
    year5 int DEFAULT NULL,
    journ_qual varchar(2) DEFAULT NULL,
    type6 varchar(500) DEFAULT NULL,
    year7 int DEFAULT NULL,
    bwl varchar(2) DEFAULT NULL,
    type8 varchar(500) DEFAULT NULL,
    year9 int DEFAULT NULL,
    vwl varchar(2) DEFAULT NULL,
    journal_name varchar(500) DEFAULT NULL
);

DROP TABLE IF EXISTS altmetrics;
CREATE TABLE altmetrics (
  id serial PRIMARY KEY,
  altmetric_score int DEFAULT NULL,
  title varchar(500) DEFAULT NULL,
  journal_name varchar(500) DEFAULT NULL,
  print_issn varchar(500) DEFAULT NULL,
  online_issn varchar(500) DEFAULT NULL,
  subjects varchar(500) DEFAULT NULL,
  affiliation varchar(500) DEFAULT NULL,
  publication_date varchar(100) DEFAULT NULL,
  doi varchar(500) DEFAULT NULL,
  news int DEFAULT NULL,
  blog int DEFAULT NULL,
  policy int DEFAULT NULL,
  patent int DEFAULT NULL,
  twitter int DEFAULT NULL,
  peer_review int DEFAULT NULL,
  weibo int DEFAULT NULL,
  facebook int DEFAULT NULL,
  wikipedia int DEFAULT NULL,
  google int DEFAULT NULL,
  linkedIn int DEFAULT NULL,
  reddit int DEFAULT NULL,
  pinterest int DEFAULT NULL,
  f1000 int DEFAULT NULL,
  qa int DEFAULT NULL,
  videos int DEFAULT NULL,
  syllabi int DEFAULT NULL,
  mendeley int DEFAULT NULL,
  dimensions_citations int DEFAULT NULL,
  details_page varchar(500) DEFAULT NULL,
  badge_url varchar(500) DEFAULT NULL
);


DROP TABLE IF EXISTS mendeley_doi;
CREATE TABLE mendeley_doi (
  id serial PRIMARY KEY,
  doi varchar(50) NOT NULL DEFAULT '',
  count int NOT NULL DEFAULT '0',
  year varchar(50) NOT NULL DEFAULT '',
  issn varchar(500) DEFAULT NULL,
  title varchar(1000) DEFAULT NULL,
  publisher varchar(200) DEFAULT NULL,
  discipline varchar(350) DEFAULT NULL,
  electronic varchar(150) DEFAULT NULL
);

DROP TABLE IF EXISTS mendeley_doi_simp;
CREATE TABLE mendeley_doi_simp (
  issn varchar(50) PRIMARY KEY,
  instances int NOT NULL DEFAULT 0,
  id varchar(50),
  count int NOT NULL DEFAULT 0
);

DROP TABLE IF EXISTS mendeley_country;
CREATE TABLE mendeley_country (
  id serial PRIMARY KEY,
  id_doi varchar(50) NOT NULL DEFAULT '',
  country varchar(50),
  count int NOT NULL DEFAULT '0',
  code varchar(4)
);

DROP TABLE IF EXISTS mendeley_status;
CREATE TABLE mendeley_status (
  id serial PRIMARY KEY,
  id_doi varchar(50) NOT NULL DEFAULT '',
  status varchar(50),
  count int NOT NULL DEFAULT '0'
);

DROP TABLE IF EXISTS mendeley_discipline;
CREATE TABLE mendeley_discipline (
  id serial PRIMARY KEY,
  id_doi varchar(50) NOT NULL DEFAULT '',
  category varchar(150) NOT NULL DEFAULT '',
  discipline varchar(50) NOT NULL DEFAULT '',
  count int NOT NULL DEFAULT '0'
);


--
-- Load Data in Tables
--
COPY bibliometrics (handle, year, cites, impact_factor, if_5, documents_published, h_index, type, issn1, issn2, type2, year3, scimago_id, sjr, type4, year5, journ_qual, type6, year7, bwl, type8, year9, vwl, journal_name)
from '/srv/conda/data/biblio_data.csv' (FORMAT CSV, DELIMITER(';'), HEADER)

\COPY mendeley_doi from '/srv/conda/data/mendeley_doi.csv' (FORMAT CSV, DELIMITER(';'), HEADER, QUOTE('"'))
\COPY altmetrics from '/srv/conda/data/cleaned_altmetrics.csv' (FORMAT CSV, DELIMITER(';'), HEADER, QUOTE('"'))
\COPY mendeley_doi_simp from '/srv/conda/data/simplified_mend_doi.csv' (FORMAT CSV, DELIMITER(';'), HEADER, QUOTE('"'))
\COPY mendeley_country from '/srv/conda/data/mendeley_country.csv' (FORMAT CSV, DELIMITER(';'), HEADER, QUOTE('"'))
\COPY mendeley_status from '/srv/conda/data/mendeley_status.csv' (FORMAT CSV, DELIMITER(';'), HEADER, QUOTE('"'))
\COPY mendeley_discipline from '/srv/conda/data/mendeley_discipline.csv' (FORMAT CSV, DELIMITER(';'), HEADER, QUOTE('"'))

