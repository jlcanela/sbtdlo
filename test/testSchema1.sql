CREATE TABLE attributes (id INTEGER PRIMARY KEY, name TEXT, min NUMERIC, max NUMERIC, category INTEGER);
CREATE TABLE database_version (version_number INTEGER);
CREATE TABLE data (id INTEGER PRIMARY KEY, letter TEXT );
CREATE TABLE events (start_day INTEGER, duration INTEGER );
CREATE INDEX idx_index ON attributes (don't care);
