-- Adding a "wikibox" field to area --
ALTER TABLE ocsforge_right_areas
      ADD COLUMN sources_container int NOT NULL;