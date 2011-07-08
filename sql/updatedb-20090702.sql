
--- Switching the repository path/kind from ocsforge_tasks to ocsforge_right_areas

ALTER TABLE ocsforge_tasks DROP COLUMN repository_kind ;
ALTER TABLE ocsforge_tasks DROP COLUMN repository_path ;

ALTER TABLE ocsforge_right_areas
  ADD COLUMN repository_kind text ;
ALTER TABLE ocsforge_right_areas
  ADD COLUMN repository_path text ;
ALTER TABLE ocsforge_right_areas
  ADD COLUMN wiki integer DEFAULT 1 references wikis(id) NOT NULL;

--- Adding a "deleted" flag to tasks

ALTER TABLE ocsforge_tasks
  ADD COLUMN deleted boolean DEFAULT false NOT NULL;
ALTER TABLE ocsforge_tasks_history
  ADD COLUMN deleted boolean DEFAULT false NOT NULL;

--- Adding a "area root" to task

ALTER TABLE ocsforge_tasks
  ADD COLUMN area_root boolean NOT NULL;


--- Adding a container column to right zones

ALTER TABLE ocsforge_right_areas
  ADD COLUMN wiki_container integer REFERENCES wikiboxindex(uid) ;

--- Deleting the area_inheritance field

ALTER TABLE ocsforge_right_areas
  DROP COLUMN inheritance ;
