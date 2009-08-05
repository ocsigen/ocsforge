

--- Getting rid of deadlines...

ALTER TABLE ocsforge_tasks DROP COLUMN deadline_version ;
ALTER TABLE ocsforge_tasks DROP COLUMN deadline_time ;

--- ...and Using separators instead

CREATE TABLE ocsforge_tasks_separators (

	id serial NOT NULL primary key,
	after integer NOT NULL REFERENCES ocsforge_tasks(id),
	content text NOT NULL

) ;
