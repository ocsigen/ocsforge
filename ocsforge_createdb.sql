--- Ocsimore
--- Copyright (C) 2005
--- Laboratoire PPS - Université Paris Diderot - CNRS
---
--- This program is free software; you can redistribute it and/or modify
--- it under the terms of the GNU General Public License as published by
--- the Free Software Foundation; either version 2 of the License, or
--- (at your option) any later version.
---
--- This program is distributed in the hope that it will be useful,
--- but WITHOUT ANY WARRANTY; without even the implied warranty of
--- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--- GNU General Public License for more details.
---
--- You should have received a copy of the GNU General Public License
--- along with this program; if not, write to the Free Software
--- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
---


CREATE TABLE ocsforge_right_areas (
	id serial NOT NULL primary key,
	forum_id integer NOT NULL REFERENCES forums(id),
	version text NOT NULL,
	inheritance integer NOT NULL REFERENCES ocsforge_right_areas(id)
);

ALTER TABLE ocsforge_right_areas OWNER TO ocsimore;

CREATE TABLE ocsforge_task_kinds (
	right_area integer NOT NULL REFERENCES ocsforge_right_areas(id),
	kind text NOT NULL
);

ALTER TABLE ocsforge_task_kinds OWNER TO ocsimore;

CREATE TABLE ocsforge_tasks (

    id serial NOT NULL primary key,
    parent integer NOT NULL REFERENCES ocsforge_tasks(id),

    message integer NOT NULL REFERENCES forums_messages(id),
    
    edit_author integer NOT NULL REFERENCES users(id),
    edit_time timestamp DEFAULT (now())::timestamp NOT NULL,
    edit_version text NOT NULL,

    length interval,
    progress integer DEFAULT NULL CHECK (progress >= 0 AND 100 >= progress), 
    importance integer DEFAULT NULL CHECK (importance >= 0 AND 100 >= importance), 
    deadline_time timestamp,
    deadline_version text,
    kind text,

    area integer NOT NULL REFERENCES ocsforge_right_areas(id),

    repository_kind text,
    repository_path text, 
    
    tree_min integer DEFAULT 0 NOT NULL,
    tree_max integer DEFAULT 1 NOT NULL
);


ALTER TABLE public.ocsforge_tasks OWNER TO ocsimore;

CREATE TABLE ocsforge_tasks_history (

    id integer NOT NULL,
    parent integer NOT NULL REFERENCES ocsforge_tasks(id),

    edit_author integer NOT NULL REFERENCES users(id),
    edit_time timestamp DEFAULT (now())::timestamp NOT NULL,
    edit_version text NOT NULL,

    length interval,
    progress integer DEFAULT NULL CHECK (progress >=0 AND progress <=100),
    importance integer DEFAULT NULL CHECK (importance >= 0 AND importance <= 100),
    deadline_time timestamp,
    deadline_version text,
    kind text,

    area integer NOT NULL REFERENCES ocsforge_right_areas(id)
);

ALTER TABLE ocsforge_tasks_history OWNER TO ocsimore;

