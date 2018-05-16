PRAGMA foreign_keys = ON; -- enforce foreign key constraint

-- make the main tables
CREATE TABLE studies (id INTEGER PRIMARY KEY,
	study_title TEXT unique, contact_person TEXT, year_started date,
	date_added date);

CREATE TABLE tasks  (id INTEGER PRIMARY KEY, task_name TEXT unique,
	description TEXT, date_added date);

CREATE TABLE participants (id INTEGER PRIMARY KEY,
	first_name TEXT, last_name TEXT, date_of_birth DATE, date_added date);


-- make the mapping tables
CREATE TABLE study_task (study_reference INTEGER NOT NULL references study(id),
                         task_reference INTEGER NOT NULL references task(id),
                         PRIMARY KEY (study_reference,task_reference));

CREATE TABLE part_study (part_reference INTEGER NOT NULL references participants(id),
                         study_reference INTEGER NOT NULL references study(id),
                         PRIMARY KEY (part_reference, study_reference));
