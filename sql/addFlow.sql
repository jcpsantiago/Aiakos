-- 1 add study
insert into study (study_title, contact_person) values ('soOxy', 'Joao Santiago');

-- 2 add study/task mapping (repeat for each task)
insert into study_task values ((select id from study where study_title = 'soOxy'),
                               (Select id from task where task_name = 'OGTT'));

-- 3 add participant
insert into participants (first_name, last_name) values ('Ines', 'Costa');

-- 4 add participant/study mapping
insert into part_study values ((select last_insert_rowid()),
                               (select id from study where study_title = 'InsuSO'));
