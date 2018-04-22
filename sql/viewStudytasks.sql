-- list studies and respective tasks
SELECT study_title as 'Study title', group_concat(task_name, ', ') AS 'Tasks'
FROM study_task
JOIN study ON study_reference = study.id
JOIN task ON task_reference = task.id
GROUP BY study.id
ORDER by study_title;
