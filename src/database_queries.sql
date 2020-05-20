/* query to determine the number of 'submitted' and 'started by never submitted' preprint in 2020 by month and provider */

WITH pp_finished AS (SELECT id, 
						provider_id, created, 
						machine_state, 
						CASE WHEN machine_state = 'initial' THEN 'not_finished' ELSE 'finished' END AS pp_submit_finished
					FROM osf_preprint
					WHERE osf_preprint.created >= '2020-01-01' AND osf_preprint.created < '2020-04-01')


SELECT COUNT(pp_finished.id) num_pps, 
		date_trunc('month', pp_finished.created) AS month_created, 
		osf_abstractprovider._id AS provider, 
		pp_submit_finished
	FROM pp_finished
	LEFT JOIN osf_abstractprovider
	ON pp_finished.provider_id = osf_abstractprovider.id
	GROUP BY pp_submit_finished, osf_abstractprovider._id, date_trunc('month', pp_finished.created)