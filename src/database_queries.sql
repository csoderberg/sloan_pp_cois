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

/* query to get number of started, but not completed preprints, that got to each stage of the upload process */
SELECT COUNT(osf_preprint.id) AS num_pps, action
	FROM osf_preprint
	LEFT JOIN (SELECT preprint_id, action, MIN(created)
				FROM osf_preprintlog
				WHERE created >= '2020-03-01'
				GROUP BY preprint_id, action) AS pp_actions
	ON osf_preprint.id = pp_actions.preprint_id
	WHERE created >= '2020-03-01' AND machine_state = 'initial'
	GROUP BY action