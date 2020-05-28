/* query to determine the number of 'submitted' and 'started by never submitted' preprint in 2020 by month and provider */

/*set up time windows and 'finished' state categorizations */
WITH pp_finished AS (SELECT id, 
						provider_id, created, 
						machine_state, 
						CASE WHEN machine_state = 'initial' THEN 'not_finished' ELSE 'finished' END AS pp_submit_finished,
						CASE WHEN osf_preprint.created < '2020-04-21 20:37:53.449468+00:00' THEN 'pre_exp' ELSE 'during_exp' END AS timeframe
					FROM osf_preprint
					WHERE osf_preprint.created >= '2020-03-17 20:37:53.449468+00:00' AND osf_preprint.created <= '2020-05-26 20:37:53.449468+00:00' AND 
							provider_id != 7)

/* count up preprints by finished state and provider during/before experiment */
SELECT COUNT(pp_finished.id) num_pps, 
		timeframe, 
		osf_abstractprovider._id AS provider, 
		pp_submit_finished
	FROM pp_finished
	LEFT JOIN osf_abstractprovider
	ON pp_finished.provider_id = osf_abstractprovider.id
	GROUP BY pp_submit_finished, osf_abstractprovider._id, timeframe


/* query to get number of started, but not completed preprints, that got to each stage of the upload process */

/* set up categorization of preprints into time windows */
WITH pp_timing AS (SELECT id,
						created,
						CASE WHEN osf_preprint.created < '2020-04-21 20:37:53.449468+00:00' THEN 'pre_exp' ELSE 'during_exp' END AS timeframe
					FROM osf_preprint
					WHERE osf_preprint.created >= '2020-03-17 20:37:53.449468+00:00' AND osf_preprint.created <= '2020-05-26 20:37:53.449468+00:00' AND 
							provider_id != 7 AND machine_state = 'initial')

/* count up the number of preprints with each action type before/during the experiment' */
SELECT COUNT(pp_timing.id) AS num_pps, action, timeframe
	FROM pp_timing
	LEFT JOIN (SELECT preprint_id, action, MIN(created)
				FROM osf_preprintlog
				WHERE created >= '2020-03-17 20:37:53.449468+00:00'
				GROUP BY preprint_id, action) AS pp_actions
	ON pp_timing.id = pp.preprint_id
	GROUP BY action, timeframe