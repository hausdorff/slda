compile:
	@/export/apps/sbt-0.12.1/bin/sbt compile

cleanup:
	$(@./cleanup)

run: compile cleanup
	@echo "RUNNING PROJECT"
	qsub coe-qsub-job.sh
