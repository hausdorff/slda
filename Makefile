compile:
	@/export/apps/sbt-0.12.1/bin/sbt compile

run: compile
	@./cleanup
	@qsub coe-qsub-job
