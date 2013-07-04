compile:
	@/export/apps/sbt-0.12.1/bin/sbt compile

run: compile
	-@rm core.* 2> /dev/null
	-@rm slda.* 2> /dev/null
	@qsub coe-qsub-job
