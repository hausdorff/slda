compile:
	@/export/apps/sbt-0.12.1/bin/sbt compile

cleanup:
	@rm core.*
	@rm slda.o*

run: compile cleanup
	./run_coe.sh
