#$ -cwd
#$ -j y
#$ -V
#$ -l num_proc=1,mem_free=3G,h_vmem=3G,h_rt=03:00:00
#$ -N "slda"
#cd $BUILD && src/build.sh log-linear ~aclemmer/Jerboa /home/hltcoe/aclemmer/Jerboa/analytic/english-gender/build/twitterMITRE prob
/export/apps/sbt-0.12.1/bin/sbt run
