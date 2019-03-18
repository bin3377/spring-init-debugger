args="$*"
HERE=`dirname $0`
MODULE_HOME=`cd $HERE/..; pwd`
cd $MODULE_HOME

export MAVEN_OPTS="$JVM_ARGS"
mvn -e compile exec:java -Dexec.mainClass=com.sumologic.springdebugger.Main -Dexec.args="$args"