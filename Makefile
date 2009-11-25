libs = libs/mysql-connector-java-3.0.17-ga-bin.jar
all: scalafiles

scalafiles:
	fsc -unchecked -deprecation -classpath ${libs} -d classes `find . -name "*.scala"`

run-tests: scalafiles
	scala -cp classes:${libs} ChessServer.tests.Tester config-run.xml

run-server: scalafiles
	scala -cp classes:${libs} ChessServer.Main config-run.xml

only-run-server:
	scala -cp classes:${libs} ChessServer.Main config-run.xml

run-server-dev:
	scala -cp classes:${libs} ChessServer.Main config-run-dev.xml

run-client: scalafiles
	scala -cp classes:${libs} ChessServer.MainClient

run-stress-client: scalafiles
	scala -cp classes:${libs} ChessServer.StressClient
