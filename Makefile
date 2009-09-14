libs = libs/mysql-connector-java-3.0.17-ga-bin.jar
all: scalafiles

scalafiles:
	fsc -unchecked -deprecation -classpath ${libs} -d classes `find . -name "*.scala"`

run-server: scalafiles
	scala -cp classes:${libs} ChessServer.Main config.xml

run-client: scalafiles
	scala -cp classes:${libs} ChessServer.MainClient
