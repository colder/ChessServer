all: scalafiles

scalafiles:
	fsc -unchecked -deprecation -d classes `find . -name "*.scala"`

test: scalafiles
	scala -cp classes ChessServer.Main

run-server: scalafiles
	scala -cp classes ChessServer.Main

run-client: scalafiles
	scala -cp classes ChessServer.MainClient
