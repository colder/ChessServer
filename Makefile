all: scalafiles

scalafiles:
	fsc -unchecked -deprecation -d classes `find . -name "*.scala"`

test: scalafiles
	scala -cp classes ChessServer.Main

run: scalafiles
	scala -cp classes ChessServer.Main
