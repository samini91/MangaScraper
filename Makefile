all: Build Run

Build:
	stack build --allow-different-user --ghc-options="-Wall -fwarn-incomplete-uni-patterns"
Run:
	stack exec MangaScraper-exe --allow-different-user
StartSeleniumServer:
	java -jar selenium-server-standalone-2.53.1.jar




