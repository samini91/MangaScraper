all: Build Run

Build:
	stack build
Run:
	stack exec MangaScraper-exe
