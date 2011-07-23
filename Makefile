RUN = runhaskell
GHC = ghc
GHC_FLAGS = --make

spec: Audio/MusicBrainz/Testing/Main.hs
	$(RUN) Audio/MusicBrainz/Testing/Main.hs

clean:
	rm -rf **/*.hi **/*.o
