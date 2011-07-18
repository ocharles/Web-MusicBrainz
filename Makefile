RUN = runhaskell
GHC = ghc
GHC_FLAGS = --make

spec: specbin
	./spec

# runhaskell is busted on Arch right now
runspec: Audio/MusicBrainz/Testing/Main.hs
	$(RUN) Audio/MusicBrainz/Testing/Main.hs

specbin: Audio/MusicBrainz/Testing/*.hs
	$(GHC) $(GHC_FLAGS) -o spec Audio/MusicBrainz/Testing/Main.hs

clean:
	rm -rf **/*.hi **/*.o spec
