HC      = ghc
HC_OPTS = -rtsopts=all -prof -auto-all --make

default: h99

%: %.hs
	$(HC) $(HC_OPTS) $<
	./$@ +RTS -hc -RTS > /dev/null
	hp2ps -c $@.hp
	open $@.ps

clean:
	rm h99
