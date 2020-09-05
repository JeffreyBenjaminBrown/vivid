export SC_JACK_DEFAULT_INPUTS="system"
export SC_JACK_DEFAULT_OUTPUTS="system"
scsynth -u 57110 \
	-m 1000000 # Assigns 1 GB of memory it.
