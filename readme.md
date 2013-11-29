Methods for generating params that are discriminating i.e. can be used to select the best models


`mchain` --> Use markov chains to generate params
`smac`   --> Use the smac tool to genrate params



#mchain

example run from the instancegen-models/prob028-Bibd

````
../../instancegen/mchain/chain_main.py time 21600  --chain_length=200 --select_radius=15 --influence_radius=15 --model_timeout=1200  --essence=prob028-Bibd.essence
````