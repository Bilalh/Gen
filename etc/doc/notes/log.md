
#Tue 20 May 2014

The aim of this experiment is to test the effects of the :

- radius size,
- time per model
- timing method


Essences:  (same as paper)
PPP
Kanpsack
Langford
SGP
PPP
Warehouse
BACP

Methods:
Markov
Undirected

Max time per model:
Using the closest powers of two of {30s  60s 10m 30m 1h} i.e
32s  64s  512s 2048s  4096s

Radii for Markov:
5% 10% 20% 40%

Timeout used:
Always used the same time per model.
Exponential growth for the first 5 races.

Races
60


Exponential growth

For 32s the progression would be:

2
4
8
16
32

We use floor on the values if they are non integers


#Wed 28 May 2014

Running compact on ig1 to see if the specs in the essence catalog work and how long these take.

Running the new version of --sample which should much more efficient (this work by shuffling the list instead of picking indexes) on various problem classes (including classes were there were too many models) which seems to work quite well. 

Check the models to seem how biased the sampling is.

Use the models to do the improvements 


