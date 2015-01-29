# btspeech
Some text work with German parliamentary speech  

Extract parliamentary speech from raw session protocols of the German Parliament (Bundestag). Split it up by Member of Parliament (MP) and legislative period (for now 15 - 17) and match it to meta data about the MPs (*speech_extract.py*).
*splitter.py* collects all speeches given by one MP in a session and saves them in a separate file. *cleaner.py* stems and cleans the text for statistical analysis. 

I ran a topic model (bayesian latent class model) on the cleaned files (which is not up yet but will be soon). *join_by_topic.py* takes the output from this topic model *topicdocs.txt* and joins text files by a MP and topic.

This is old work of the young(er) inexperienced me and I'm in the process of revising and improving it.