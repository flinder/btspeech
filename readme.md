# btspeech
Some text work with German parliamentary speech  

This is old work of the young(er) inexperienced me and I'm in the process of revising and improving it.

Contents:
- /data: all data, from raw to cleaned and splitted
- /text_prep: python scripts to prepare text data
- /topic_model: Files to fit Grimmer (2010) expressed agenda topic model
- /naive_bayes: Files to locate text in ideological space using the 'wordscores' algorithm) 

Files in '/text_prep':
Extract parliamentary speech from raw session protocols of the German Parliament (Bundestag). Split it up by Member of Parliament (MP) and legislative period (for now 15 - 17) and match it to meta data about the MPs (*speech_extract.py*).
*splitter.py* collects all speeches given by one MP in a session and saves them in a separate file. *cleaner.py* stems and cleans the text for statistical analysis. 
