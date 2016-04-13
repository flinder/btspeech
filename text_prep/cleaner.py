# Cleans and stemms speech from splitted MP-speech documents. And saves it in 
# c_splitted/. 

import re
import os
from nltk.stem import SnowballStemmer

stemmer = SnowballStemmer("german")

maindir = 'C:/Users/samsung/Dropbox/Ma Thesis/Empirics/data/parl/'
filedir = maindir + 'splitted/'

nouns = False
sep_comp = False
stemming = True

if nouns:
    outdir = maindir + 'c_splitted_nouns/'
else:
    outdir = maindir + 'c_splitted/'
    
reg_arbeit = r'arbeitslos'

worddir = 'C:/Users/samsung/Dropbox/Ma Thesis/Empirics/analysis/Textprep/'

if not os.path.isdir(outdir):
    os.makedirs(outdir)

words0 = open(worddir+'names.txt').read()
words0 = re.sub('\n',' ',words0).lower()
words0 = words0.split(' ')
words0 = list(set(words0))

 words1 = open(worddir+'rmwords.txt').read()
words1 = set(words1.split('\n'))
words1 = [w.lower() for w in words1]
stopwords = words1 + words0

i = 0
while i < len(stopwords):
    w = stopwords[i]
    w = re.sub('\\xc3\\xa4','\xe4',w) # ae
    w = re.sub('\\xc3\\xb6','\xf6',w) # oe 
    w = re.sub('\\xc3\\xbc','\xfc',w) # ue
    w = re.sub('\\xc3\\x9f','\xdf',w) # ss
    stopwords[i] = w
    i += 1



infiles = os.listdir(filedir)
#infiles = ['Stephan Mayer (CDU-CSU) (15)(direct)_15024_.txt']



for infilename in infiles:
    
    print 'Processing: %s' % infilename

    infile_handle = filedir + infilename
    intext = open(infile_handle)
    
    outfile_handle = outdir + infilename
    outfile = open(outfile_handle,'a')
    
    for line in intext:
            
        line = re.sub('\w+:','',line)
        
        if nouns:
            match = re.findall('[A-Z][^\s]+',line)
            if match:
                line = ' '.join(match)
                line = line + '\n'
            else:
                continue
        
        line = line.lower()

        if sep_comp:
            a = re.search(reg_arbeit,line)
            if a:
                line = re.sub(reg_arbeit,' arbeitslos ',line)
                
                
        words = line.split(' ')

        newline = []
        for word in words: 
            word = re.sub('\n','',word)   
            if word not in stopwords:
                if stemming:
                    word = stemmer.stem(word)
                newline.append(word)
    
        newline.append('\n')
        newline = ' '.join(newline)
        outfile.write(newline)
    
    outfile.close()


        
        
        
        







    


