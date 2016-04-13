#-*- coding: utf-8 -*-
# Takes the output from a topic model and joins all speech by one
# MP to a topic in one document. Stemmed and cleaned speech documents 
# as input. Output file names: 
# 'FirstName LastName (Party) (Period)(MandateType)_TopicCode.txt'

import re
import os
import sys
from collections import defaultdict

# Directories
maindir = 'C:/Users/samsung/Dropbox/Ma Thesis/Empirics/data/parl/'
outdir = maindir + 'by_topic/'
indir = maindir + 'c_splitted/'
outfile_temp = outdir + '{0}' + '_{1}.txt' 

if not os.path.isdir(outdir):
    os.makedirs(outdir)

files = os.listdir(indir)

info = open('C:/Users/samsung/Dropbox/Ma Thesis/Empirics/analysis/grimmer/output/topicdocs.txt').read()
info = info.split('\n')


topicdocs = {}
for line in info:
    cont = line.partition(',')
    fname = cont[0]
    if fname == '':
        continue
    topic = cont[2]
    topicdocs[fname] = topic


infiles = os.listdir(indir)
author_list = []

for filename in topicdocs:
    author = re.sub('_.+_.txt','',filename)
    author = re.sub('\(','\\(',author)
    author = re.sub('\)','\\)',author)
    author0 = re.sub('_.+_.txt','',filename)
    
    if author in author_list:
        continue
    author_list.append(author)
    print 'Processing: %s' %author
    
    for infilename in infiles:
    
        match = re.search(author,infilename)
        if match:
            infile_handle = indir + infilename
            content = open(infile_handle).read() + '\n\n'
            try:
                topic = topicdocs[infilename]
            except KeyError:
                print infilename + ' not found in topicdocs'
            outfile_handle = outfile_temp.format(author0,topic)
            outfile = open(outfile_handle,'a')
            outfile.write(content)
            outfile.close()
            
    

    
"""   
    
    some_dict = { 'abc':'a', 'cdf':'b', 'gh':'a', 'fh':'g', 'hfz':'g' }
    new_dict = defaultdict(list)
    for k, v in some_dict.iteritems():
        new_dict[v].append(k)
            
            
    break

        
    


length = len(topicdocs.keys())
author_list = []
i = 0
for key1 in topicdocs:
    i += 1
    collection = {}
    author = re.sub('\(.+\)','',key1)
    author = re.sub('_.+','',key1)
    digit = None
    digit = re.search('(\d\d)',key1)
    if digit:
        digit = re.sub('[(,)]','',digit)
        period = int(digit)
        print period
    if author in author_list:
        continue
    author_list.append(author)
    
    prog = 'Processing file %d/' % i + str(length) + '\r'
    print(prog)
    #sys.stdout.write('\r')
    #sys.stdout.write(prog)
    #sys.stdout.flush()

    for key2 in topicdocs:
        
        if re.search(author,key2):
            collection[key2] = topicdocs[key2]
            print('second if') 

    for key3 in collection:
        infile = indir + key3
        content = open(infile).read()
        fname = re.sub('_.+_','_%s_',key3)
        outfile_name = outdir + fname % collection[key3]
        outfile = open(outfile_name,'a')
        outfile.write(content)
        outfile.write('\n\n')
    
"""





    