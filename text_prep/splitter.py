#-*- coding: utf-8 -*-
# This script searches through the speech files generated by speech_extract.py
# collects all speech from one session and saves it in a separate file:
# 'FirstName LastName(Party)(Period)(MandateType)_SessionNumber_.txt'

import re
import os
import time
start_time = time.time()

# Directories
maindir = 'C:/Users/samsung/Dropbox/Ma Thesis/Empirics/data/parl/'

outdir = maindir + 'splitted/'

indir = maindir + 'mpout/'

# Regular expressions
reg_tag = r'<\\?session>'
reg_seno = r'^\d\d\d\d\d\r?\n$'

if not os.path.isdir(outdir):
    os.makedirs(outdir)

files = os.listdir(indir)
#outfile = None
line_count = 0

#files = ['Nadine Schoen (CDU-CSU) (17)(direct).txt']

for infile_name in files:
    outfile = None
    if not infile_name.endswith(".txt"):
        continue
    
    print 'Splitting %s' %infile_name
    
    infile_name_dir = indir + infile_name
    infile = open(infile_name_dir)   
    
    outfile_template = outdir + infile_name[0:-4] + '_%d_.txt'
    
    tp = False
    
    
    for line in infile:
        
        line_count = line_count + 1
        
        if not line.strip():
            continue
        if re.search('^\n$',line):
            continue

        if tp:
            if re.search(reg_seno,line):
                line = re.sub('\s','',line)
                seno = int(line)
                outhandle = outfile_template %seno
                outfile = open(outhandle,'w')
                continue
            if re.search(reg_tag,line):
                tp = False
                continue
                     
        if re.search(reg_tag,line):
            tp = True
            if outfile:
                outfile.close()
            continue
        
        outfile.write(line)
        

        
print 'Finished in (sec):'
T = time.time() - start_time
print T
print line_count
lps = line_count/T
print str(lps) + ' lps'


        
        
        
    