#-*- coding: utf-8 -*-
# This script takes raw session protocols of the German Bundestag 
# from period 15 to 17 and extracts speeches by single MPs (Members 
# of Parliament). ALl other content (Comments, Interruptions, 
# Organizational Content, Speech by the session leader) is discarded. 
# Each MP is matched with a list of MPs to extract metadata. Each 
# single speech is saved in a file: 
# ' mpout/FirstName LastName(Party)(Period)(MandateType).txt'. Guest 
# speakers (not MPs) are saved in 'mpout/notMP/NAME.txt'
# The raw documents are publicly available from the German parliamentary 
# archive ('http://www.bundestag.de/dokumente/parlamentsarchiv')

import re
import os
import time
start_time = time.time()

# Directories
maindir = 'C:/Users/samsung/Dropbox/Ma Thesis/Empirics/data/parl/'
indir = maindir + 'txtcomplete/'
outdir = maindir + 'mpout/'

# Regular expressions
reg_start = r'^ ?Beginn:.+Uhr\s+$'
reg_stop = r'Anlagen? zum [S,s]tenografischen Bericht'
reg_comment = r'^\(.+\)? ?$'
reg_spkch = r'^[A-Z].+:\s+$' 
reg_ncap = r'( [a-z][a-z])'
reg_van = r'( +van)|( +von)|( +bei)|( +und)|( +f\\xc3\\xbcr)|( +f\xc3\xbcr)|( +für)|( +f\xfcr)|( +im)|( +beim)|( +der)|( +der)|( +des)|( +besondere)|( +wirtschaftliche)|( +de)|( +zu)|( +am)'
reg_space = r'\w+ \w+'
reg_TOP = r'punkt'
reg_bracket = r'^\('
reg_ex1 = r'(Kolleg)|(Beispiel)|(Schluss)|(Beirates)|(Ja,)|(Leitlinie)|(Verlangen)|(Frage)|(Antwort)|(Zitat)|(Wahlkampf)|(Herr)|(Drittens)'
reg_drop = r'[^a-zA-Z\xe4\xf6\xfc\xdf\s:]' # delete : again
reg_smcol = r' [a-z]+:$'

def matchNames(line,namelist):
    match_count = 0
    match_list = []
	# MPs who changed names
    if line == 'Grietje Bettin':
        line = 'Grietje Staffelt'
    if line == 'Kersten Naumann':
        line = 'Kersten Steinke'
    if line == 'Agnes Malczak':
        line = 'Agnes Brugger'
    if line == 'Nadine Müller':
        line = 'Nadine Schön'
    try:
        mandate = namelist[line][period]
    except KeyError:
        name = line.split(' ')
        lastname = name[-1] + '$'
        for key in namelist:
            match = None
            match = re.search(lastname,key)
            if match:
                match_count = match_count + 1
                match_list.append(namelist[key])
                mandate = namelist[key][period]   
        if match_count < 1:
            mandate = None
            print 'Warning: No mandate type found for %s' %line
        if match_count > 1:
            firstname = name[0]
            match_count2 = 0
            for mp in match_list:
                match = re.search(firstname,mp['Name'])
                if match:
                    match_count2 = match_count2 + 1
                    mandate = mp[period]
            if match_count2==0:
                mandate = None
                print 'Warning: No mandate type found for %s' %line
            if match_count2 > 1:
                print 'Warning: Multiple matches for %s not resolved #########' %line
    return(mandate) 
        
    
def makeFilename (line,period,namelist,outdir):
    dir_others = outdir + 'notmp/'
    if not os.path.isdir(outdir):
        os.makedirs(outdir)
    if not os.path.isdir(dir_others):
        os.makedirs(dir_others)
    fname_temp = '{0} {1} ({2})({3}).txt'
    reg_party = r'\(CDU-CSU\)|\(FDP\)|\(SPD\)|\(B\xdcNDNIS 90-DIE GR\xdcNEN\)|\(DIE LINKE\)|\(fraktionslos\)'
    reg_para = r'\(.+\)'
    reg_rm = r'(Dr\.)|(h\. +c\. +)|(:)|(\n)'
    party_match = re.search(reg_party,line)
    line = re.sub(reg_para,'',line)
    line = re.sub(reg_rm,'',line)
    line = re.sub('\s+$','',line)
    line = re.sub('^\s+','',line) 
    mandate = None   
    if party_match: 
        mandate = matchNames(line,namelist)
        if not mandate:
            mandate = 'NA#########'
            no_mandate.append(line)
        party = party_match.group()
        fname = fname_temp.format(line,party,period,mandate)
        fname = outdir + fname
    else:
        fname = dir_others + line + '.txt'
    return fname


# Get list of mps with districts etc
namelist_fname = maindir + 'out.txt'
namelist_file = open(namelist_fname,'r')
namelist1 = namelist_file.readlines()

namelist = {}
for mp in namelist1:
    line = mp
    line = re.sub('\\xc3\\x9c','\xdc',line)
    line = re.sub('\\xc3\\xa4','\xe4',line) # ae
    line = re.sub('\\xc3\\xb6','\xf6',line) # oe 
    line = re.sub('\\xc3\\xbc','\xfc',line) # ue
    line = re.sub('\\xc3\\x9f','\xdf',line) # ss
    line = re.sub('\xc3\xa9','e',line)
    line = re.sub('\xe9','e',line)
    line = re.sub('\xc4\x9f','g',line)
    line = re.sub('\xf0','g',line)
    line = re.sub('\xc3\xb0','g',line)
    line = re.sub('\xc5\xa1','s',line)
    line = re.sub('\xc4\x87','c',line)
    line = re.sub('/','-',line)
    line = re.sub('\n','',line)
    mp = line.split(',')
    name = re.sub('\s+$','',mp[0])
    namelist[name] = {'Name':name ,'Party':mp[1],'15':mp[2],'16':mp[3],'17':mp[4]}

outfile = None
files = os.listdir(indir)
k = 0
line_count = 0
session_stamp = '\n<session>\n%s\n<\session>\n\n'
first_entry = True
no_mandate = []

for infile_name in files:
    """
    #=================================
    start_file = '16118.txt'
    stop_file  = '17120.txt'
    if infile_name == start_file:
        k = 1
    if k == 0:
        print infile_name + 'skipped'
        continue
    if infile_name == stop_file:
        break
    #=================================
    #"""

    period = infile_name[0:2]
    infile_name_dir = indir + infile_name
    infile = open(infile_name_dir)
    print 'Recording from ' + infile_name
    
    i = 0
    changeline = 0
    change_count = 0
    change_dic = {}
    fname_list = []
    
    for line in infile:
        line_count = line_count + 1
        line = re.sub('Sevim Da.delen','Sevim Dagdelen',line)
        
        line = re.sub('\(cid:228\)','\xe4',line) #ae
        line = re.sub('\(cid:246\)','\xf6',line) #oe
        line = re.sub('\(cid:252\)','\xfc',line) #ue
        line = re.sub('\(cid:223\)','\xdf',line) #ss
        
        line = re.sub('Deutscher Bundestag (cid:150) 15. Wahlperiode (cid:150) \d\d\. Sitzung. Berlin, \w+, den \d\d?. \w+ \d\d\d\d','',line)
        line = re.sub('\(cid:\d\d\d\)','',line) #rest
        
        # Controls for starting after the preamble and stopping at the appendix
        start = re.search(reg_start,line)
        stop = re.search(reg_stop,line)
        if start and i == 0:
            i = 1 
        if stop:
            outfile.close()
            outfile = None
            change_dic[infile_name] = change_count
            break
        if i == 0:
            continue
        comment = re.search(reg_comment,line)
        if comment:
            continue
        
        #line = re.sub(r'-','',line)
        line = re.sub('\xc2',' ',line)
        line = re.sub('\xa0',' ',line) # latin-1 space
        line = re.sub('\t','',line)    
        line = re.sub('\/','-',line)
        line = re.sub('\xc2\xa0',' ',line)
        line = re.sub('\\xc3\\x9c','\xdc',line)
        line = re.sub('\\xc3\\xa4','\xe4',line)
        line = re.sub('\\xc3\\xb6','\xf6',line)
        line = re.sub('\\xc3\\xbc','\xfc',line)
        line = re.sub('\\xc3\\x9f','\xdf',line)
        line = re.sub('\xc3\xa9','e',line)
        line = re.sub('\xe9','e',line)
        line = re.sub('\xc4\x9f','g',line)
        line = re.sub('\xf0','g',line)
        line = re.sub('\xc3\xb0','g',line)
        line = re.sub('\xc5\xa1','s',line)
        line = re.sub('\x9a','s',line)
        line = re.sub('\xc4\x87','c',line)
        line = re.sub('^ +','',line)
        line = re.sub(' +$','',line)
                
        # Identify Speaker change
        van = None
        spkch = re.search(reg_spkch,line)
        if spkch:
            #print line
            ncap = re.search(reg_ncap,line)    
            space = re.search(reg_space,line)  
            TOP = re.search(reg_TOP,line)       
            ex1 = re.search(reg_ex1,line)       
            bracket = re.search(reg_bracket,line)
            smcol = re.search(reg_smcol,line)
            
            if ncap:
                van = re.search(reg_van,line) # Exception for names with van/von/des etc...
                
                if van:
                    tline = re.sub(reg_van,' ',line)
                    ncap = re.search(reg_ncap,tline)
            
            if space and not ncap and not TOP and not ex1 and not bracket and not smcol:
                change_count = change_count + 1
                changeline = 1
       
        if outfile != None and changeline == 0:
            
            if first_entry:
                outfile.write(session_stamp %infile_name[0:5])
                first_entry = False
            
            line = re.sub(reg_drop,' ',line)
            outfile.write(line)
            
        if changeline == 1:
            
            if outfile != None:
                outfile.close()
            
            fname = makeFilename(line,period,namelist,outdir)
            
            if fname not in fname_list:
                first_entry = True
                fname_list.append(fname)
            
            outfile = open(fname,'a')

        changeline = 0
        
    infile.close()

for key in change_dic:
    print change_dic[key]

print 'Finished in (sec):'
T = time.time() - start_time
print T
print line_count
lps = line_count/T
print str(lps) + ' lps'
print list(set(no_mandate))
