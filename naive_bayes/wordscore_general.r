# Function for calculating wordscores
# Takes either a directorty containing all texts (references and virgins) or a term document matrix containing all texts
# The reference texts are taken by the argument references in the form of a data frame with rows for dimensions and 
# two columns for the end points of the dimensions:
#             left pole   right pole
#     D1      'regex'       'regex'
#     D2      'regex'       'regex'
#     .         ...           ...
#     .         ...           ...
#     .
# The cells contain regular expressions that match the filenames of the reference texts in the directory 
# (or the colnames of the term documant matrix)
# All texts in the directory/tdm get scored so the user has to sort out what she wants in the end

wordscore = function(input,references,refPos=c(-1,1),language='german'
                     ,sparsemax=0.99,specialwords=FALSE,commonremove=FALSE,scorereferences=FALSE){
  
  if(is.matrix(input)==FALSE){
    require(tm)
    cat('Preparing Term Document Matrix: \n')
    cat('Generating Corpus... \n')
    corpus = Corpus(DirSource(input),readerControl = list(reader=readPlain,language = NA))
    cat('Cleaning Text... \n')
    corpus = tm_map(corpus,removeNumbers)
    corpus = tm_map(corpus,tolower)
    corpus = tm_map(corpus,removePunctuation)
    corpus = tm_map(corpus,removeWords,stopwords(language))
    if(is.character(specialwords)) corpus = tm_map(corpus,removeWords,specialwords)
    corpus = tm_map(corpus,stripWhitespace)
    cat('Constructing TDM... \n')
    TDM = TermDocumentMatrix(corpus, control = list(stemming=function(x) stemDocument(x, language=language)))
    TDM = removeSparseTerms(TDM, sparsemax)
    if(commonremove!=FALSE){
      TDM = cbind(TDM,rowSums(TDM))
      TDM = TDM[order(TDM[,ncol(TDM)],decreasing=TRUE),]
      TDM = TDM[commonremove,-ncol(TDM)]
    }
    TDM = as.matrix(TDM)
    cat('Finished!\n\n')
  }
  else TDM = input
  
  results = matrix(NA,nr=nrow(references),nc=ncol(TDM))
  colnames(results) = colnames(TDM)
  ses = matrix(NA,nr=nrow(references),nc=ncol(TDM))
  colnames(ses) = colnames(TDM)
  refM = function(refTDM){
    if(is.vector(refTDM)) out_vec = refTDM/sum(refTDM)
    else out_vec = apply(refTDM,1,sum)/sum(apply(refTDM,1,sum))}
  
  for(d in 1:nrow(references)){
    cat(paste('Scoring Virgin Documents on Dimension',d, '\n'))
    ref1 = grep(as.character(references[d,1]),colnames(TDM))
    ref2 = grep(as.character(references[d,2]),colnames(TDM))
    if(length(ref1)==0 | length(ref2)==0) stop(paste('Reference texts for dimension',d,'not found'))
    refTDM1 = TDM[,ref1]
    refTDM2 = TDM[,ref2]
    virginTDM = TDM
    
    ref_matrix = matrix(0,nc=2,nr=nrow(TDM))
    ref_matrix[,1] = refM(refTDM1)
    ref_matrix[,2] = refM(refTDM2)
    ref_matrix = ref_matrix/outer(rowSums(ref_matrix),rep(1,ncol(ref_matrix)))
    ref_matrix[is.na(ref_matrix)] = 0
    
    # calculate scores
    ref_matrix = ref_matrix*cbind(rep(refPos[1],nrow(ref_matrix)),rep(refPos[2],nrow(ref_matrix)))
    ref_matrix = cbind(ref_matrix,rowSums(ref_matrix))
    wordscores = ref_matrix
    rownames(wordscores) = rownames(TDM)
    wordscores = wordscores[order(wordscores[,3],decreasing=T),]
    
    # calculate scores of virgin texts
    rtf_matrix = virginTDM/outer(rep(1,nrow(virginTDM)),colSums(virginTDM))
    ref_score_matrix = matrix(rep(ref_matrix[,3],ncol(rtf_matrix)),nr=nrow(rtf_matrix))
    score_matrix = rtf_matrix*ref_score_matrix
    scores = colSums(score_matrix)
    
    
    # calculate uncertainty
    mean_score_matrix = matrix(scores,nr=nrow(score_matrix),nc=ncol(score_matrix),byrow=TRUE)
  
    deviations = (ref_score_matrix-mean_score_matrix)^2
    variances = colSums(rtf_matrix*deviations)
    se = sqrt(variances)
    raw_lower = scores - 2*se
    raw_upper = scores + 2*se
    
    # Scale Results
    scaled_scores = (scores-mean(scores))*(sd(refPos)/sd(scores)) + mean(scores)
    scaled_lower = (raw_lower-mean(raw_lower))*(sd(refPos)/sd(raw_lower)) + mean(raw_lower)
    scaled_upper = (raw_upper-mean(raw_upper))*(sd(refPos)/sd(raw_upper)) + mean(raw_upper)
    
    # Calcuklate se
    se_scaled = abs(scaled_scores-scaled_upper)/2
    
    # store results
    results[d,] = scaled_scores
    ses[d,] = se_scaled
    
  }
  
  return(list('results'=results,'se'=ses,'tdm'=TDM,'wordscores'=wordscores))
}
















