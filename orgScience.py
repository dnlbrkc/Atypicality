exec(open('magclass.py').read())
MAG = MicrosoftAcademicGraph()


#get journal ID
SMJ = [ 'Organization Science' ]
journal = MAG.getDataframe( 'Journals' )
journalPapers = journal.filter( journal.DisplayName.isin( SMJ ) ) \
                                 .select(journal.JournalId , journal.NormalizedName, journal.PaperCount)
#journalPapers.show(10)
#thorAuthors.show(10)


papers = MAG.getDataframe( 'Papers' )
jp = papers.filter( papers.JournalId == '206124708' ) \
                                 .select(papers.JournalId , papers.PaperId, papers.PaperTitle, papers.CitationCount,papers.Year)

#authors = MAG.getDataframe( 'PaperAuthorAffiliations' )
#paperAuthors = authors.join( jp, authors.PaperId == jp.PaperId ) \
#                                 .select(authors.AuthorId , authors.OriginalAuthor)


refs= MAG.getDataframe( 'PaperReferences' )
paperCitations = refs.join( jp , jp.PaperId == refs.PaperId)
newNames = ['PaperId','PaperReferenceId', 'JournalId','PaperId2', 'PaperTitle','CitationCount','Year']
thorCitations= paperCitations.toDF(*newNames)

thorCitations.write.csv("years_final")




#papers = MAG.getDataframe( 'Papers' )
#paperJournals = papers.filter( papers.PaperId == thorCitations.PaperReferenceId ) \
#                                 .select(papers.JournalId , papers.PaperId, papers.PaperTitle)

paperJournals = papers.join( thorCitations , papers.PaperId == thorCitations.PaperReferenceId ) \
                   .select(thorCitations.PaperId,thorCitations.PaperReferenceId,thorCitations.JournalId,thorCitations.PaperTitle,papers.JournalId,papers.Year)


#paperJournals = papers.join( thorCitations , papers.PaperId == '65337815' ) \
#                    .select(thorCitations.PaperId,thorCitations.PaperReferenceId,thorCitations.JournalId,thorCitations.PaperTitle,papers.JournalId,papers.Year)

newNames = ['PaperId','PaperReferenceId', 'JournalId', 'PaperTitle','JournalId2','Year']
paperJournals= paperJournals.toDF(*newNames)

paperJournals.write.csv("JournalInfo")

























newNames = ['PaperId','PaperReferenceId', 'PaperId2', 'AuthorId','NormalizedName']
thorCitations= thorCitations.toDF(*newNames)



#match Author with Papers
paperAuthorAffiliations = MAG.getDataframe( 'PaperAuthorAffiliations' )
thorAuthorPaperAffs = paperAuthorAffiliations \
                         .join( thorAuthors , paperAuthorAffiliations.AuthorId == thorAuthors.AuthorId , 'inner' ) \
                         .select(paperAuthorAffiliations.PaperId , paperAuthorAffiliations.AuthorId , thorAuthors.NormalizedName, paperAuthorAffiliations.AuthorSequenceNumber)
#thorAuthorPaperAffs .show(10)


#get outgoing paper references
paperCIT = MAG.getDataframe( 'PaperReferences' )
thorCitations = paperCIT.join( thorAuthorPaperAffs , thorAuthorPaperAffs.PaperId == paperCIT.PaperId)
newNames = ['PaperId','PaperReferenceId', 'PaperId2', 'AuthorId','NormalizedName']
thorCitations= thorCitations.toDF(*newNames)


thorCitations2 = paperCIT.join(thorAuthorPaperAffs, thorAuthorPaperAffs.PaperId == paperCIT.PaperReferenceId ) 
newNames = ['PaperId','PaperReferenceId', 'PaperId2', 'AuthorId','NormalizedName']
thorCitations2= thorCitations2.toDF(*newNames)
#new_df = thorCitations2.drop('PaperId')
outputFileNamePrefix = 'ORGSCI'
fileName = outputFileNamePrefix + '.csv' 
asu.save( thorCitations2 , fileName )




paperCIT = MAG.getDataframe( 'PaperReferences' )
thorCitations = paperCIT.join( thorAuthorPaperAffs , paperCIT.PaperId == thorAuthorPaperAffs.PaperId )
newNames = ['PaperId','PaperReferenceId', 'PaperId2', 'AuthorId','NormalizedName']
thorCitations= thorCitations.toDF(*newNames)