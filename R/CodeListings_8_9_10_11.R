#Code Listing 8
#cosine similarity among terms
cosine_simil_terms<-cosine(t(projdocterms))
# terms most similar to sicherstellung
names(which(cosine_simil_terms[,"sicherstellung"]>.5))

#Code Listing 9
# terms most similar to koordinierung
names(which(cosine_simil_terms[,"koordinierung"]>.5))

#Code Listing 10
# correlations among terms
term_correlation<-cor(t(projdocterms))
# correlations among terms
document_correlation<-cor(projdocterms)

# Code Listing 11 
# Project the documents to the latent dimensions
projdoc<-t(mytdm_mat_lsa) %*% lsaSpace$tk %*% solve(diag(lsaSpace$sk))
projdoc<-projdoc[rowSums(projdoc)!=0,]
