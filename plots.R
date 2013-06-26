


barplot(t(as.matrix(results.fence$strategy1f[,3:5])),horiz=TRUE,beside=FALSE,cex.names=0.7,las=1)

barplot(t(as.matrix(results.nofence$strategy1[,3:5])),horiz=TRUE,beside=FALSE,cex.names=0.7,las=1)




resp.plot <- t(as.matrix(cbind(resp$ppos,resp$pneg,resp$pzero)))
dimnames(resp.plot) <- list(c("pos","neg","zero"),nodes)

# plot the responses
barplot(resp.plot,cex.names=0.7,xlab="Proportion",horiz=TRUE,las=1)
title("People/domestic ruminants suppression",line=3)