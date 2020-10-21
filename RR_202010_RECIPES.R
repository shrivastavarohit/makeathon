install.packages("jsonlite")
install.packages("factoextra")
install.packages("varhandle")
install.packages("lattice")

library("jsonlite")
library("factoextra")
library("varhandle")
library("lattice")



wssKM <- function(data, maxCluster = 10) {

    	SSw <- (nrow(data) - 1) * sum(apply(data, 2, var))
    	SSw <- vector()
	ADc <- (nrow(data) - 1) * sum(apply(data, 2, var))
    	ADc <- vector()
	ACd <- (nrow(data) - 1) * sum(apply(data, 2, var))
    	ACd <- vector()
	Q <- (nrow(data) - 1) * sum(apply(data, 2, var))
    	Q <- vector()
    	for (i in 2:maxCluster) {
		KM <- factoextra::hkmeans(data, k = i, hc.metric = "euclidean", hc.method = "ward.D2")
        	SSw[i] <- KM$tot.withinss
		ADc[i] <- KM$tot.withinss / sum(KM$size)
		ACd[i] <- mean(dist(KM$center))
		Q[i] <- KM$betweenss / KM$totss
    	}

	L <- list(SSw, ADc, ACd, Q)
    	return(L)
}


TXT_CLEAN <- function(TXT) {
  	
	TXT <- base::gsub("[^[:alnum:] ]"," ",TXT) # REPLACE ALL NON ALPHANUMERIC CHARACTERS WITH " "
	
	TXT <- textclean::mgsub(TXT, c("é","è","ë","ê"),"e")
	TXT <- textclean::mgsub(TXT, c("í","ì","ï","î"),"i")
	TXT <- textclean::mgsub(TXT, c("á","à","â","ã","å"),"a")
	TXT <- base::gsub("ä","ae",TXT)
	TXT <- textclean::mgsub(TXT, c("ú","ù","û"),"u")
	TXT <- base::gsub("ü","ue",TXT)
	TXT <- textclean::mgsub(TXT, c("ó","ò","ô"),"o")
	TXT <- base::gsub("ö","oe",TXT)
	TXT <- base::gsub("ñ","n",TXT)
	TXT <- base::gsub("ç","c",TXT)
	TXT <- base::gsub("ß","ss",TXT)
	TXT <- base::gsub("\\s+"," ",TXT) # REMOVE EXTRA WHITE SPACE
	TXT <- base::gsub('[[:digit:]]+', '', TXT)
	TXT <- base::trimws(TXT) # REMOVE LEADING OR TAILING WHITE SPACE

  	return(TXT)
}



path <- file.choose()

main_sample = jsonlite::stream_in(file(path))
data = jsonlite::flatten(main_sample)
i <- sapply(data, is.list)
data[i] <- lapply(data[i], as.character)

strings <- TXT_CLEAN(
		gsub('\\b[A-Z]+\\b', '', 
			gsub('\\b[a-z]+\\b', '', 
				gsub('\\W*\\b\\w\\b\\W*', ' ',
					gsub('[[:punct:]]+', '', 
						gsub('[[:digit:]]+', '', 
							gsub("\\s*\\([^\\)]+\\)","",TXT_CLEAN(data$Ingredients))
						)
					)
				)
			)
		)
	)


tab <- base::unlist(base::strsplit(strings," "))

TAB <- data.frame(table(tab))
TAB <- TAB[order(-TAB[,2]),]



###################################

ingedients <- TAB[1:100,1]

list_ingedients <- c()
for(i in 1:100){ list_ingedients [i] <- 
paste0(sample(ingedients, round(runif(1, 3, 10))), collapse = ", ")
}

TAB <- data.frame(table(gsub('[[:punct:]]+', '',base::unlist(base::strsplit(list_ingedients," ")))))
TAB <- TAB[order(-TAB[,2]),]

list <- c("USER_A","USER_B","USER_C","USER_D","USER_E","USER_F","USER_G","USER_H",
	"USER_I","USER_J","USER_K","USER_L","USER_M","USER_N","USER_O","SAMPLE")

DATA_1 <- data.frame(
	USER = list,
	AGE = sample(20:45, 16, replace=T),
	#FOOD = sample(gsub('[[:punct:]]+', '',base::unlist(base::strsplit(list_ingedients," "))),16),
	WEIGHT = sample(55:95, 16, replace=T),
	NATIONALITY = sample(c("ITA", "FRA", "GER", "CHE"), 16, replace=T),
	GENDER = sample(c("M","F"),16, replace=T)
	)

NATIONALITY <- to.dummy(DATA_1$NATIONALITY, "NATIONALITY")
GENDER <- to.dummy(DATA_1$GENDER, "GENDER")

DATA_2 <- data.frame(
	RECIPES = sample(data$Name,100), 
	INGREDIENTS = list_ingedients, 
	USER_A = sample(-1:1, 100, replace=T,prob=c(0.01,0.8,0.19)),
	USER_B = sample(-1:1, 100, replace=T,prob=c(0.01,0.8,0.19)),
	USER_C = sample(-1:1, 100, replace=T,prob=c(0.01,0.8,0.19)),
	USER_D = sample(-1:1, 100, replace=T,prob=c(0.01,0.8,0.19)),
	USER_E = sample(-1:1, 100, replace=T,prob=c(0.01,0.8,0.19)),
	USER_F = sample(-1:1, 100, replace=T,prob=c(0.01,0.8,0.19)),
	USER_G = sample(-1:1, 100, replace=T,prob=c(0.01,0.8,0.19)),
	USER_H = sample(-1:1, 100, replace=T,prob=c(0.01,0.8,0.19)),
	USER_I = sample(-1:1, 100, replace=T,prob=c(0.01,0.8,0.19)),
	USER_J = sample(-1:1, 100, replace=T,prob=c(0.01,0.8,0.19)),
	USER_K = sample(-1:1, 100, replace=T,prob=c(0.01,0.8,0.19)),
	USER_L = sample(-1:1, 100, replace=T,prob=c(0.01,0.8,0.19)),
	USER_M = sample(-1:1, 100, replace=T,prob=c(0.01,0.8,0.19)),
	USER_N = sample(-1:1, 100, replace=T,prob=c(0.01,0.8,0.19)),
	USER_O = sample(-1:1, 100, replace=T,prob=c(0.01,0.8,0.19))
	)

DATA_1_cat <- data.frame(
	USER = DATA_1$USER,
	AGE = DATA_1$AGE,
	WEIGHT = DATA_1$WEIGHT,
	NATIONALITY,
	GENDER
	)

View(DATA_2)
View(DATA_1)
View(DATA_1_cat)


# PRINCIPAL COMPONENT ANAYLSIS
# :::::::::::::::::::::::::::::::::::::::::::::::::::

DATA_1_cat_scale <- scale(DATA_1_cat[,-1])
df.pca <- FactoMineR::PCA(DATA_1_cat_scale,  graph = FALSE)

# EXTRACT EIGENVALUES/ VARIANCE 
# :::::::::::::::::::::::::::::::::::::::::::::::::::

ev <- factoextra::fviz_screeplot(
	df.pca, 
	addlabels = TRUE
	)

# EXTRACT THE RESULTS FOR VARIABLES
# :::::::::::::::::::::::::::::::::::::::::::::::::::
var <- factoextra::get_pca_var(df.pca)

# CONTRIBUTION OF VARIABLES TO PC1
# :::::::::::::::::::::::::::::::::::::::::::::::::::
pc1 <- factoextra::fviz_contrib(df.pca, choice = "var", axes = 1)

# CONTRIBUTION OF VARIABLES TO PC2
# :::::::::::::::::::::::::::::::::::::::::::::::::::
pc2 <- factoextra::fviz_contrib(df.pca, choice = "var", axes = 2)

# CONTRIBUTION OF VARIABLES TO PC3
# :::::::::::::::::::::::::::::::::::::::::::::::::::
pc3 <- factoextra::fviz_contrib(df.pca, choice = "var", axes = 3)

# COMPUTE PCA ON DATA SET
# :::::::::::::::::::::::::::::::::::::::::::::::::::

pcp <- factoextra::fviz_pca_ind(
		df.pca,
            label = "none", # hide individual labels
            )

pcp <- pcp + 
	ggtitle("PCA 1 & PCA 2") +
	ylab(paste("PC2 (", round(df.pca$eig[2,2],1),"%)", sep = "")) +
 	xlab(paste("PC1 (", round(df.pca$eig[1,2],1),"%)", sep = ""))
	

# 3D SCATTERPLOT OF PCA 1-3
# :::::::::::::::::::::::::::::::::::::::::::::::::::

PC1 <- matrix(df.pca$ind$coord[,1])

PC2 <- matrix(df.pca$ind$coord[,2])

PC3 <- matrix(df.pca$ind$coord[,3])

s3d <- lattice::cloud(PC3 ~ PC1 * PC2, 
		data = df.pca,
		screen = list(x = -90, y = 70), 
		distance = 0.4, 
		zoom = 0.6,
		pch=16
		)

# ESTIMATE NUMBER OF CLUSTERS (ELBOW METHOD)
# :::::::::::::::::::::::::::::::::::::::::::::::::::

# KMEANS
nclustKM <- wssKM(DATA_1_cat_scale)

CLUST.KM.WSS <- data.frame(cbind(seq(1:10),nclustKM[[1]]))
colnames(CLUST.KM.WSS) <- c("NCLUST","TWSS")

# LINE PLOT
# :::::::::::::::::::::::::::::::::::::::::::::::::::

lpKM_WSS <- ggpubr::ggline(
	CLUST.KM.WSS, 
	x = "NCLUST", 
	y = "TWSS",
	lab.col = "black"
)

center = 4

lpKM_WSS <- lpKM_WSS + geom_vline(
			xintercept = center, 
			linetype = "dashed", 
			size = 1)

lpKM_WSS <- lpKM_WSS + ggtitle(
			"# CLUST. (H-KMEANS)") +
  			xlab("# CLUSTERS") + 
			ylab("WITHIN GROUP \n SUM OF SQUARE")

KM <- factoextra::hkmeans(DATA_1_cat_scale, k = center, hc.metric = "euclidean", hc.method = "ward.D2")
KM.center <- KM$center

cluster <- stats::kmeans(DATA_1_cat_scale, centers=KM.center)

DATA_1_cat$SCORE <- as.factor(cluster$cluster) 


s3d <- lattice::cloud(PC3 ~ PC1 * PC2, 
		data = df.pca,
		screen = list(x = -90, y = 70), 
		distance = 0.4, 
		zoom = 0.6,
		pch=16,
		group=DATA_1_cat$SCORE
		)



