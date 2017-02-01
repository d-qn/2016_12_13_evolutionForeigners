############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")

widthFig <- 10
heightFig <- widthFig

############################################################################################
###		Plot world map % immigrant foreign born
############################################################################################

data.file <- 'hs-f-01.01.01.03.csv'
data.read <- read.csv(data.file)

## Reformat
# get rid of the ...
for(i in 2:ncol(data.read)) {
	data.read[,i] <- as.numeric(as.character(data.read[,i]))
}
# replace NA with 0
data.read[is.na(data.read)] <- 0

#rename country
data.read[,1] <- gsub(" \\d\\)", "", data.read[,1])
# rename columns/year
colnames(data.read) <- gsub("^X", "", colnames(data.read))
# cluster ex-yougoslavia countries
yougo <- c('Yougoslavie', 'Serbie-et-Monténégro', 'Serbie', 'Monténégro', 'Kosovo', 'Croatie', 'Bosnie et Herzégovine','Macédoine')
idx <- match(yougo, data.read[,1])
stopifnot(all(idx))

data <- data.read[-idx,]
tmp <- cbind(Nationalité = "Ex-Yougoslavie", as.data.frame(lapply(data.read[idx,2:ncol(data.read)], function(col) sum(col, na.rm = T))))
colnames(tmp) <- colnames(data)

data <- rbind(data, tmp)

pdf("Figures.pdf", width = widthFig, height = heightFig)

############################################################################################
###		Population Suisse, Européenne, et non-européenne
############################################################################################

data.sub <- data
data.sub <- data.sub[data.sub[,1] %in% c('Suisse', 'Europe'),]
data.sub <- melt(rbind(data.sub, cbind(Nationalité="non-européenne", data[data[,1] == 'Etranger',-1] - data[data[,1] == 'Europe',-1])))
data.sub$variable <- as.numeric(as.character(data.sub$variable))
data.sub$facet <- 'area'
data.sub$pc <- NA

etranger.pc <- tapply(data.sub$value, data.sub$variable, function(tt) (sum(tt[2:3], na.rm =T) / sum(tt)))
etranger.pc <- data.frame(Nationalité = NA, variable = as.numeric(rownames(etranger.pc)), value = NA, facet = "line", pc = as.vector(etranger.pc))
data.sub2 <- rbind(data.sub, etranger.pc)


plots <- list() # list of plots to lay them out in an single plot
#ggplot(data.sub) + geom_line(aes(variable, value, group = Nationalité, color = Nationalité))
g1 <- ggplot(data.sub2) + geom_area(aes(variable, value, fill = Nationalité), position = 'stack') + ggtheme +
	scale_y_continuous(name = "", limits = c(0,8 * 10^6), expand = c(0.0,0.0), labels  = function(x) x / 10^6) +
	scale_x_continuous(name = "", expand = c(0.0,0.0)) + theme(axis.ticks = element_line(size = 0.2)) +
	xlab("") + ylab("") + scale_fill_manual(values = swi_22rpalette[c(10,2,1)]) +
	ggtitle("Nationalité de population résidente en Suisse de 1850-2009") + theme(legend.position = "top")

g1
g1 + geom_vline(xintercept = 1880, color = "lightgrey")



ggplot(data = data.sub2) + geom_area(aes(variable, value / 10^6, fill = Nationalité), position = 'stack') + ggtheme +
	xlab("Année") + ylab("Population") + scale_fill_manual(values = swi_22rpalette[c(10,2,1)]) +
	ggtitle("La population résidente en Suisse de 1850-2009") + theme(legend.position = "top") +
	geom_line(aes(variable, pc)) + facet_wrap (~ facet, nrow = 2, scales = "free_y")+ theme(axis.ticks = element_line(size = 0.2))


g2 <- ggplot(dplyr::filter(data.sub2, facet == "line")) + geom_line(aes(variable, pc), size =2, color = "#663333") + ggtheme_ygrid + scale_x_continuous(name = "", expand = c(0.0,0.0)) +
scale_y_continuous(name = "", limits = c(0,0.25), expand = c(0.0,0.0), labels  = percent)


plots[[1]] <- g1  # add each plot into plot list
plots[[2]] <- g2
layout <- matrix(c(1, 1, 1, 1, 2), ncol = 1, byrow = TRUE)
multiplot(plotlist = plots, layout = layout)

## ANNOTATION
# 1. Au XIXe siècle, des millions d'Européens, des centaines de milliers de Suisses ont quitté le Vieux-Continent pour chercher un monde nouveau.
# La précarité des conditions de vie et l’insuffisance de la production agricole
# 2. C'est seulement vers 1880 que l’immigration d’étrangers vers la Suisse, provenant surtout des pays limitrophes, dépasse l’émigration hors de Suisse
# Au début du 20e siècle, 97% des étrangers proviennent des pays voisins (Allemagne, France, Italie, Autriche).
# 3. En Suisse où la politique d'immigration et la question des étrangers est régulièrement débattue, il est bon de mentionner qu'en 2012, 85% de la population résidante permanente étrangère en Suisse provient d'un pays européen. 2/3 de l'UE/AELE.

############################################################################################
###		Population pays d'Europe et continents
############################################################################################

data.sub <- melt(data)
data.sub$variable <- as.numeric(as.character(data.sub$variable))

#
by(data.sub, data.sub$variable, function(dd) (dd[dd$Nationalité == "Allemagne", 'value'] /  dd[dd$Nationalité == "Total ", 'value']) * 100)


#pays.sub <- c('Afrique', 'Asie', 'Amérique', 'Allemagne', 'France', 'Italie', 'Portugal', 'Espagne', 'Ex-Yougoslavie')
#pays.sub <- c('Allemagne', 'France', 'Italie', 'Portugal', 'Espagne', 'Ex-Yougoslavie')
pays.sub <- c('Allemagne','Italie', 'Portugal', 'Espagne', 'Ex-Yougoslavie', 'France', "Autriche",  "Turquie", 'Autre pays européen')

pays.sub <- structure(1:length(pays.sub), names = c(pays.sub))
facetGroup <- structure(c(1:5, rep(6, 4)), names = names(pays.sub)[c(1:5, rep(9, 4))])
# # recompute 'autre pays européen'
# data.sub[data.sub[,1] == 'Autre pays européen','value'] <- as.vector(by(data.sub, data.sub$variable, function(dd) {
# 	dd[dd[,1] == 'Europe','value'] - sum(dd[dd[,1] %in% names(pays.sub)[-length(pays.sub)],'value'], na.rm = T)
# }))

data.sub <- data.sub[which(data.sub[,1] %in% names(pays.sub)),]
data.sub[,1] <- factor(data.sub[,1], levels = names(pays.sub))
data.sub$facetGroup <- factor(names(facetGroup)[match(data.sub[,1], names(pays.sub))], levels = unique(names(facetGroup)))

ggplot(data.sub, aes(variable / 100, value)) + geom_area(aes(fill = Nationalité), position = 'stack') + ggtheme_ygrid +
	facet_wrap ( ~ facetGroup, nrow = 3) + scale_fill_manual(values = swi_9palette) + scale_x_continuous("", labels=function(x) x*100,  expand = c(0.0,0.0)) +
	ggtitle("Nationalité des étrangers européens en Suisse 1850-2009") + paneltheme + scale_y_continuous("Population", labels=function(x) x/1000) +
	paneltheme + theme(legend.position = "top")

dev.off()
## ANNOTATIONS
# 1. Au début du XXème siècle, Les ressortissants des divers états allemands forment le pourcentage le plus élevé. Il s’agit essentiellement d’ouvriers artisans qui constituent, dans quelques villes, une proportion importante de tailleurs, de cordonniers et de charpentiers. Les Italiens s’imposent de plus en plus, une grande partie d’entre eux s’engage comme terrassiers et manœuvres dans la construction des chemins de fer et des premières usines hydro-électriques
# XXème siècle, les ressortissants allemands étaient essentiellement des artisans qui constituent, dans quelques villes, une proportion importante de tailleurs, de cordonniers et de charpentiers
# 2.


