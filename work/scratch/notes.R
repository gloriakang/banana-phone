(ff <- factor(substring("statistics", 1:10, 1:10), levels = letters))
as.integer(ff)      # the internal codes
(f. <- factor(ff))  # drops the levels that do not occur
ff[, drop = TRUE]   # the same, more transparently


## --- Integer levelsMap ---

(f <- factor(sample(letters, size=20, replace=TRUE)))
(mapInt <- mapLevels(f))

## Integer to factor
(int <- as.integer(f))
(mapLevels(int) <- mapInt)
all.equal(int, f)

## Remap levels of a factor
(fac <- factor(as.integer(f)))
(mapLevels(fac) <- mapInt) # the same as levels(fac) <- mapInt
all.equal(fac, f)

## --- Character levelesMap ---

f1 <- factor(letters[1:10])
f2 <- factor(letters[5:14])

## Internal codes are the same, but levels are not
as.integer(f1)
as.integer(f2)

## Get character levelsMaps and combine them
mapCha1 <- mapLevels(f1, codes=FALSE)
mapCha2 <- mapLevels(f2, codes=FALSE)
(mapCha <- c(mapCha1, mapCha2))

## Remap factors
mapLevels(f1) <- mapCha # the same as levels(f1) <- mapCha
mapLevels(f2) <- mapCha # the same as levels(f2) <- mapCha

## Internal codes are now "consistent" among factors
as.integer(f1)
as.integer(f2)

## ---------

as.numeric(factor(5:10)) # not what you'd expect
f <- factor(1:5)
## what you typically meant and want:
as.numeric(as.character(f))
## the same, considerably (for long factors) more efficient:
as.numeric(levels(f))[f]

## -----------


#creating dataframe    
df1=data.frame(c("Mary","Sarah","Linda","Mark","Shaun","Jo"),c(1,2,3,4,5,6),c(2,2,2,1,1,2))
names(df1)=c("a","b","c")

#setting levels and labels for questions b and c
blevels=c(1,2,3,4,5,6)
blabels=c("bike","walk","car","bus","train","subway")

clevels=c(1,2)
clabels=c("male","female")

#creating labelled data.frame
df2 = df1

#applying labels
df2$b<-factor(df1$b,blevels,blabels)
df2$c<-factor(df1$c,clevels,clabels)


