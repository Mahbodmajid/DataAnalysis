library(gutenbergr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(stringr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(highcharter)
library(knitr)
library(kableExtra)

ThePickwickPapers = gutenberg_download(580)
OliverTwist = gutenberg_download(730)
NicholasNickleby = gutenberg_download(967)
TheOldCuriosityShop = gutenberg_download(700)
BarnabyRudge = gutenberg_download(917)
MartinChuzzlewit = gutenberg_download(968)
DombeyandSon = gutenberg_download(821)
DavidCopperfield =gutenberg_download(766)
BleakHouse =gutenberg_download(1023)
HardTimes =gutenberg_download(786)
LittleDorrit =gutenberg_download(963)
ATaleofTwoCities = gutenberg_download(98)
GreatExpectations = gutenberg_download(1400)
OurMutualFriend = gutenberg_download(883)
TheMysteryofEdwinDrood =gutenberg_download(564)

book_ids = c(580, 730, 967, 700, 917, 968, 821, 766, 1023, 786, 963, 98, 1400, 883, 564)

LeMiserables <-gutenberg_download(135)

gutenberg_metadata %>% filter(gutenberg_id %in% book_ids) -> book_meta

#setwd("Desktop/96-97-2/Data Analysis/HW/HW8/")
dickens_picture <- "images/dickens1_1.png"

dickens <- gutenberg_download(c(98, 1400, 46, 730, 786))

