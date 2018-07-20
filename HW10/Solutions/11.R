
############ 
### the more the population lives in the city the higher life expectancy is

cor_tester("SP.URB.TOTL.IN.ZS","SP.DYN.LE00.IN", years = c(1960,2017))$cor_test

### right now
cor_tester("SP.URB.TOTL.IN.ZS","SP.DYN.LE00.IN")$plot

# B
############ 
### Number of Children a woman gives birth to and BS degree
### negative relationship

cor_tester("SE.TER.CUAT.BA.FE.ZS","SP.DYN.TFRT.IN", years = c(1960,2017))$cor_test

### right now
cor_tester("SE.TER.CUAT.BA.FE.ZS","SP.DYN.TFRT.IN")$plot

# C
############ 
### Number of Reasearchers in a million and GDP per Capita relationship
### spearman correlation of 0.8

cor_tester("SP.POP.SCIE.RD.P6","NY.GDP.PCAP.KD", years = c(1960,2017))$cor_test

### right now
cor_tester("SP.POP.SCIE.RD.P6","NY.GDP.PCAP.KD")$plot

