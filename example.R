############################################################
# MRP EXAMPLE                                              #
# Impute party ID at the county level using the 2009 CCES  #
#                                                          #
# Clifford Vickrey                                         #
############################################################

# include library
source('/easyMrp/easyMrp.R')

# new MRP object
example.mrp<-Mrp()

# variable names
example.mrp@weight.var<-'pop'
example.mrp@state.var<-'state'
example.mrp@substate.var<-'county'

# path slots
wd<-get.wd()
example.mrp@geographies.path<-paste(wd,'example.csv','counties.csv',sep='/')
example.mrp@state.fe.path<-paste(wd,'example.csv','state.fe.csv',sep='/')
example.mrp@substate.fe.path<-paste(wd,'example.csv','county.fe.csv',sep='/')
example.mrp@poll.path<-paste(wd,'example.csv','cces.truncated.csv',sep='/')
example.mrp@post.strat.path<-paste(wd,'example.csv','post.strat.csv',sep='/')
example.mrp@bin.path<-paste(wd,'example.bin',sep='/')
example.mrp@output.path<-paste(wd,'example.mrp.csv',sep='/')

# MRP model
# model Democrat (Y) as a function of:
#
# region (ICPSR region)
#
# state (1:51)
#
# state-level labor union rates
#
# whether states have right-to-work laws (as of 2007)
#
# county-level Republican presidential vote in 2004
#
# county population density
#
# sex (0 = male; 1 = female)
#
# race (1 = non-Latino white; 2 = African-American; 3 = Latino; 4 = Asian or
# Pacific Islander; 5 = Native American/Alaskan; 6 = more than one race; 7 =
# other)
#
# interaction term of sex and race
# 
# education (1 = less than high school; 2 = highschool/GED; 3 = some college
# or Associate degree; 4 = Bachelor's degree or higher)
#
example.mrp@mrp.model<-
    paste0(
        'democrat~(1|region)+(1|state)+state.labor.union+state.rtw+',
        '(1|county)+county.bush.vote04+county.pop.density+sex+(1|race)+',
        '(1|sex.by.race)+(1|educ)'
    )

# run!
example.mrp<-runMrp(example.mrp)

# sim
sim<-validationSim(example.mrp,.05)