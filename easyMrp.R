############################################################
# EASY MRP                                                 #
# Class methods for multilevel regression, imputation, and #
# post-stratification                                      #
#                                                          #
# Clifford Vickrey                                         #
############################################################

# basic S4 definition (no constructor/validity function is necessary, but you
# can add one!)
Mrp<-setClass(
    'Mrp',
    slots=c(
        geographies.path='character',
        state.fe.path='character',
        substate.fe.path='character',
        poll.path='character',
        post.strat.path='character',
        bin.path='character',
        output.path='character',
        mrp.model='character',
        weight.var='character',
        year.var='character',
        state.var='character',
        substate.var='character',
        geographies='data.frame',
        state.fe='data.frame',
        substate.fe='data.frame',
        poll='data.frame',
        half='data.frame',
        sim='data.frame',
        post.strat='data.frame',
        output='data.frame',
        estimator='list',
        model.path='character',
        dep.var='character',
        ranefs='list',
        post.strat.formula='list',
        population.parameter='list',
        cases='list',
        sample.mean='list',
        indiv.estimates='list',
        point.estimates='list',
        point.variance='list',
        sampling.error='numeric',
        mrp.error='numeric',
        wins='numeric',
        t.stat='numeric',
        p.stat='numeric',
        model.i='list',
        convert.underscore='logical'
    ),
    prototype=list(
        geographies.path='geographies.csv',
        state.fe.path='',
        substate.fe.path='',
        poll.path='poll.csv',
        post.strat.path='post.strat.csv',
        bin.path='',
        output.path='mrp.csv',
        mrp.model='',
        weight.var='weight',
        year.var='',
        state.var='state',
        substate.var='',
        geographies=data.frame(),
        state.fe=data.frame(),
        substate.fe=data.frame(),
        poll=data.frame(),
        half=data.frame(),
        sim=data.frame(),
        post.strat=data.frame(),
        output=data.frame(),
        estimator=list(mrp=NA,sim=NA),
        model.path='',
        dep.var='',
        ranefs=list(mrp=NA,sim=NA),
        post.strat.formula=list(mrp=NA,sim=NA),
        population.parameter=list(mrp=NA,sim=NA),
        cases=list(mrp=NA,sim=NA),
        sample.mean=list(mrp=NA,half=NA,sim=NA),
        indiv.estimates=list(mrp=NA,sim=NA),
        point.estimates=list(mrp=NA,sim=NA),
        point.variance=list(mrp=NA,sim=NA),
        sampling.error=c(),
        mrp.error=c(),
        wins=c(),
        t.stat=c(),
        p.stat=c(),
        model.i=list(mrp=0,sim=0),
        convert.underscore=TRUE
    )
)

# load the dataframe necessary for MRP simulation
setGeneric(name=
    'loadData',
    def=function(object,model.name)
        standardGeneric('loadData'),
)
setMethod(
    f='loadData',
    signature='Mrp',
    definition=function(object,model.name){
        # defaults (I manually set these, since R's assignment of default S4
        # method arguments is unpredictable)
        if(missing(model.name)) model.name<-'model'

        # make sure directory for the binaries exists
        dir.create(object@bin.path,showWarnings=FALSE)
            
        # convert the CSVs to binaries for later use, or load them if they
        # already exist
        for(x in c('geographies','state.fe','substate.fe','poll','post.strat')
        ){
            csv.path<-slot(object,paste(x,'path',sep='.'))
            if(
                csv.path==''|
                nrow(slot(object,x))
            ) next
            bin.path=paste(object@bin.path,paste(x,'rds',sep='.'),sep='/')
            if(file.exists(bin.path)){
                slot(object,x)<-readRDS(bin.path)
            }
            else{
                slot(object,x)<-read.csv(csv.path,stringsAsFactors=FALSE)
                if(x %in% c('state.fe','substate.fe','poll','post.strat'))
                    slot(object,x)<-
                        slot(object,x)[complete.cases(slot(object,x)),]
                if(object@convert.underscore) names(slot(object,x))<-gsub(
                '_','.',names(slot(object,x)),fixed=TRUE)
                saveRDS(slot(object,x),file=bin.path)
            }
        }
        
        # merge in state-level fixed effects
        if(object@state.fe.path!='') object<-
            mergeInAggregateData(object,'state')
        
        # merge in substate-level fixed effects
        if(object@substate.fe.path!='') object<-
            mergeInAggregateData(object,'substate')
        
        # output uses place data as a template and appends columns
        object<-resetOutput(object)
        
        # load the model if it exists
        object@model.path<-
            paste(object@bin.path,paste(model.name,'rds',sep='.'),sep='/')
        if(file.exists(object@model.path)) object@estimator$mrp<-
        readRDS(object@model.path)
        
        # return object
        object
    }
)

# merge in aggregate predictors at the appropriate level
setGeneric(
    name='mergeInAggregateData',
    def=function(object,level)
        standardGeneric('mergeInAggregateData')
)
setMethod(
    f='mergeInAggregateData',
    signature='Mrp',
    definition=function(object,level){
        by.vars<-if(object@year.var!='') c(object@year.var) else c()
        by.vars<-c(by.vars,slot(object,paste(level,'var',sep='.')))
        for(x in c('poll','post.strat'))
            slot(object,x)<-merge(
                slot(object,x),
                slot(object,paste(level,'fe',sep='.')),
                by.x=by.vars,
                by.y=by.vars
            )
        object
    }
)

# reset output dataframe
setGeneric(
    name='resetOutput',
    def=function(object)
        standardGeneric('resetOutput')
)
setMethod(
    f='resetOutput',
    signature='Mrp',
    definition=function(object){
        object@model.i<-list(mrp=0,sim=0)
        object@output<-object@geographies
        object
    }
)

# register the model
setGeneric(
    name='registerModel',
    def=function(object,model.type)
        standardGeneric('registerModel')
)
setMethod(
    f='registerModel',
    signature='Mrp',
    definition=function(object,model.type){
        # defaults
        if(missing(model.type)) model.type<-'mrp'
        
        # list of random intercepts
        object@ranefs[[model.type]]<-list()
        
        # normalize whitespace
        object@mrp.model<-gsub('[[:space:]]','',object@mrp.model)
        if(object@mrp.model=='') stop('You must specify a model!')
        
        # model intercept
        object@post.strat.formula[[model.type]]<-
            "fixef(object@estimator[[model.type]])['(Intercept)']"
        
        # list of predictors
        params<-unlist(strsplit(object@mrp.model,'+',fixed=TRUE))
        
        # extract, scrub dependent variable
        first.param<-unlist(strsplit(params[1],'~',fixed=TRUE))
        object@dep.var<-first.param[1]
        params[1]<-first.param[2]
        
        # create MRP weighting formula with the predictors
        for(x in params){
            # add random or fixed predictor to inv logit function
            random.predictor<-
                if(grepl('|',x,fixed=TRUE))
                    gsub('.{1}$','',unlist(strsplit(x,'|',fixed=TRUE))[2])
                else
                    NA
            to.append<-
                if(!is.na(random.predictor))
                    paste0(
                        'object@ranefs[[model.type]]$',
                        random.predictor,
                        '[object@post.strat$',
                        random.predictor,
                        ',1]'
                    )
                else
                    paste0(
                        "fixef(object@estimator[[model.type]])['",
                        x,
                        "']*object@post.strat$",
                        x
                    )
            object@post.strat.formula[[model.type]]<-
                paste(object@post.strat.formula[[model.type]],to.append,
                sep='+')
            
            # register random predictor; get its possible values
            if(!is.na(random.predictor)){
                cats<-length(unique(object@post.strat[[random.predictor]]))
                object@ranefs[[model.type]][[random.predictor]]<-array(0,
                c(cats,1))
                dimnames(object@ranefs[[model.type]][[random.predictor]])<-
                    list(c(1:cats),'effect')
            }
        }
        object
    }
)

# get number of observations in the jurisdiction
setGeneric(
    name='observationCount',
    def=function(object,model.type)
        standardGeneric('observationCount')
)
setMethod(
    f='observationCount',
    signature='Mrp',
    definition=function(object,model.type){
        # defaults
        if(missing(model.type)) model.type<-'mrp'
    
        slot.name<-if(model.type=='mrp') 'poll' else 'sim'
        level<-if(object@substate.var!='') 'substate' else 'state'
        x<-tapply(
            rep(1,length(slot(object,slot.name)[[slot(object,paste(level,
            'var',sep='.'))]])),
            slot(object,slot.name)[[slot(object,paste(level,'var',sep='.'))]],
            sum
        )
        object@cases[[model.type]]<-rep(0,nrow(object@geographies))
        for(i in c(1:length(object@cases[[model.type]])))
            object@cases[[model.type]][i]<-as.numeric(x[toString(i)])
        object@cases[[model.type]][is.na(object@cases[[model.type]])]<-0
        object
    }
)

# get sample mean (joint mean of dependent variable by desired jurisdiction)
setGeneric(
    name='sampleMean',
    def=function(object,model.type)
        standardGeneric('sampleMean')
)
setMethod(
    f='sampleMean',
    signature='Mrp',
    definition=function(object,model.type){
        # defaults
        if(missing(model.type)) model.type<-'mrp'
        
        slot.name<-if(model.type=='mrp') 'poll' else if (model.type=='sim')
        'sim' else 'half'
        level<-if(object@substate.var!='') 'substate' else 'state'
        x<-tapply(
            slot(object,slot.name)[[object@dep.var]],
            slot(object,slot.name)[[slot(object,paste(level,'var',sep='.'))]],
            mean
        )
        object@sample.mean[[model.type]]<-rep(NA,nrow(object@geographies))
        for(i in c(1:length(object@sample.mean[[model.type]])))
            object@sample.mean[[model.type]][i]<-as.numeric(x[toString(i)])
        object
    }
)

# run the model and save the binary
setGeneric(
    name='runModel',
    def=function(object,model.type,save.model)
        standardGeneric('runModel')
)
setMethod(
    f='runModel',
    signature='Mrp',
    definition=function(object,model.type,save.model){
        # defaults
        if(missing(model.type)) model.type<-'mrp'
        if(missing(save.model)) save.model<-TRUE
        
        eval(
            parse(
                text=paste0(
                    'object@estimator[[model.type]]<-glmer(formula=',
                    object@mrp.model,
                    ",family=binomial(link='logit'),data=object@",
                    if(model.type=='mrp') 'poll' else 'sim',
                    ')'
                ),
                n=1
            )
        )
        if(save.model) saveRDS(object@estimator[[model.type]],
        file=object@model.path)
        object
    }
)

# fill in random intercepts missing in the poll data
setGeneric(
    name='fillMissingIntercepts',
    def=function(object,model.type)
        standardGeneric('fillMissingIntercepts')
)
setMethod(
    f='fillMissingIntercepts',
    signature='Mrp',
    definition=function(object,model.type){
        # defaults
        if(missing(model.type)) model.type<-'mrp'
    
        slot.name<-if(model.type=='mrp') 'poll' else 'sim'
        for(predictor in names(ranef(object@estimator[[model.type]]))){
            vals<-sort(unique(slot(object,slot.name)[[predictor]]))
            for(val in vals)
                object@ranefs[[model.type]][[predictor]][val,1]<-
                ranef(object@estimator[[model.type]])[[predictor]][
                match(val,vals),1]
        }
        object
    }
)

# probability adjustment using Census data
setGeneric(
    name='probabilityAdjustment',
    def=function(object,model.type)
        standardGeneric('probabilityAdjustment')
)
setMethod(
    f='probabilityAdjustment',
    signature='Mrp',
    definition=function(object,model.type){
        # defaults
        if(missing(model.type)) model.type<-'mrp'
        
        eval(
            parse(
                text=paste0(
                        'object@indiv.estimates[[model.type]]<-',
                        'as.vector(invlogit(',
                        object@post.strat.formula[[model.type]],
                        '))*object@post.strat$',
                        object@weight.var
                    ),
                n=1
            )
        )
        object
    }
)

# MRP weighted point estimate by jurisdiction
setGeneric(
    name='pointEstimate',
    def=function(object,model.type)
        standardGeneric('pointEstimate')
)
setMethod(
    f='pointEstimate',
    signature='Mrp',
    definition=function(object,model.type){
        # defaults
        if(missing(model.type)) model.type<-'mrp'
        
        object@point.estimates[[model.type]]<-
            as.vector(
                tapply(
                    object@indiv.estimates[[model.type]],
                    object@post.strat[[
                        if(object@substate.var!='')
                        object@substate.var
                        else
                        object@state.var
                    ]],
                    sum,
                    na.rm=TRUE
                )
            )/
            as.vector(
                tapply(
                    object@post.strat[[object@weight.var]],
                    object@post.strat[[
                        if(object@substate.var!='')
                        object@substate.var
                        else
                        object@state.var
                    ]],
                    sum,
                    na.rm=TRUE
                )
            )
        object
    }
)

# weighted variance by jurisdiction
setGeneric(
    name='weightedVariance',
    def=function(object,model.type)
        standardGeneric('weightedVariance')
)
setMethod(
    f='weightedVariance',
    signature='Mrp',
    definition=function(object,model.type){
        # defaults
        if(missing(model.type)) model.type<-'mrp'
        
        object@point.variance[[model.type]]<-
            as.vector(
                by(
                    data.frame(
                        y=object@indiv.estimates[[model.type]]/
                        object@post.strat[[object@weight.var]],
                        z=object@post.strat[[object@weight.var]]
                    ),
                    object@post.strat[[
                        if(object@substate.var!='')
                        object@substate.var
                        else
                        object@state.var
                    ]],
                    function(x) wtd.var(x$y,x$z)
                )
            )
        object
    }
)

# save the estimates!
setGeneric(
    name='saveEstimates',
    def=function(object,model.type,save.csv)
        standardGeneric('saveEstimates')
)
setMethod(
    f='saveEstimates',
    signature='Mrp',
    definition=function(object,model.type,save.csv){
        # defaults
        if(missing(model.type)) model.type<-'mrp'
        if(missing(save.csv)) save.csv<-TRUE
    
        object@model.i[[model.type]]<-object@model.i[[model.type]]+1
        for(x in c('population.parameter','cases','sample.mean',
        'point.estimates','point.variance')){
            if(model.type=='mrp'&x=='population.parameter')
                next
            else
                object@output[[paste(model.type,x,
                object@model.i[[model.type]],sep='.')]]<-
                slot(object,x)[[model.type]]
        }
        if(save.csv) write.csv(object@output,file=object@output.path,
        row.names=FALSE)
        object
    }
)

# clear data except output
setGeneric(
    name='clearData',
    def=function(object)
        standardGeneric('clearData')
)
setMethod(
    f='clearData',
    signature='Mrp',
    definition=function(object){
        for(x in c('geographies','state.fe','substate.fe','poll','sim',
        'half','post.strat'))
            slot(object,x)<-data.frame()
        object@estimator<-list(mrp=NA,sim=NA)
        object
    }
)

# run the MRP
setGeneric(
    name='runMrp',
    def=function(object,model.name)
        standardGeneric('runMrp')
)
setMethod(
    f='runMrp',
    signature='Mrp',
    definition=function(object,model.name){
        # defaults
        if(missing(model.name)) model.name='model'
    
        # load data
        cat('MRP procedure\nLoading data...\n')
        object<-loadData(object,model.name)

        # register the model
        cat('Registering model...\n')
        object<-registerModel(object)

        # add the observations by jusridiction
        cat('Tabulating observations by jurisdiction...\n')
        object<-observationCount(object)

        # add the mean by jusridiction
        cat('Tabulating sample means by jurisdiction...\n')
        object<-sampleMean(object)

        # run the model, if it's not already saved
        cat('Converging model...\n')
        if(typeof(object@estimator$mrp)!='S4') object<-runModel(object)

        # fix the missing random effects to account for missing state
        # intercepts
        cat('Filling missing random intercepts...\n')
        object<-fillMissingIntercepts(object)

        # post-stratify data
        cat('Post-stratifying model estimates...\n')
        object<-probabilityAdjustment(object)

        # generate point estimates
        cat('Generating point estimates by jurisdiction...\n')
        object<-pointEstimate(object)

        # generate point estimates of variance
        cat('Computing MRP variance by jurisdiction...\n')
        object<-weightedVariance(object)

        # save the MRP point estimates
        cat('Saving...\n')
        object<-saveEstimates(object)
        
        # return object
        cat('Done!\n\n')
        object
    }
)

# split-level validation simulation
setGeneric(
    name='validationSim',
    def=function(object,sample.ratio)
        standardGeneric('validationSim')
)
setMethod(
    f='validationSim',
    signature='Mrp',
    definition=function(object,sample.ratio){
        # defaults
        if(missing(sample.ratio)) sample.ratio<-.05
        
        # welcome message
        cat(
            'MRP split-level validation procedure using',
            sprintf('%.2f%%',sample.ratio*100),
            'sample of the survey...\n'
        )

        # randomly split the data in half
        s<-sample(nrow(object@poll),round(nrow(object@poll)/2),replace=FALSE)
        object@half<-object@poll[s,]
        object@sim<-object@poll[-s,]
        
        # sample a portion of one of the halves
        if(sample.ratio*2<1)
            object@sim<-object@sim[
                sample(
                    nrow(object@sim),
                    round(nrow(object@sim)*sample.ratio*2),
                    replace=FALSE
                ),
            ]


        # refresh random effects
        object<-registerModel(object,'sim')

        # get descriptives
        object<-observationCount(object,'sim')
        object<-sampleMean(object,'half')
        object@population.parameter$sim<-
            object@sample.mean$half
        object<-sampleMean(object,'sim')
        
        # run the simulation and save the output
        object<-runModel(object,'sim',FALSE)
        object<-fillMissingIntercepts(object,'sim')
        object<-probabilityAdjustment(object,'sim')
        object<-pointEstimate(object,'sim')
        object<-weightedVariance(object,'sim')
        object<-saveEstimates(object,'sim')
        
        # sampling error
        object@sampling.error<-abs(
            object@sample.mean$sim-
            object@population.parameter$sim
        )
        
        # MRP error
        object@mrp.error<-abs(
            object@point.estimates$sim-
            object@population.parameter$sim
        )
        
        # wins
        object@wins<-as.numeric(
            object@mrp.error<
            object@sampling.error
        )
        
        # Student's t test
        ttest<-t.test(
            object@sampling.error,
            object@mrp.error
        )
        object@t.stat<-as.numeric(ttest$statistic)
        object@p.stat<-as.numeric(ttest$p.value)
        
        # show summary stats
        cat('\nResults:\n')
        cat('Sampling error: ',sprintf('%1.2f',mean(object@sampling.error,
        na.rm=TRUE)),'\n',sep='')
        cat('MRP error: ',sprintf('%1.2f',mean(object@mrp.error,na.rm=TRUE)),
        '\n',sep='')
        cat('Win %: ',sprintf('%.2f%%',mean(object@wins,na.rm=TRUE)),'\n\n',
        sep='')
        cat('t value: ',object@t.stat,'\n\n',sep='')
        cat('pr(|T| > |t|): ',object@p.stat,'\n\n',sep='')
        
        # return object
        object
    }
)

#####################
# helpful functions #
#####################

# get working directory in any environment
get.wd<-function()
    dirname(
        tryCatch(
            normalizePath(parent.frame(3)$ofile),
            error=function(e)
                normalizePath(
                    unlist(
                        strsplit(
                            commandArgs()[
                                grep('^--file=',commandArgs())
                            ],
                            '='
                        )
                    )[2]
                )
        )
    )

############
# includes #
############

suppressWarnings(suppressPackageStartupMessages(require(arm)))
suppressWarnings(suppressPackageStartupMessages(require(Hmisc)))
