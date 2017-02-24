# EasyMRP

## MRP Overview

Commercial and social-scientific surveys are almost invariably national in scope and do not contain representative samples of respondents at the level of state or below. In light of this, researchers have proposed various simulation techniques to impute aggregate attitudes and traits at the subnational level. Multilevel regression and post-stratification, better known as MRP, is the latest and best of such techniques. MRP has been proven to produce valid and reliable estimates of jurisdictional public opinion from merely individual commercial polls.

MRP proceeds in two steps. First, using national survey data, a multilevel model estimates an unobserved individual-level variable using demographic (for instance, age and income) and geographic (for instance, state and county) predictors. The model generates predictions for every demographic-geographic group in the survey data. In the second step, the predicted group propensities are reweighted by the groups’ demographic composition per each jurisdiction. This probability adjustment distinguishes MRP from raw survey disaggregation, since it corrects the random and nonrandom sources of measurement error that afflict raw disaggregated sample means by jurisdiction.

## EasyMRP

EasyMRP allows social scientists to easily impute MRP estimates and run diagnostics on them. After exporting the requisite datasets (list of jurisdiction, poll data, post-stratification data, aggregate fixed effects) in third-party applications into CSVs, declare an MRP object with the multilevel model, paths to the datasets, and a few other properties. Use the runMrp() method, which will run the model, produce the MRP point estimates, and save A) the estimated parameters, B) the MRP point estimates, C) the raw disaggregated sample means by jurisdiction, and D) the weighted variance of the MRP point estimates (theoretically useful for many research questions). Run the validationSim() method to test the robustness of your results. And that's it!

EasyMRP is designed for ease of use and descriptive reporting. It's ideal for people who are comfortable creating and exporting datasets in Stata, SPSS, or SAS but find R's arcane syntax a bit daunting. Consequently, it may (in its unchanged form) not be as flexible as some R power users would like. If you can think of ways to improve it, please let me know!

## Required packages

arm (for multilevel/hierchical modeling): https://cran.r-project.org/web/packages/arm/index.html
Hmisc (for weighted variance): https://cran.r-project.org/web/packages/Hmisc/index.html

## Properties

Declare an Mrp object:

foo<-Mrp()

Define the following properties with the S4 slot selector (foo@bar<-x):

    geographies.path: path to a CSV containing geography identifiers (see example)
    poll.path: path to a CSV containing the survey data for MRP disaggregation
    post.strat.path: path to a CSV containing post-stratification for MRP's probability adjustment
    bin.path: directory with which to store compressed R binaries for later use. For speed, EasyMRP will convert each CSV, as well as the multilevel model, into an R binary and load these for future MRP procedures. EasyMRP also stores the multilevel model in this directory so that R will not need to reconverge the model if you run the same script again
    coef.path: path to a CSV that will contain the model's random and fixed effect estimates, along with significance levels
    output.path: path to a CSV that will contain the MRP point estimates, weighted variance of the MRP estimates, and raw disaggregated sample means
    mrp.model: model for the multilevel modeling step of MRP (e.g. the string 'y ~ x + (1|z)' = estimate y as a function of fixed effect x and a random intercept for each category of z)
    weight.var: name of the weighting variable in the post-stratification dataset. Defaults to "weight"

Define the following optional properties:

    state.fe.path: path to a CSV containing state-level aggregate fixed effects, which EasyMRP will merge into both the poll and post-stratification dataframes. Analysis of the reliability of MRP imputation suggests that the incorporation of fixed effects substantially increases the quality of MRP estimates 
    substate.fe.path: path to a CSV containing substate-level (Congressional district, county, metropolitan statistical area, etc.) aggregate fixed effects, which EasyMRP will merge into both the poll and post-stratification dataframes
    year.var: if fixed effects are being merged in by year, define the year variable
    state.var: if using state-level fixed effects OR estimating MRP at the level of state, define the state variable. Should be constant across all dataframes. Defaults to "state"
    substate.var: if using substate-level fixed effects OR estimate MRP at a sub-state jurisdiction, define the substate variable. Should be constant across all dataframes
    convert.underscore: if TRUE (the default), it converts Stata-friendly column names (foo_bar) into R-friendly names (foo.bar)

If you want to use EasyMRP with existing dataframes, simply define foo@poll<-df, foo@post.strat<-df, etc. and EasyMRP will ignore the paths.

## Class methods

To run the MRP procedure and output the public opinion estimates (to foo@output.path) and model data (to foo@coef.path), use the runMrp() method:

    foo<-runMrp(foo)

EasyMRP also contains a robustness check, vital because MRP (like other simulations) reports no standard errors. This is a split-level validation technique, which (naturally) splits the survey in the multilevel modeling stage of MRP in two. The jurisditional sample of means of one half are treated as the population parameter, and a portion of the other half (say, 5%) is used to impute MRP estimates. If, for a given county, the MRP imputation is closer to the simulated population parameter than the 5% sample mean, then MRP "wins." If not, MRP "loses." EasyMRP reports the simulated sampling error, the simulated MRP error, the win percentage of MRP vis-a-vis raw survey disaggregation, and a t test where H0 = MRP is a worse measurement of public opinion than the sample mean. It also appends the results of the simulation to foo@output.path. To run the validation procedure, use the validationSim() method:

    foo<-validationSim(foo)

Example output (two simulations):

    MRP split-level validation procedure using 5.00% sample of the survey...
    
    Results of simulation #1:
    Sampling error: 0.39
    MRP error: 0.32
    Comparisons: 314
    Win %: 73.01%
    t value: 3.866925
    pr(|T| > |t|): 0.0001297358
    
    Results of all simulations so far:
    Sampling error: 0.39
    MRP error: 0.32
    Comparisons: 314
    Win %: 73.01%
    t value: 3.866925
    pr(|T| > |t|): 0.0001297358
    
    MRP split-level validation procedure using 5.00% sample of the survey...
    
    Results of simulation #2:
    Sampling error: 0.40
    MRP error: 0.32
    Comparisons: 345
    Win %: 74.78%
    t value: 4.710483
    pr(|T| > |t|): 3.370071e-06
    
    Results of all simulations so far:
    Sampling error: 0.40
    MRP error: 0.32
    Comparisons: 659
    Win %: 73.89%
    t value: 6.083234
    pr(|T| > |t|): 1.827467e-09

## Example

See example.R, also in this Git repository.

## License

EasyMRP is distributed for free using the GNU general public license.

## Contact

Please direct any questions, comments, or ideas to Clifford Vickrey <clifforddavidvickrey@gmail.com>.