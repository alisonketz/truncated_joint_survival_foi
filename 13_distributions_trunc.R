#######################################################################
###
###   Likelihoods for each kind of data listed below
###
#######################################################################

# d_fit_sus_cens_posttest 2
# d_fit_sus_cens_postno/d_fit_endlive 4
# d_fit_sus_mort_posttest 6
# d_fit_sus_mort_postno 8
# d_fit_icap_cens 10
# d_fit_icap_mort 12
# d_fit_rec_neg_cens_posttest 14
# d_fit_rec_neg_cens_postno 16
# d_fit_rec_neg_mort 18
# d_fit_rec_pos_cens 20
# d_fit_rec_pos_mort 22
# d_fit_idead 24
#d_fit_hunt  

#######################################################################
###
###   User defined distribution for likelihood for
###   Uninfected radio-marked deer right censor:
###   Test neg at cap and censoring
###
###   d_fit_sus_cens_posttest
###   Overleaf Equation 2
###
#######################################################################

dSusCensTest <- nimble::nimbleFunction(
    run = function( ### argument type declarations
                   x = integer(0),
                   n_samples = integer(0),
                   e = double(1), # e, age of entry
                   r = double(1), # r, age of last known alive
                   sex = double(1),
                   age2date = double(1),
                   beta_male = double(0),
                   beta0_sus = double(0),
                   age_effect_surv = double(1),
                   period_effect_surv = double(1),
                   f_age_foi = double(1),
                   m_age_foi = double(1),
                   age_lookup_f = double(1),
                   age_lookup_m = double(1),
                #    period_lookup_foi = double(1),
                #    f_period_foi = double(1),
                #    m_period_foi = double(1),
                   sect = double(1),
                   space = double(1),
                   log = double()) {

        # starttime the loop through individuals
        sumllik <- 0
        for (i in 1:n_samples) {
            # intitialize vectors
            lam_foi <- 0
            lam_sus <- 0

            #############################################
            # preliminary hazards for the likelihood
            #############################################
            if (sex[i] == 0) { # age loops for females
                for (j in e[i]:(r[i] - 1)) {
                    # sum up foi
                    lam_foi <- lam_foi +
                        exp(space[sect[i]] +
                            f_age_foi[age_lookup_f[j]])
                            # f_period_foi[period_lookup_foi[age2date[i] + j]])
                   
                    lam_sus <- lam_sus +
                            exp(beta0_sus +
                                age_effect_surv[j] +
                                period_effect_surv[age2date[i] + j])
                    
                }
            } else { # age loops for males
                for (j in e[i]:(r[i] - 1)) {
                    # sum up foi
                    lam_foi <- lam_foi +
                        exp(space[sect[i]] +
                            m_age_foi[age_lookup_m[j]])
                            # m_period_foi[period_lookup_foi[age2date[i] + j]])
                    lam_sus <- lam_sus +
                        exp(beta0_sus +
                            age_effect_surv[j] +
                            period_effect_surv[age2date[i] + j] +
                            beta_male)
                }
            }
            #######################################
            ### calculating the joint likelihood
            #######################################

            sumllik <- sumllik - lam_sus - lam_foi
        } # end loop over individuals

        returnType(double(0))
        if (log) {
            return(sumllik)
        } else {
            return(exp(sumllik))
        } ## return log-likelihood
    }
)


nimble::registerDistributions(list(
    dSusCensTest = list(
        BUGSdist = "dSusCensTest(n_samples,e,r,sex,age2date,beta_male,beta0_sus,age_effect_surv,period_effect_surv,f_age_foi,m_age_foi,age_lookup_f,age_lookup_m,sect,space)",
        types = c(
            "value = integer(0)",
            "n_samples = integer(0)",
            "e = double(1)",
            "r = double(1)",
            "sex = double(1)",
            "age2date = double(1)",
            "beta_male = double(0)",
            "beta0_sus = double(0)",
            "age_effect_surv = double(1)",
            "period_effect_surv = double(1)",
            "f_age_foi = double(1)",
            "m_age_foi = double(1)",
            "age_lookup_f = double(1)",
            "age_lookup_m = double(1)",
            # "period_lookup_foi = double(1)",
            # "f_period_foi = double(1)",
            # "m_period_foi = double(1)",
            "sect = double(1)",
            "space = double(1)",
            "log = double(0)"
        ),
        discrete = TRUE
    )
))


### for a user-defined distribution
assign("dSusCensTest", dSusCensTest, envir = .GlobalEnv)

# starttime <- Sys.time()
# test <- dSusCensTest(
#         x = 1,
# 		n_samples = nrow(d_fit_sus_cens_posttest),
#         e = d_fit_sus_cens_posttest$left_age_e,
#         r = d_fit_sus_cens_posttest$right_age_r,
#         sex = d_fit_sus_cens_posttest$sex,
#         age2date = sus_cens_posttest_age2date,
#         beta_male = beta_male,
#         beta0_sus = beta0_survival_sus,
#         age_effect_surv = age_effect_survival_test,
#         period_effect_surv = period_effect_surv,
#         f_age_foi = f_age_foi,
#         m_age_foi = m_age_foi,
#         age_lookup_f = age_lookup_f,
#         age_lookup_m = age_lookup_m,
#         # period_lookup_foi = period_lookup_foi,
#         # f_period_foi = f_period_foi,
#         # m_period_foi = m_period_foi,
#         sect = d_fit_sus_cens_posttest$study_area,
#         space = c(0,-.55),
#         log = TRUE
#         )
# (end<- Sys.time()-starttime)
# test


#######################################################################
###
###   User defined distribution for likelihood for
###   Uninfected radio-marked deer right censored:
###   Test neg at cap and censoring
###
###   d_fit_sus_cens_postno
###   d_fit_endlive
###
###   Overleaf Equation (4)
###
#######################################################################

dSusCensNo <- nimble::nimbleFunction(
    run = function( ### argument type declarations
                   x = integer(0),
                   n_samples = integer(0), # number of samples in dataset
                   e = double(1), # e, age of entry
                   r = double(1), # r, age of last known alive
                   sex = double(1),
                   age2date = double(1),
                   beta_male = double(0),
                   beta0_sus = double(0),
                   beta0_inf = double(0),
                   age_effect_surv = double(1),
                   period_effect_surv = double(1),
                   f_age_foi = double(1),
                   m_age_foi = double(1),
                   age_lookup_f = double(1),
                   age_lookup_m = double(1),
                #    period_lookup_foi = double(1),
                #    f_period_foi = double(1),
                #    m_period_foi = double(1),
                   space = double(1),
                   sect = double(1),
                   log = double()) {
        sumllik <- 0 # intialize log-likelihood
        for (i in 1:n_samples) {
            lam_foi <- nimNumeric(r[i] - 1)
            lam_sus <- nimNumeric(r[i] - 1)
            lam_inf <- nimNumeric(r[i]- 1)
            lik_temp <- 0

            #############################################
            # preliminary hazards for the likelihood
            #############################################
            if (sex[i] == 0) {
                lam_foi[e[i]:(r[i] - 1)] <- exp(space[sect[i]] +
                    f_age_foi[age_lookup_f[e[i]:(r[i]-1)]])

                lam_sus[e[i]:(r[i] - 1)] <- exp(beta0_sus +
                                            age_effect_surv[e[i]:(r[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                                       (r[i] - 1 + age2date[i])])
                lam_inf[e[i]:(r[i] - 1)] <- exp(beta0_inf +
                        age_effect_surv[e[i]:(r[i] - 1)] +
                        period_effect_surv[(age2date[i] + e[i]):
                                           (age2date[i] + r[i] - 1)])
            } else{
                lam_foi[e[i]:(r[i]-1)] <- exp(space[sect[i]] +
                    m_age_foi[age_lookup_m[e[i]:(r[i]-1)]])

                lam_sus[e[i]:(r[i] - 1)] <- exp(beta0_sus +
                                            age_effect_surv[e[i]:(r[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                                       (r[i] - 1 + age2date[i])] + beta_male)
                lam_inf[e[i]:(r[i] - 1)] <- exp(beta0_inf +
                        age_effect_surv[e[i]:(r[i] - 1)] +
                        period_effect_surv[(age2date[i] + e[i]):
                                           (age2date[i] + r[i] - 1)] + beta_male)
            }

            lik_temp <- (1 - exp(lam_foi[e[i]])) * exp(-sum(lam_inf[e[i]:(r[i] - 1)]))

            if ((r[i] - e[i]) > 1) {
                for(k in (e[i] + 1):(r[i] - 1)) {
                    lik_temp <- lik_temp +
                                (1 - exp(lam_foi[k])) *
                                exp(-sum(lam_inf[k:(r[i] - 1)])) *
                                exp(-sum(lam_foi[e[i]:(k - 1)])) *
                                exp(-sum(lam_sus[e[i]:(k - 1)])) 
                }
            }
            #######################################
            ### calculating the joint likelihood
            #######################################
            sumllik <- sumllik +
                       log(
                        exp(-sum(lam_foi[e[i]:(r[i] - 1)])) *
                        exp(-sum(lam_sus[e[i]:(r[i] - 1)])) +
                        lik_temp
                       )
        }
        returnType(double(0))
        if (log) {
            return(sumllik)
        } else {
            return(exp(sumllik))
        } ## return log-likelihood
    }
)

nimble::registerDistributions(list(
    dSusCensNo = list(
        BUGSdist = "dSusCensNo(n_samples,e,r,sex,age2date,beta_male,beta0_sus,beta0_inf,age_effect_surv,period_effect_surv,f_age_foi,m_age_foi,age_lookup_f,age_lookup_m,space,sect)",
        types = c(
            "value=integer(0)",
            "n_samples = integer(0)",
            "e = double(1)",
            "r = double(1)",
            "sex = double(1)",
            "age2date = double(1)",
            "beta_male = double(0)",
            "beta0_sus = double(0)",
            "beta0_inf = double(0)",
            "age_effect_surv = double(1)",
            "period_effect_surv = double(1)",
            "f_age_foi = double(1)",
            "m_age_foi = double(1)",
            "age_lookup_f = double(1)",
            "age_lookup_m = double(1)",
            # "period_lookup_foi=double(1)",
            # "f_period_foi=double(1)",
            # "m_period_foi=double(1)",
            "space = double(1)",
            "sect = double(1)",
            "log = double()"
        ),
        discrete = TRUE
    )
))

### for a user-defined distribution
assign("dSusCensNo", dSusCensNo, envir = .GlobalEnv)

# starttime <- Sys.time()
# test <- dSusCensNo(
#         x = 1,
#         n_samples = nrow(d_fit_sus_cens_postno),
#         e = d_fit_sus_cens_postno$left_age_e,
#         r = d_fit_sus_cens_postno$right_age_r,
#         sex = d_fit_sus_cens_postno$sex,
#         age2date = sus_cens_postno_age2date,
#         beta_male = beta_male,
#         beta0_sus = beta0_survival_sus,
#         beta0_inf = beta0_survival_inf,
#         age_effect_surv = age_effect_survival_test,
#         period_effect_surv = period_effect_surv,
#         f_age_foi = f_age_foi,
#         m_age_foi = m_age_foi,
#         age_lookup_f = age_lookup_f,
#         age_lookup_m = age_lookup_m,
#         # period_lookup_foi = period_lookup_foi,
#         # f_period_foi = f_period_foi,
#         # m_period_foi = m_period_foi,
#         space = c(0,-.55),
#         sect = d_fit_sus_cens_postno$study_area,
#         log = TRUE
#         )
# (end<- Sys.time()-starttime)
# test

# starttime <- Sys.time()
# test2 <- dSusCensNo(
#         x = 1,
#         n_samples = nrow(d_fit_endlive),
#         e = d_fit_endlive$left_age_e,
#         r = d_fit_endlive$right_age_r,
#         sex = d_fit_endlive$sex,
#         age2date = endlive_age2date,
#         beta_male = beta_male,
#         beta0_sus = beta0_survival_sus,
#         beta0_inf = beta0_survival_inf,
#         age_effect_surv = age_effect_survival_test,
#         period_effect_surv = period_effect_surv,
#         f_age_foi = f_age_foi,
#         m_age_foi = m_age_foi,
#         age_lookup_f = age_lookup_f,
#         age_lookup_m = age_lookup_m,
#         # period_lookup_foi = period_lookup_foi,
#         # f_period_foi = f_period_foi,
#         # m_period_foi = m_period_foi,
#         space = c(0,-.55),
#         sect = d_fit_endlive$study_area,
#         log = TRUE
#         )
# (end<- Sys.time()-starttime)
# test2

#######################################################################
###
###   User defined distribution for likelihood for
###   uninfected radio-marked deer mortalities:
###   test neg at cap and tested mort
###
###   d_fit_sus_mort_posttest
###
###   Overleaf Equation (6)
###
#######################################################################

dSusMortTest <- nimble::nimbleFunction(
    run = function( ### argument type declarations
                   x = double(0),
                   n_samples = integer(0), # number of samples in dataset
                   e = double(1), # e, age of entry
                   r = double(1), # r, age of last known alive
                   s = double(1), # s, age of known mortality
                   sex = double(1),
                   age2date = double(1),
                   beta_male = double(0),
                   beta0_sus = double(0),
                   age_effect_surv = double(1),
                   period_effect_surv = double(1),
                   f_age_foi = double(1),
                   m_age_foi = double(1),
                   age_lookup_f = double(1),
                   age_lookup_m = double(1),
                #    period_lookup_foi = double(1),
                #    f_period_foi = double(1),
                #    m_period_foi = double(1),
                   space = double(1),
                   sect = double(1),
                   log = double()) {
        sumllik <- 0 # intialize log-likelihood
        for (i in 1:n_samples) {
            lam_foi <- 0
            lam_sus <- 0
            lam_sus_s <- 0

            #############################################
            # preliminary hazards for the likelihood
            #############################################
            if (sex[i] == 0) {
                for (j in r[i]:(s[i] - 1)) {
                    # sum up surv hazard from r to s-1
                    lam_sus_s <- lam_sus_s +
                                 exp(beta0_sus +
                                    age_effect_surv[j] +
                                    period_effect_surv[age2date[i] + j])
                }

                for (j in e[i]:(s[i] - 1)) {
                    #  up foi hazard from 1  to j
                    lam_foi <- lam_foi +
                               exp(space[sect[i]] +
                                   f_age_foi[age_lookup_f[j]])
    #    f_period_foi[period_lookup_foi[age2date[i] + j]])
                    if (j < r[i]) {
                    lam_sus <- lam_sus +
                                exp(beta0_sus +
                                    age_effect_surv[j] +
                                    period_effect_surv[age2date[i] + j])
                    }
                }
            } else {
                for (j in r[i]:(s[i] - 1)) {
                    # sum up surv hazard from r to s-1
                    lam_sus_s <- lam_sus_s +
                                 exp(beta0_sus +
                                     age_effect_surv[j] +
                                     period_effect_surv[age2date[i] + j] +
                                     beta_male)
                }

                for (j in e[i]:(s[i] - 1)) {
                    #  up foi hazard from 1  to j
                    lam_foi <- lam_foi +
                               exp(space[sect[i]] +
                                   m_age_foi[age_lookup_f[j]])
#    m_period_foi[period_lookup_foi[age2date[i] + j]])
                    if (j < r[i]) {
                        lam_sus <- lam_sus +
                                   exp(beta0_sus +
                                       age_effect_surv[j] +
                                       period_effect_surv[age2date[i] + j] +
                                       beta_male)
                    }
                }
            }
            #######################################
            ### calculating the joint likelihood
            #######################################

            sumllik <- sumllik +
                       log(1 - exp(-lam_sus_s)) -
                       lam_sus -
                       lam_foi
        }

        returnType(double(0))
        if (log) {
            return(sumllik)
        } else {
            return(exp(sumllik))
        } ## return log-likelihood
    }
)

nimble::registerDistributions(list(
    dSusMortTest = list(
        BUGSdist = "dSusMortTest(n_samples,e,r,s,sex,age2date,beta_male,beta0_sus,age_effect_surv,period_effect_surv,f_age_foi,m_age_foi,age_lookup_f,age_lookup_m,space,sect)",
        types = c(
            "value = double(0)",
            "n_samples = integer(0)",
            "e = double(1)",
            "r = double(1)",
            "s = double(1)",
            "sex = double(1)",
            "age2date = double(1)",
            "beta_male = double(0)",
            "beta0_sus = double(0)",
            "age_effect_surv = double(1)",
            "period_effect_surv = double(1)",
            "f_age_foi = double(1)",
            "m_age_foi = double(1)",
            "age_lookup_f = double(1)",
            "age_lookup_m = double(1)",
            # "period_lookup_foi=double(1)",
            # "f_period_foi=double(1)",
            # "m_period_foi=double(1)",
            "space = double(1)",
            "sect = double(1)",
            "log = double(0)"
        ),
        discrete = TRUE
    )
))

# ###for a user-defined distribution
assign("dSusMortTest", dSusMortTest, envir = .GlobalEnv)

# starttime <- Sys.time()
# test <- dSusMortTest(
#         x = 1,
#         n_samples = nrow(d_fit_sus_mort_posttest),
#         e = d_fit_sus_mort_posttest$left_age_e,
#         r = d_fit_sus_mort_posttest$right_age_r,
#         s = d_fit_sus_mort_posttest$right_age_s,
#         sex = d_fit_sus_mort_posttest$sex,
#         age2date = sus_mort_posttest_age2date,
#         beta_male = beta_male,
#         beta0_sus = beta0_survival_sus,
#         age_effect_surv = age_effect_survival_test,
#         period_effect_surv = period_effect_surv,
#         f_age_foi = f_age_foi,
#         m_age_foi = m_age_foi,
#         age_lookup_f = age_lookup_f,
#         age_lookup_m = age_lookup_m,
#         # period_lookup_foi = period_lookup_foi,
#         # f_period_foi = f_period_foi,
#         # m_period_foi = m_period_foi,
#         space = c(0,-.55),
#         sect = d_fit_sus_mort_posttest$study_area,
#         log = TRUE
#         )
# (end<- Sys.time()-starttime)
# test


#######################################################################
###
###   User defined distribution for likelihood for
###   uninfected radio-marked deer mortalities:
###   test neg at cap and no test at mortality, no recap
###
###   d_fit_sus_mort_postno
###
###   Overleaf Equation (8)
###
#######################################################################

dSusMortNoTest <- nimble::nimbleFunction(
    run = function( ### argument type declarations
                   x = integer(0),
                   n_samples = integer(0), # number of samples in dataset
                   e = double(1), # e, age of entry
                   r = double(1), # r, age of last known alive
                   s = double(1), # s, age of known mortality
                   sex = double(1),
                   age2date = double(1),
                   beta_male = double(0),
                   beta0_sus = double(0),
                   beta0_inf = double(0),
                   age_effect_surv = double(1),
                   period_effect_surv = double(1),
                   f_age_foi = double(1),
                   m_age_foi = double(1),
                   age_lookup_f = double(1),
                   age_lookup_m = double(1),
                #    period_lookup_foi = double(1),
                #    f_period_foi = double(1),
                #    m_period_foi = double(1),
                   space = double(1),
                   sect = double(1),
                   log = double(0)) {
        sumllik <- 0 # intialize log-likelihood
        for (i in 1:n_samples) {
            lam_foi <- nimNumeric(s[i] - 1, init = FALSE)
            lam_sus <- nimNumeric(s[i] - 1, init = FALSE)
            lam_inf <- nimNumeric(s[i] - 1, init = FALSE)
            lik_temp <- 0

            #############################################
            # preliminary hazards for the likelihood
            #############################################
            if (sex[i] == 0) { # age loops for females
                # survival hazard for susceptible deer
                lam_sus[e[i]:(s[i] - 1)] <- exp(beta0_sus +
                    age_effect_surv[e[i]:(s[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                                       (s[i] - 1 + age2date[i])])

                # survival hazard while infected
                lam_inf[e[i]:(s[i] - 1)] <- exp(beta0_inf +
                    age_effect_surv[e[i]:(s[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                                       (s[i] - 1 + age2date[i])])

                # force of infection infection hazard
                lam_foi[e[i]:(s[i] - 1)] <- exp(space[sect[i]] +
                    f_age_foi[age_lookup_f[e[i]:(s[i] - 1)]])
  # f_period_foi[period_lookup_foi[(e[i] + age2date[i]):(s[i] - 1 + age2date[i])]])
            } else { # males
                # survival hazard for susceptible deer
                lam_sus[e[i]:(s[i] - 1)] <- exp(beta0_sus +
                    age_effect_surv[e[i]:(s[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                                       (s[i] - 1 + age2date[i])] +
                    beta_male)

                # survival hazard while infected
                lam_inf[e[i]:(s[i] - 1)] <- exp(beta0_inf +
                    age_effect_surv[e[i]:(s[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                                       (s[i] - 1 + age2date[i])] +
                    beta_male)

                # force of infection infection hazard
                lam_foi[e[i]:(s[i] - 1)] <- exp(space[sect[i]] +
                    m_age_foi[age_lookup_m[e[i]:(s[i] - 1)]]) 
          # m_period_foi[period_lookup_foi[(e[i] + age2date[i]):(s[i] - 1 + age2date[i])]])
            }
            #######################################
            ### calculating the joint likelihood
            #######################################

            # total probability of getting infected and dying before end of the study
            #if x == e[i]
            lik_temp <- lik_temp + 
                        (1 - exp(-lam_foi[e[i]]))*
                        (1 - exp(-sum(lam_inf[r[i]:(s[i] - 1)])))*
                        exp(-sum(lam_inf[e[i]:(r[i] - 1)]))
            for(k in (e[i] + 1):(r[i] - 1)) {
                lik_temp <- lik_temp + 
                            (1 - exp(-lam_foi[k])) * 
                            (1 - exp(-sum(lam_inf[r[i]:(s[i] - 1)]))) *
                            exp(-sum(lam_foi[e[i]:(k - 1)])) *
                            exp(-sum(lam_sus[e[i]:(k - 1)])) *
                            exp(-sum(lam_inf[k:(r[i] - 1)]))
            }
            lik_temp <- lik_temp +
                        (1 - exp(-lam_foi[s[i]-1])) * 
                        (1 - exp(-sum(lam_inf[r[i]:(s[i] - 1)]))) *
                        exp(-sum(lam_foi[e[i]:(r[i] - 1)])) *
                        exp(-sum(lam_sus[e[i]:(r[i] - 1)]))

            # lik_temp <- lik_temp + lam_foi[dn1[i] + 1] *
            #     exp(-sum(lam_inf[(dn1[i] + 1):(r[i] - 1)])) *
            #     (1 - exp(-sum(lam_inf[r[i]:(s[i] - 1)])))
            # if ((r[i] - dn1[i]) > 3) {
            #     for (k in (dn1[i] + 2):(r[i] - 2)) {
            #         lik_temp <- lik_temp + lam_foi[k] *
            #             exp(-sum(lam_foi[(dn1[i] + 1):(k - 1)])) *
            #             exp(-sum(lam_sus[(dn1[i] + 1):(k - 1)])) *
            #             exp(-sum(lam_inf[k:(r[i] - 1)])) *
            #             (1 - exp(-sum(lam_inf[r[i]:(s[i] - 1)])))
            #     }
            # }
            # if ((r[i] - dn1[i]) > 2) {
            #     for (k in (r[i] - 1):(s[i] - 2)) {
            #         lik_temp <- lik_temp + lam_foi[k] *
            #             exp(-sum(lam_foi[(dn1[i] + 1):(k - 1)])) *
            #             exp(-sum(lam_sus[(dn1[i] + 1):(k - 1)])) *
            #             (1 - exp(-sum(lam_inf[k:(s[i] - 1)])))
            #     }
            # }
            # lik_temp <- lik_temp + lam_foi[(s[i] - 1)] *
            #     exp(-sum(lam_foi[(dn1[i] + 1):((s[i] - 1) - 1)])) *
            #     exp(-sum(lam_sus[(dn1[i] + 1):((s[i] - 1) - 1)])) *
            #     (lam_inf[(s[i] - 1)])

            sumllik <- sumllik +
                       log(
                        exp(-sum(lam_foi[e[i]:(s[i] - 1)])) *
                        exp(-sum(lam_sus[e[i]:(r[i] - 1)])) *
                        (1 - exp(-sum(lam_sus[r[i]:(s[i] - 1)]))) +
                        lik_temp
                       )#endloglik
        }
        returnType(double(0))
        if (log) {
            return(sumllik)
        } else {
            return(exp(sumllik))
        }
    }
)

nimble::registerDistributions(list(
    dSusMortNoTest = list(
        BUGSdist = "dSusMortNoTest(n_samples,e,r,s,sex,age2date,beta_male,beta0_sus,beta0_inf,age_effect_surv,period_effect_surv,f_age_foi,m_age_foi,age_lookup_f,age_lookup_m,space,sect)",
        types = c(
            "value = integer(0)",
            "n_samples = integer(0)",
            "e = double(1)",
            "r = double(1)",
            "s = double(1)",
            "sex = double(1)",
            "age2date = double(1)",
            "beta_male = double(0)",
            "beta0_inf = double(0)",
            "beta0_sus = double(0)",
            "age_effect_surv = double(1)",
            "period_effect_surv = double(1)",
            "f_age_foi = double(1)",
            "m_age_foi = double(1)",
            "age_lookup_f = double(1)",
            "age_lookup_m = double(1)",
            # "period_lookup_foi=double(1)",
            # "f_period_foi=double(1)",
            # "m_period_foi=double(1)",
            "space = double(1)",
            "sect = double(1)",
            "log = double(0)"
        ),
        discrete = TRUE
    )
))

### for a user-defined distribution
assign("dSusMortNoTest", dSusMortNoTest, envir = .GlobalEnv)

# starttime <- Sys.time()
# test <-  dSusMortNoTest(
#         x = 1,
#         n_samples = nrow(d_fit_sus_mort_postno),
#         e = d_fit_sus_mort_postno$left_age_e,
#         r = d_fit_sus_mort_postno$right_age_r,
#         s = d_fit_sus_mort_postno$right_age_s,
#         sex = d_fit_sus_mort_postno$sex,
#         age2date = sus_mort_postno_age2date,
#         beta_male = beta_male,
#         beta0_sus = beta0_survival_sus,
#         beta0_inf = beta0_survival_inf,
#         age_effect_surv = age_effect_survival_test,
#         period_effect_surv = period_effect_surv,
#         f_age_foi = f_age_foi,
#         m_age_foi = m_age_foi,
#         age_lookup_f = age_lookup_f,
#         age_lookup_m = age_lookup_m,
#         # period_lookup_foi = period_lookup_foi,
#         # f_period_foi = f_period_foi,
#         # m_period_foi = m_period_foi,
#         sect = d_fit_sus_mort_postno$study_area,
#         space = c(0,-.55),
#         log = TRUE
#         )
# (end<- Sys.time()-starttime)
# test


#######################################################################
###
###   User defined distribution for likelihood for
###   infected deer mortalities for radio marked deer that
###   enter the study as test positive at capture
###
###   d_fit_icap_cens
###
###   Overleaf Equation (10)
###
#######################################################################

dIcapCens <- nimble::nimbleFunction(
    run = function( ### argument type declarations
                   x = integer(0),
                   n_samples = integer(0), # number of samples in dataset
                   e = double(1), # e, age of entry
                   r = double(1), # r, age of last known alive
                   sex = double(1),
                   age2date = double(1),
                   beta_male = double(0),
                   beta0_inf = double(0),
                   age_effect_surv = double(1),
                   period_effect_surv = double(1),
                   log = double()) {
        sumllik <- 0 # intialize log-likelihood
        for (i in 1:n_samples) {
            lam_inf <- 0

            #############################################
            # preliminary hazards for the likelihood
            #############################################
            if (sex[i] == 0) { # age loops for females
                # now recalculate lam_inf to sum over e to r-1
                for (j in e[i]:(r[i] - 1)) {
                    lam_inf <- lam_inf +
                               exp(beta0_inf +
                                   age_effect_surv[j] +
                                   period_effect_surv[age2date[i] + j])
                }
            } else { # age loops for males
                # now recalculate lam_inf to sum over e to r - 1
                for (j in e[i]:(r[i] - 1)) {
                    lam_inf <- lam_inf +
                               exp(beta0_inf +
                                   age_effect_surv[j] +
                                   period_effect_surv[age2date[i] + j] +
                                   beta_male)
                }
            }
            #######################################
            ### calculating the joint likelihood
            #######################################
            sumllik <- sumllik - lam_inf
        }
        returnType(double(0))
        if (log) {
            return(sumllik)
        } else {
            return(exp(sumllik))
        } ## return log-likelihood
    }
)


nimble::registerDistributions(list(
    dIcapCens = list(
        BUGSdist = "dIcapCens(n_samples,e,r,sex,age2date,beta_male,beta0_inf,age_effect_surv,period_effect_surv)",
        types = c(
            "value=integer(0)",
            "n_samples = integer(0)",
            "e = double(1)",
            "r = double(1)",
            "sex = double(1)",
            "age2date = double(1)",
            "beta_male = double(0)",
            "beta0_inf = double(0)",
            "age_effect_surv = double(1)",
            "period_effect_surv = double(1)",
            "log = double()"
        ),
        discrete = TRUE
    )
))

# for a user-defined distribution
assign("dIcapCens", dIcapCens, envir = .GlobalEnv)

# starttime <- Sys.time()
# test <-  dIcapCens(
#         x = 1,
#         n_samples = nrow(d_fit_icap_cens),
#         e = d_fit_icap_cens$left_age_e,
#         r = d_fit_icap_cens$right_age_r,
#         sex = d_fit_icap_cens$sex,
#         age2date = icap_cens_age2date,
#         beta_male = beta_male,
#         beta0_inf = beta0_survival_inf,
#         age_effect_surv = age_effect_survival_test,
#         period_effect_surv = period_effect_surv,
#         log = TRUE
#         )
# (endtime <- Sys.time() - starttime)
# test


#######################################################################
###
###   User defined distribution for likelihood for
###   infected deer mortalities for radio marked deer that
###   enter the study as test positive at capture
###
###   d_fit_icap_mort
###
###   Overleaf Equation (12)
###
#######################################################################

dIcapMort <- nimble::nimbleFunction(
    run = function( ### argument type declarations
                   x = integer(0),
                   n_samples = integer(0), # number of samples in dataset
                   e = double(1), # e, age of entry
                   r = double(1), # r, age of last known alive
                   s = double(1), # s, age of known mortality
                   sex = double(1),
                   age2date = double(1),
                   beta_male = double(0),
                   beta0_inf = double(0),
                   age_effect_surv = double(1),
                   period_effect_surv = double(1),
                   log = double()) {

        # start the loop through individuals
        sumllik <- 0
        for (i in 1:n_samples) {
            lam_inf <- 0
            #############################################
            # preliminary hazards for the likelihood
            #############################################
            if (sex[i] == 0) { # females
                # sum up infected hazard from 1 to e-1
                for (j in e[i]:(r[i] - 1)) {
                    lam_inf <- lam_inf +
                               exp(beta0_inf +
                                   age_effect_surv[j] +
                                   period_effect_surv[age2date[i] + j])
                }
                lam_inf_s <- sum(exp(beta0_inf +
                    age_effect_surv[(r[i]):(s[i] - 1)] +
                    period_effect_surv[age2date[i] + (r[i]):(s[i] - 1)]))
            } else {
                # sum up infected hazard from 1 to e-1
                for (j in e[i]:(r[i] - 1)) {
                    lam_inf <- lam_inf +
                               exp(beta0_inf +
                                   age_effect_surv[j] +
                                   period_effect_surv[age2date[i] + j] +
                                   beta_male)
                }
                lam_inf_s <- sum(exp(beta0_inf +
                    age_effect_surv[r[i]:(s[i] - 1)] +
                    period_effect_surv[age2date[i] + r[i]:(s[i] - 1)] +
                    beta_male))
            }
            #######################################
            ### calculating the joint likelihood
            #######################################

            sumllik <- sumllik - lam_inf +
                       log(1 - exp(-lam_inf_s))
        }#end n_samples

        returnType(double(0))
        if (log) {
            return(sumllik)
        } else {
            return(exp(sumllik))
        } ## return log-likelihood
})


nimble::registerDistributions(list(
    dIcapMort = list(
        BUGSdist = "dIcapMort(n_samples,e,r,s,sex,age2date,beta_male,beta0_inf,age_effect_surv,period_effect_surv)",
        types = c(
            "value = integer(0)",
            "n_samples = integer(0)",
            "e = double(1)",
            "r = double(1)",
            "s = double(1)",
            "sex = double(1)",
            "age2date = double(1)",
            "beta_male = double(0)",
            "beta0_inf = double(0)",
            "age_effect_surv = double(1)",
            "period_effect_surv = double(1)",
            "log = double()"
        ),
        discrete = TRUE
    )
))

### for a user-defined distribution
assign("dIcapMort", dIcapMort, envir = .GlobalEnv)

# starttime <- Sys.time()
# test <- dIcapMort(
#         x = 1,
#         n_samples = nrow(d_fit_icap_mort),
#         e = d_fit_icap_mort$left_age_e,
#         r = d_fit_icap_mort$right_age_r,
#         s = d_fit_icap_mort$right_age_s,
#         sex = d_fit_icap_mort$sex,
#         age2date = icap_mort_age2date,
#         beta_male = beta_male,
#         beta0_inf = beta0_survival_inf,
#         age_effect_surv = age_effect_survival_test,
#         period_effect_surv = period_effect_surv,
#         log = TRUE
#         )
# (end <- Sys.time() - starttime)
# test

#######################################################################
###
###   User defined distribution for likelihood for
###   uninfected deer that were test neg at capture,
###   then test negative at recap, that are right censored,
###   and have been tested post censoring
###
###   d_fit_rec_neg_cens_posttest
###
###   Overleaf Equation (14)
###
#######################################################################

dRecNegCensTest <- nimble::nimbleFunction(
    run = function( ### argument type declarations
                   x = integer(0),
                   n_samples = integer(0), # number of samples in dataset
                   e = double(1), # e, age of entry
                   r = double(1), # r, age of last known alive
                   sex = double(1),
                   age2date = double(1),
                   beta_male = double(0),
                   beta0_sus = double(0),
                   age_effect_surv = double(1),
                   period_effect_surv = double(1),
                   f_age_foi = double(1),
                   m_age_foi = double(1),
                   age_lookup_f = double(1),
                   age_lookup_m = double(1),
                #    period_lookup_foi = double(1),
                #    f_period_foi = double(1),
                #    m_period_foi = double(1),
                   sect = double(1),
                   space = double(1),
                   log = double()) {
        sumllik <- 0 # intialize log-likelihood
        for (i in 1:n_samples) {
            lam_foi <- 0
            lam_sus <- 0
            # lam_inf <- 0
            # lik_temp <- 0

            #############################################
            # preliminary hazards for the likelihood
            #############################################
            if (sex[i] == 0) { # females
                for (j in e[i]:(r[i] - 1)) {
                    # survival hazard for susceptible deer
                    lam_sus <- lam_sus + exp(beta0_sus +
                        age_effect_surv[j] +
                        period_effect_surv[j + age2date[i]])
                    # force of infection infection hazard
                    lam_foi <- lam_foi + exp(space[sect[i]] +
                        f_age_foi[age_lookup_f[j]])
                        # f_period_foi[period_lookup_foi[j + age2date[i]]])
                }
            } else { # males
                for (j in e[i]:(r[i] - 1)) {
                    # survival hazard for susceptible deer
                    lam_sus <- lam_sus + exp(beta0_sus +
                        age_effect_surv[j] +
                        period_effect_surv[j + age2date[i]] +
                        beta_male)
                    # force of infection infection hazard
                    lam_foi <- lam_foi + exp(space[sect[i]] +
                        m_age_foi[age_lookup_m[j]]) 
                        # m_period_foi[period_lookup_foi[j + age2date[i]]])
                }
            }
            #######################################
            ### calculating the joint likelihood
            #######################################
            sumllik <- sumllik - lam_sus - lam_foi
        }
        returnType(double(0))
        if (log) {
            return(sumllik)
        } else {
            return(exp(sumllik))
        } ## return log-likelihood
    }
)

nimble::registerDistributions(list(
    dRecNegCensTest = list(
        BUGSdist = "dRecNegCensTest(n_samples,e,r,sex,age2date,beta_male,beta0_sus,age_effect_surv,period_effect_surv,f_age_foi,m_age_foi,age_lookup_f,age_lookup_m,space,sect)",
        types = c(
            "value = integer(0)",
            "n_samples = integer(0)",
            "e = double(1)",
            "r = double(1)",
            "sex = double(1)",
            "age2date = double(1)",
            "beta_male = double(0)",
            "beta0_sus = double(0)",
            "age_effect_surv = double(1)",
            "period_effect_surv = double(1)",
            "f_age_foi = double(1)",
            "m_age_foi = double(1)",
            "age_lookup_f = double(1)",
            "age_lookup_m = double(1)",
            # "f_period_foi=double(1)",
            # "m_period_foi=double(1)",
            # "period_lookup_foi=double(1)",
            "space = double(1)",
            "sect = double(1)",
            "log = double(0)"
        ),
        discrete = TRUE
    )
))

## for a user-defined distribution
assign("dRecNegCensTest", dRecNegCensTest, envir = .GlobalEnv)

# starttime <- Sys.time()
# test <- dRecNegCensTest(
#         x = 1,
#         n_samples = nrow(d_fit_rec_neg_cens_posttest),
#         e = d_fit_rec_neg_cens_posttest$left_age_e,
#         r = d_fit_rec_neg_cens_posttest$right_age_r,
#         sex = d_fit_rec_neg_cens_posttest$sex,
#         age2date = rec_neg_cens_posttest_age2date,
#         beta_male = beta_male,
#         beta0_sus = beta0_survival_sus,
#         age_effect_surv = age_effect_survival_test,
#         period_effect_surv = period_effect_surv,
#         f_age_foi = f_age_foi,
#         m_age_foi = m_age_foi,
#         age_lookup_f = age_lookup_f,
#         age_lookup_m = age_lookup_m,
#         # period_lookup_foi = period_lookup_foi,
#         # f_period_foi = f_period_foi,
#         # m_period_foi = m_period_foi,
#         sect = d_fit_rec_neg_cens_posttest$study_area,
#         space = c(0,-.55),
#         log = TRUE
#         )
# (end<- Sys.time()-starttime)
# test


#######################################################################
###
###   User defined distribution for likelihood for
###   uninfected deer that were test neg at capture,
###   then test negative at recap, that are right censored,
###   and have not been tested post censoring
###
###   d_fit_rec_neg_cens_postno
###
###   Overleaf Equation (16)
###
#######################################################################

dRecNegCensPostNo <- nimble::nimbleFunction(
    run = function( ### argument type declarations
                   x = integer(0),
                   n_samples = integer(0), # number of samples in dataset
                   e = double(1), # e, age of entry
                   r = double(1), # r, age of last known alive
                   dn1 = double(1), # right of last test negative
                   sex = double(1),
                   age2date = double(1),
                   beta_male = double(0),
                   beta0_sus = double(0),
                   beta0_inf = double(0),
                   age_effect_surv = double(1),
                   period_effect_surv = double(1),
                   f_age_foi = double(1),
                   m_age_foi = double(1),
                   age_lookup_f = double(1),
                   age_lookup_m = double(1),
                #    period_lookup_foi = double(1),
                #    f_period_foi = double(1),
                #    m_period_foi = double(1),
                   sect = double(1),
                   space = double(1),
                   log = double()) {
        sumllik <- 0 # intialize log-likelihood
        for (i in 1:n_samples) {
            lam_foi <- nimNumeric(r[i], init = FALSE)
            lam_sus <- nimNumeric(r[i], init = FALSE)
            lam_inf <- nimNumeric(r[i], init = FALSE)
            # lik_temp <- 0

            #############################################
            # preliminary hazards for the likelihood
            #############################################
            if (sex[i] == 0) { # females
                # survival hazard for susceptible deer
                lam_sus[e[i]:(r[i] - 1)] <- exp(beta0_sus +
                    age_effect_surv[e[i]:(r[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                                       (r[i] - 1 + age2date[i])])
                # survival hazard while infected
                lam_inf[(dn1[i]):(r[i] - 1)] <- exp(beta0_inf +
                    age_effect_surv[(dn1[i]):(r[i] - 1)] +
                    period_effect_surv[(dn1[i] + age2date[i]):
                                       (r[i] - 1 + age2date[i])])
                # force of infection infection hazard
                lam_foi[e[i]:(r[i] - 1)] <- exp(space[sect[i]] +
                    f_age_foi[age_lookup_f[e[i]:(r[i] - 1)]])
                    # f_period_foi[period_lookup_foi[(1 + age2date[i]):
                                #  (r[i] - 1 + age2date[i])]])
            } else {
                # survival hazard for susceptible deer
                lam_sus[e[i]:(r[i] - 1)] <- exp(beta0_sus +
                    age_effect_surv[e[i]:(r[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                                       (r[i] - 1 + age2date[i])] +
                    beta_male)
                # survival hazard while infected
                lam_inf[(dn1[i]):(r[i] - 1)] <- exp(beta0_inf +
                    age_effect_surv[(dn1[i]):(r[i] - 1)] +
                    period_effect_surv[(dn1[i] + age2date[i]):
                                       (r[i] - 1 + age2date[i])] +
                    beta_male)
                # force of infection infection hazard
                lam_foi[e[i]:(r[i] - 1)] <- exp(space[sect[i]] +
                    m_age_foi[age_lookup_m[e[i]:(r[i] - 1)]])
    # m_period_foi[period_lookup_foi[(1 + age2date[i]):
            # (r[i] - 1 + age2date[i])]])
            }

            #######################################
            ### calculating the joint likelihood
            #######################################

            lik_temp <- (1 - exp(-lam_foi[dn1[i]])) *
                        exp(-sum(lam_inf[dn1[i]:(r[i] - 1)]))

            for (k in (dn1[i] + 1):(r[i] - 1)) {
                lik_temp <- lik_temp +
                            (1 - exp(-lam_foi[k])) *
                            exp(-sum(lam_sus[dn1[i]:(k - 1)])) *
                            exp(-sum(lam_inf[k:(r[i] - 1)])) *
                            exp(-sum(lam_foi[k:(r[i] - 1)]))
            }

            sumllik <- sumllik +
                       log(exp(-sum(lam_sus[e[i]:(dn1[i] - 1)])) *
                           exp(-sum(lam_foi[e[i]:(dn1[i] - 1)])) *
                           lik_temp +
                       exp(-sum(lam_foi[e[i]:(r[i] - 1)])) *
                       exp(-sum(lam_sus[e[i]:(r[i] - 1)])))
        }
        returnType(double(0))
        if (log) {
            return(sumllik)
        } else {
            return(exp(sumllik))
        } ## return log-likelihood
    }
)

nimble::registerDistributions(list(
    dRecNegCensPostNo = list(
        BUGSdist = "dRecNegCensPostNo(n_samples,e,r,dn1,sex,age2date,beta_male,beta0_inf,beta0_sus,age_effect_surv,period_effect_surv,f_age_foi,m_age_foi,age_lookup_f,age_lookup_m,sect,space)",
        types = c(
            "value = integer(0)",
            "n_samples = integer(0)",
            "e = double(1)",
            "r = double(1)",
            "dn1 = double(1)",
            "sex = double(1)",
            "age2date = double(1)",
            "beta_male = double(0)",
            "beta0_inf = double(0)",
            "beta0_sus = double(0)",
            "age_effect_surv = double(1)",
            "period_effect_surv = double(1)",
            "f_age_foi = double(1)",
            "m_age_foi = double(1)",
            "age_lookup_f = double(1)",
            "age_lookup_m = double(1)",
            # "period_lookup_foi=double(1)",
            # "f_period_foi=double(1)",
            # "m_period_foi=double(1)",
            "sect = double(1)",
            "space = double(1)",
            "log = double(0)"
        ),
        discrete = TRUE
    )
))

### for a user-defined distribution
assign("dRecNegCensPostNo", dRecNegCensPostNo, envir = .GlobalEnv)

# starttime <- Sys.time()
# test <-  dRecNegCensPostNo(
#         x = 1,
#         n_samples = nrow(d_fit_rec_neg_cens_postno),
#         e = d_fit_rec_neg_cens_postno$left_age_e,
#         r = d_fit_rec_neg_cens_postno$right_age_r,
#         dn1 = d_fit_rec_neg_cens_postno$ageweek_recap,
#         sex = d_fit_rec_neg_cens_postno$sex,
#         age2date = rec_neg_cens_postno_age2date,
#         beta_male = beta_male,
#         beta0_sus = beta0_survival_sus,
#         beta0_inf = beta0_survival_inf,
#         age_effect_surv = age_effect_survival_test,
#         period_effect_surv = period_effect_surv,
#         f_age_foi = f_age_foi,
#         m_age_foi = m_age_foi,
#         age_lookup_f = age_lookup_f,
#         age_lookup_m = age_lookup_m,
#         # period_lookup_foi = period_lookup_foi,
#         # f_period_foi = f_period_foi,
#         # m_period_foi = m_period_foi,
#         sect = d_fit_rec_neg_cens_postno$study_area,
#         space = c(0,-.55),
#         log = TRUE
#         )
# (end <- Sys.time()-starttime)
# test


#######################################################################
###
###   User defined distribution for likelihood for
###   uninfected deer that were test neg at capture,
###   then test negative at recap,
###   that die
###
###   d_fit_rec_neg_mort
###
###   Overleaf Equation (18)
###
#######################################################################

dRecNegMort <- nimble::nimbleFunction(
    run = function( ### argument type declarations
                   x = integer(0),
                   n_samples = integer(0), # number of samples in dataset
                   e = double(1), # e, age of entry
                   r = double(1), # r, age of last known alive
                   s = double(1), # s, age of known mortality
                   sex = double(1),
                   age2date = double(1),
                   beta_male = double(0),
                   beta0_sus = double(0),
                   age_effect_surv = double(1),
                   period_effect_surv = double(1),
                   f_age_foi = double(1),
                   m_age_foi = double(1),
                   age_lookup_f = double(1),
                   age_lookup_m = double(1),
                #    period_lookup_foi = double(1),
                #    f_period_foi = double(1),
                #    m_period_foi = double(1),
                   sect = double(1),
                   space = double(1),
                   log = double(0)) {
        sumllik <- 0 # intialize log-likelihood
        for (i in 1:n_samples) {
            lam_foi <- 0
            lam_sus <- 0

            #############################################
            # preliminary hazards for the likelihood
            #############################################
            if (sex[i] == 0) { # age loops for females
                # survival hazard for susceptible deer
                for (j in e[i]:(r[i] - 1)) {
                    lam_sus <- lam_sus + exp(beta0_sus +
                        age_effect_surv[j] +
                        period_effect_surv[(j + age2date[i])])
                }
                lam_sus_s <- sum(exp(beta0_sus +
                    age_effect_surv[r[i]:(s[i] - 1)] +
                    period_effect_surv[(r[i] + age2date[i]):(s[i] - 1 + age2date[i])]))
                # force of infection infection hazard
                for (j in e[i]:(s[i] - 1)) {
                    lam_foi <- lam_foi + exp(space[sect[i]] +
                        f_age_foi[age_lookup_f[j]]) 
                        # f_period_foi[period_lookup_foi[(j + age2date[i])]])
                }
            } else { # males
                # survival hazard for susceptible deer
                for (j in e[i]:(r[i] - 1)) {
                    lam_sus <- lam_sus + exp(beta0_sus +
                        age_effect_surv[j] +
                        period_effect_surv[(j + age2date[i])] +
                        beta_male)
                }
                lam_sus_s <- sum(exp(beta0_sus +
                    age_effect_surv[r[i]:(s[i] - 1)] +
                    period_effect_surv[(r[i] + age2date[i]):(s[i] - 1 + age2date[i])] +
                        beta_male))

                # force of infection infection hazard
                for (j in e[i]:(s[i] - 1)) {
                    lam_foi <- lam_foi + exp(space[sect[i]] +
                        m_age_foi[age_lookup_m[j]])
                        # m_period_foi[period_lookup_foi[(j + age2date[i])]])
                }
            }

            #######################################
            ### calculating the joint likelihood
            #######################################

            sumllik <- sumllik - lam_sus + log(1 - exp(-lam_sus_s)) - lam_foi
        }
        returnType(double(0))
        if (log) {
            return(sumllik)
        } else {
            return(exp(sumllik))
        } ## return log-likelihood
    }
)

nimble::registerDistributions(list(
    dRecNegMort = list(
        BUGSdist = "dRecNegMort(n_samples,e,r,s,sex,age2date,beta_male,beta0_sus,age_effect_surv,period_effect_surv,f_age_foi,m_age_foi,age_lookup_f,age_lookup_m,space,sect)",
        types = c(
            "value = integer(0)",
            "n_samples = integer(0)",
            "e = double(1)",
            "r = double(1)",
            "s = double(1)",
            "sex =  double(1)",
            "age2date =  double(1)",
            "beta_male = double(0)",
            "beta0_sus = double(0)",
            "age_effect_surv = double(1)",
            "period_effect_surv = double(1)",
            "f_age_foi = double(1)",
            "m_age_foi = double(1)",
            "age_lookup_f = double(1)",
            "age_lookup_m = double(1)",
            # "period_lookup_foi = double(1)",
            # "f_period_foi = double(1)",
            # "m_period_foi = double(1)",
            "space = double(1)",
            "sect = double(1)",
            "log = double(0)"
        ),
        discrete = TRUE
    )
))

# ###for a user-defined distribution
assign("dRecNegMort", dRecNegMort, envir = .GlobalEnv)

# starttime <- Sys.time()
# test <-  dRecNegMort(
#         x = 1,
#         n_samples = nrow(d_fit_rec_neg_mort),
#         e = d_fit_rec_neg_mort$left_age_e,
#         r = d_fit_rec_neg_mort$right_age_r,
#         s = d_fit_rec_neg_mort$right_age_s,
#         sex = d_fit_rec_neg_mort$sex,
#         age2date = rec_neg_mort_age2date,
#         beta_male = beta_male,
#         beta0_sus = beta0_survival_sus,
#         age_effect_surv = age_effect_survival_test,
#         period_effect_surv = period_effect_surv,
#         f_age_foi = f_age_foi,
#         m_age_foi = m_age_foi,
#         age_lookup_f = age_lookup_f,
#         age_lookup_m = age_lookup_m,
#         # period_lookup_foi = period_lookup_foi,
#         # f_period_foi = f_period_foi,
#         # m_period_foi = m_period_foi,
#         sect = d_fit_rec_neg_mort$study_area,
#         space = c(0,-.55),
#         log = TRUE
#         )
# (end<- Sys.time()-starttime)
# test

#######################################################################
###
###   User defined distribution for likelihood for
###   infected deer that were test neg at capture,
###   then test positive at recap,
###   than these were right censored
###
###   d_fit_rec_pos_cens
###
###   Overleaf Equation (20)
###
#######################################################################

dRecPosCens <- nimble::nimbleFunction(
    run = function( ### argument type declarations
                   x = integer(0),
                   e = double(0), # e, age of entry
                   r = double(0), # r, age of last known alive
                   dn = double(0), # date of positive test
                   sex = double(0),
                   age2date = double(0),
                   beta_male = double(0),
                   beta0_sus = double(0),
                   beta0_inf = double(0),
                   age_effect_surv = double(1),
                   period_effect_surv = double(1),
                   f_age_foi = double(1),
                   m_age_foi = double(1),
                   age_lookup_f = double(1),
                   age_lookup_m = double(1),
                #    period_lookup_foi = double(1),
                #    f_period_foi = double(1),
                #    m_period_foi = double(1),
                   space = double(0),
                   # sect = double(0),
                   log = double()) {
        sumllik <- 0 # intialize log-likelihood
        lam_foi <- nimNumeric(r - 1, init = FALSE)
        lam_sus <- nimNumeric(r - 1, init = FALSE)
        lam_inf <- nimNumeric(r - 1, init = FALSE)
        lik_temp <- 0

        #############################################
        # preliminary hazards for the likelihood
        #############################################
        if (sex == 0) { # females
            # survival hazard for susceptible deer
            lam_sus[e:(dn - 1)] <- exp(beta0_sus +
                age_effect_surv[e:(dn - 1)] +
                period_effect_surv[(e + age2date):(dn - 1 + age2date)])

            # survival hazard while infected
            lam_inf[e:(r - 1)] <- exp(beta0_inf +
                age_effect_surv[e:(r - 1)] +
                period_effect_surv[(e + age2date):
                				   (r - 1 + age2date)])
            # force of infection infection hazard
            lam_foi[e:(dn - 1)] <- exp(space +
                f_age_foi[age_lookup_f[e:(dn - 1)]] )
                # f_period_foi[period_lookup_foi[(e + age2date):
                #                            ((dn - 1) + age2date)]])
        } else { # males
            # survival hazard for susceptible deer
            lam_sus[e:(dn - 1)] <- exp(beta0_sus +
                age_effect_surv[e:(dn - 1)] +
                period_effect_surv[(e + age2date):(dn - 1 + age2date)] +
                beta_male)
            # survival hazard while infected
            lam_inf[e:(r - 1)] <- exp(beta0_inf +
                age_effect_surv[e:(r - 1)] +
                period_effect_surv[(e + age2date):
                (r - 1 + age2date)] +
                beta_male)
            # force of infection infection hazard
            lam_foi[e:(dn - 1)] <- exp(space +
                m_age_foi[age_lookup_m[e:(dn - 1)]])
                # m_period_foi[period_lookup_foi[(e + age2date):
                #                               ((dn - 1) + age2date)]])
        }

        #######################################
        ### calculating the joint likelihood
        #######################################

        lik_temp <- lik_temp + (1 - exp(-lam_foi[e])) *
            exp(-sum(lam_inf[e:(r - 1)]))

        for (k in (e + 1):(dn - 1)) {
            lik_temp <- lik_temp + 
                (1 - exp(-lam_foi[k])) *
                exp(-sum(lam_inf[k:(r - 1)])) *
                exp(-sum(lam_foi[e:(k - 1)])) *
                exp(-sum(lam_sus[e:(k - 1)]))
        }
        sumllik <- sumllik + log(lik_temp)

        returnType(double(0))
        if (log) {
            return(sumllik)
        } else {
            return(exp(sumllik))
        } ## return log-likelihood
    }
)

nimble::registerDistributions(list(
    dRecPosCens = list(
        BUGSdist = "dRecPosCens(e,r,dn,sex,age2date,beta_male,beta0_sus,beta0_inf,age_effect_surv,period_effect_surv,f_age_foi,m_age_foi,age_lookup_f,age_lookup_m,space)",
        types = c(
            "e = double(0)",
            "r = double(0)",
            "dn = double(0)",
            "sex = double(0)",
            "age2date = double(0)",
            "beta_male = double(0)",
            "beta0_inf = double(0)",
            "beta0_sus = double(0)",
            "age_effect_surv = double(1)",
            "period_effect_surv = double(1)",
            "f_age_foi = double(1)",
            "m_age_foi = double(1)",
            "age_lookup_f = double(1)",
            "age_lookup_m = double(1)",
            # "period_lookup_foi=double(1)",
            # "f_period_foi=double(1)",
            # "m_period_foi=double(1)",
            "space = double(0)",
            #   "sect = double(0)",
            "log = double(0)"
        ),
        discrete = TRUE
    )
))

# ### for a user-defined distribution
# assign("dRecPosCens", dRecPosCens, envir = .GlobalEnv)

# starttime <- Sys.time()
# test <-  dRecPosCens(
#         x = 1,
#         #n_samples = nrow(d_fit_rec_pos_cens),
#         e = d_fit_rec_pos_cens$left_age_e,
#         r = d_fit_rec_pos_cens$right_age_r,
#         dn = d_fit_rec_pos_cens$ageweek_recap,
#         sex = d_fit_rec_pos_cens$sex,
#         age2date = rec_pos_cens_age2date,
#         beta_male = beta_male,
#         beta0_sus = beta0_survival_sus,
#         beta0_inf = beta0_survival_inf,
#         age_effect_surv = age_effect_survival_test,
#         period_effect_surv = period_effect_surv,
#         f_age_foi = f_age_foi,
#         m_age_foi = m_age_foi,
#         age_lookup_f = age_lookup_f,
#         age_lookup_m = age_lookup_m,
#         # period_lookup_foi = period_lookup_foi,
#         # f_period_foi = f_period_foi,
#         # m_period_foi = m_period_foi,
#         space = 0,#c(0,-.55),
#         # sect = d_fit_rec_pos_cens$study_area,
#         log = TRUE
#         )
# (end<- Sys.time()-starttime)
# test


#######################################################################
###
###   User defined distribution for likelihood for
###   deer that were test neg at capture,
###   then test positive at recap,
###   than die
###
###   d_fit_rec_pos_mort
###
###   Overleaf Equation (22)
###
#######################################################################

dRecPosMort <- nimble::nimbleFunction(
    run = function( ### argument type declarations
                   x = integer(0),
                   n_samples = integer(0), # number of samples in dataset
                   e = double(1), # e, age of entry
                   r = double(1), # r, age of last known alive
                   s = double(1), # s, age of known mortality
                   dn = double(1), # interval of test positive (could be interval of mortality)
                   sex = double(1),
                   age2date = double(1),
                   beta_male = double(0),
                   beta0_sus = double(0),
                   beta0_inf = double(0),
                   age_effect_surv = double(1),
                   period_effect_surv = double(1),
                   f_age_foi = double(1),
                   m_age_foi = double(1),
                   age_lookup_f = double(1),
                   age_lookup_m = double(1),
                #    period_lookup_foi = double(1),
                #    f_period_foi = double(1),
                #    m_period_foi = double(1),
                   space = double(1),
                   sect = double(1),
                   log = double()) {
        sumllik <- 0 # intialize log-likelihood
        for (i in 1:n_samples) {
            lam_foi <- nimNumeric(s[i] - 1, init = FALSE)
            lam_sus <- nimNumeric(s[i] - 1, init = FALSE)
            lam_inf <- nimNumeric(s[i] - 1, init = FALSE)
            lik_temp <- 0

            #############################################
            # preliminary hazards for the likelihood
            #############################################
            if (sex[i] == 0) { # females
                # survival hazard for susceptible deer
                lam_sus[e[i]:(dn[i] - 1)] <- exp(beta0_sus +
                    age_effect_surv[e[i]:(dn[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                                       (dn[i] - 1 + age2date[i])])
                # survival hazard while infected
                lam_inf[e[i]:(s[i] - 1)] <- exp(beta0_inf +
                    age_effect_surv[e[i]:(s[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                    (s[i] - 1 + age2date[i])])
                # force of infection infection hazard
                lam_foi[e[i]:(dn[i] - 1)] <- exp(space[sect[i]] +
                    f_age_foi[age_lookup_f[e[i]:(dn[i] - 1)]])
                    # f_period_foi[period_lookup_foi[(e[i] + age2date[i]):
                    #                            ((dn[i] - 1) + age2date[i])]])
            } else { # males
                # survival hazard for susceptible deer
                lam_sus[e[i]:(dn[i] - 1)] <- exp(beta0_sus +
                    age_effect_surv[e[i]:(dn[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                                       (dn[i] - 1 + age2date[i])] +
                    beta_male)
                # survival hazard while infected
                lam_inf[e[i]:(s[i] - 1)] <- exp(beta0_inf +
                    age_effect_surv[e[i]:(s[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                    (s[i] - 1 + age2date[i])] +
                    beta_male)
                # survival hazard for susceptible deer
                lam_sus[e[i]:(dn[i] - 1)] <- exp(beta0_sus +
                    age_effect_surv[e[i]:(dn[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                                       (dn[i] - 1 + age2date[i])] +
                    beta_male)
                # force of infection infection hazard
                lam_foi[e[i]:(dn[i] - 1)] <- exp(space[sect[i]] +
                    m_age_foi[age_lookup_m[e[i]:(dn[i] - 1)]] )
                    # m_period_foi[period_lookup_foi[(e[i] + age2date[i]):
                    #                                ((dn[i] - 1) + age2date[i])]])
            }

            #######################################
            ### calculating the joint likelihood
            #######################################
            lik_temp <- lik_temp + 
                        (1 - exp(-lam_foi[(e[i])])) *
                exp(-sum(lam_inf[(e[i]):(r[i] - 1)]))

            for (k in (e[i]+ 1):(dn[i] - 1)) {
                lik_temp <- lik_temp + 
                    (1 - exp(-lam_foi[k])) *
                    exp(-sum(lam_inf[k:(r[i] - 1)])) *
                    exp(-sum(lam_foi[e[i]:(k - 1)])) *
                    exp(-sum(lam_sus[e[i]:(k - 1)]))
            }
            sumllik <- sumllik + 
                       log(1 - exp(-sum(lam_inf[r[i]:(s[i] - 1)]))) +
                       log(lik_temp)
        }
        returnType(double(0))
        if (log) {
            return(sumllik)
        } else {
            return(exp(sumllik))
        } ## return log-likelihood
    }
)

nimble::registerDistributions(list(
    dRecPosMort = list(
        BUGSdist = "dRecPosMort(n_samples,e,r,s,dn,sex,age2date,beta_male,beta0_sus,beta0_inf,age_effect_surv,period_effect_surv,f_age_foi,m_age_foi,age_lookup_f,age_lookup_m,space,sect)",
        types = c(
            "value = integer(0)",
            "n_samples = integer(0)",
            "e = double(1)",
            "r = double(1)",
            "s = double(1)",
            "dn = double(1)",
            "sex = double(1)",
            "age2date = double(1)",
            "beta_male = double(0)",
            "beta0_sus = double(0)",
            "beta0_inf = double(0)",
            "age_effect_surv = double(1)",
            "period_effect_surv = double(1)",
            "f_age_foi = double(1)",
            "m_age_foi = double(1)",
            "age_lookup_f = double(1)",
            "age_lookup_m = double(1)",
            # "period_lookup_foi=double(1)",
            # "f_period_foi=double(1)",
            # "m_period_foi=double(1)",
            "space = double(1)",
            "sect = double(1)",
            "log = double(0)"
        ),
        discrete = TRUE
    )
))

### Global Declaration so Nimble can access
assign("dRecPosMort", dRecPosMort, envir = .GlobalEnv)

# starttime <- Sys.time()
# test <-  dRecPosMort(
#         x = 1,
#         n_samples = nrow(d_fit_rec_pos_mort),
#         e = d_fit_rec_pos_mort$left_age_e,
#         r = d_fit_rec_pos_mort$right_age_r,
#         s = d_fit_rec_pos_mort$right_age_s,
#         dn = d_fit_rec_pos_mort$ageweek_recap,
#         sex = d_fit_rec_pos_mort$sex,
#         age2date = rec_pos_mort_age2date,
#         beta_male = beta_male,
#         beta0_sus = beta0_survival_sus,
#         beta0_inf = beta0_survival_inf,
#         age_effect_surv = age_effect_survival_test,
#         period_effect_surv = period_effect_surv,
#         f_age_foi = f_age_foi,
#         m_age_foi = m_age_foi,
#         age_lookup_f = age_lookup_f,
#         age_lookup_m = age_lookup_m,
#         # period_lookup_foi = period_lookup_foi,
#         # f_period_foi = f_period_foi,
#         # m_period_foi = m_period_foi,
#         sect = d_fit_rec_pos_mort$study_area,
#         space = c(0,-.55),
#         log = TRUE
#         )
# (end<- Sys.time()-starttime)
# test


#######################################################################
###
###   User defined distribution for likelihood for
###   infected deer mortalities for radio marked deer that
###   enter the study as test negative at capture
###
###   d_fit_idead
###
###   Overleaf Equation (24)
###
#######################################################################

dNegCapPosMort <- nimble::nimbleFunction(
    run = function( ### argument type declarations
                   x = integer(0),
                   n_samples = integer(0), # number of samples in dataset
                   e = double(1), # e, age of entry
                   r = double(1), # r, age of last known alive
                   s = double(1), # s, age of known mortality
                   sex = double(1),
                   age2date = double(1),
                   beta_male = double(0),
                   beta0_sus = double(0),
                   beta0_inf = double(0),
                   age_effect_surv = double(1),
                   period_effect_surv = double(1),
                   f_age_foi = double(1),
                   m_age_foi = double(1),
                   age_lookup_f = double(1),
                   age_lookup_m = double(1),
                #    period_lookup_foi = double(1),
                #    f_period_foi = double(1),
                #    m_period_foi = double(1),
                   space = double(1),
                   sect = double(1),
                   log = double()) {
        sumllik <- 0 # intialize log-likelihood
        for (i in 1:n_samples) {
            lam_foi <- nimNumeric(s[i] - 1, init = FALSE)
            lam_sus <- nimNumeric(s[i] - 1, init = FALSE)
            lam_inf <- nimNumeric(s[i] - 1, init = FALSE)
            lik_temp <- 0

            #############################################
            # preliminary hazards for the likelihood
            #############################################
            if (sex[i] == 0) { # females
                # survival hazard for susceptible deer
                lam_sus[e[i]:(s[i] - 1)] <- exp(beta0_sus +
                    age_effect_surv[e[i]:(s[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                                       (s[i] - 1 + age2date[i])])
                # survival hazard while infected
                lam_inf[e[i]:(s[i] - 1)] <- exp(beta0_inf +
                    age_effect_surv[e[i]:(s[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                                       (s[i] - 1 + age2date[i])])
                # force of infection infection hazard
                lam_foi[e[i]:(s[i] - 1)] <- exp(space[sect[i]] +
                    f_age_foi[age_lookup_f[e[i]:(s[i] - 1)]]) 
                    # f_period_foi[period_lookup_foi[(e[i] + age2date[i]):
                    #                            (e[i] - 1 + age2date[i])]])
            } else { # males
                # survival hazard for susceptible deer
                lam_sus[e[i]:(s[i] - 1)] <- exp(beta0_sus +
                    age_effect_surv[e[i]:(s[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                                       (s[i] - 1 + age2date[i])] +
                    beta_male)
                # survival hazard while infected
                lam_inf[e[i]:(s[i] - 1)] <- exp(beta0_inf +
                    age_effect_surv[e[i]:(s[i] - 1)] +
                    period_effect_surv[(e[i] + age2date[i]):
                                       (s[i] - 1 + age2date[i])] +
                    beta_male)
                # force of infection infection hazard
                lam_foi[e[i]:(s[i] - 1)] <- exp(space[sect[i]] +
                    m_age_foi[age_lookup_m[e[i]:(s[i] - 1)]])
                    # m_period_foi[period_lookup_foi[(e[i] + age2date[i]):
					#                            (e[i] - 1 + age2date[i])]] +
            }

            #######################################
            ### calculating the joint likelihood
            #######################################
            lik_temp <- lik_temp +
                        (1 - exp(-lam_foi[e[i]])) *
                        exp(-sum(lam_inf[e[i]:(r[i] - 1)]))

            for (k in (e[i] + 1):(r[i] - 1)) {
                    lik_temp <- lik_temp +
                                (1 - exp(-lam_foi[k])) *
                                exp(-sum(lam_inf[(k):(r[i] - 1)])) *
                                exp(-sum(lam_sus[e[i]:(k - 1)])) *
                                exp(-sum(lam_foi[e[i]:(k - 1)]))
            }
            lik_temp <- lik_temp +
                        (1 - exp(-lam_foi[s[i] - 1])) *
                        exp(-sum(lam_sus[e[i]:((s[i] - 1) - 1)])) *
                        exp(-sum(lam_foi[e[i]:((s[i] - 1) - 1)]))

            sumllik <- sumllik +
                       log(1 - exp(-sum(lam_inf[r[i]:(s[i] - 1)]))) +
                       log(lik_temp)
        }
        returnType(double(0))
        if (log) {
            return(sumllik)
        } else {
            return(exp(sumllik))
        } ## return log-likelihood
    }
)


nimble::registerDistributions(list(
    dNegCapPosMort = list(
        BUGSdist = "dNegCapPosMort(n_samples,e,r,s,sex,age2date,beta_male,beta0_sus,beta0_inf,age_effect_surv,period_effect_surv,f_age_foi,m_age_foi,age_lookup_f,age_lookup_m,space,sect)",
        types = c(
            "value = integer(0)",
            "n_samples = integer(0)",
            "e = double(1)",
            "r = double(1)",
            "s = double(1)",
            "sex = double(1)",
            "age2date = double(1)",
            "beta_male = double(0)",
            "beta0_sus = double(0)",
            "beta0_inf = double(0)",
            "age_effect_surv = double(1)",
            "period_effect_surv = double(1)",
            "f_age_foi = double(1)",
            "m_age_foi = double(1)",
            "age_lookup_f = double(1)",
            "age_lookup_m = double(1)",
            # "period_lookup_foi = double(1)",
            # "f_period_foi = double(1)",
            # "m_period_foi = double(1)",
            "space = double(1)",
            "sect = double(1)",
            "log = double()"
        ),
        discrete = TRUE
    )
))

### for a user-defined distribution
assign("dNegCapPosMort", dNegCapPosMort, envir = .GlobalEnv)

# starttime <- Sys.time()
# test <-  dNegCapPosMort(
#         x = 1,
#         n_samples = nrow(d_fit_idead),
#         e = d_fit_idead$left_age_e,
#         r = d_fit_idead$right_age_r,
#         s = d_fit_idead$right_age_s,
#         sex = d_fit_idead$sex,
#         age2date = idead_age2date,
#         beta_male = beta_male,
#         beta0_sus = beta0_survival_sus,
#         beta0_inf = beta0_survival_inf,
#         age_effect_surv = age_effect_survival_test,
#         period_effect_surv = period_effect_surv,
#         f_age_foi = f_age_foi,
#         m_age_foi = m_age_foi,
#         age_lookup_f = age_lookup_f,
#         age_lookup_m = age_lookup_m,
#         # period_lookup_foi = period_lookup_foi,
#         # f_period_foi = f_period_foi,
#         # m_period_foi = m_period_foi,
#         space = c(0,-.55),
#         sect = d_fit_idead$study_area,
#         log = TRUE
#         )
# (end<- Sys.time()-starttime)
# test


#######################################################################
###
###   User defined distribution for likelihood for
###  harvested deer, revised for multiple deer
###
###   d_fit_hunt
###
#######################################################################


dFOIhunt <- nimble::nimbleFunction( # nolint
    run = function( # nolint
                   ### argument type declarations
                   x = integer(0),
                   y = double(1),
                   n_cases = double(1),
                   n_samples = integer(0), # number of samples in dataset
                   a = double(1), # age (weeks) at harvest
                   sex = double(1),
                   f_age_foi = double(1),
                   m_age_foi = double(1),
                   age_lookup_f = double(1),
                   age_lookup_m = double(1),
                   space = double(1),
                   sect = double(1),
                   log = double(0)) {

        # start the loop through individuals
        sumllik <- 0
        for (i in 1:n_samples) {

            # intitialize scalars

            #################################################
            ### loop over ages and accumulate sums over 1:a-1
            ### have to loop separately for lam_inf
            #################################################

            if (sex[i] == 0) { # age loops for females
                lik_foi <- 0
                lam_foij <- 0
                for (j in 1:(a[i] - 1)) {
                    # sum up foi hazard from 1  to j
                    lam_foij <- exp(space[sect[i]] +
                        f_age_foi[age_lookup_f[j]])
                    # sum up like_temp (no sus hazard when j=1)
                    lik_foi <- lik_foi + lam_foij 
                }
                p <- 1 - exp(-lik_foi)
                lik_temp <- dbinom(y[i],1,p,log=TRUE)
            } else { # age loops for males
                lik_foi <- 0
                lam_foij <- 0
                for (j in 1:(a[i] - 1)) {
                    # sum up foi hazard from 1  to j
                    lam_foij <- exp(space[sect[i]] +
                        m_age_foi[age_lookup_f[j]]) 
                    # sum up like_temp (no sus hazard when j=1)
                    lik_foi <- lik_foi + lam_foij 
                }
                p <- 1 - exp(-lik_foi)
                lik_temp <- dbinom(y[i],1,p,log=TRUE)
            } # end if(sex)

            #######################################
            ### accumulate the joint likelihood
            #######################################
            # if(is.na(lik_temp)){stop("ack")}
            sumllik <- sumllik + lik_temp * n_cases[i]
        } # end the loop for individual i
        returnType(double(0))
        if (log) {
            return(sumllik)
        } else {
            return(exp(sumllik))
        }
    }
)

nimble::registerDistributions(list(
    dFOIhunt = list(
        BUGSdist = "dFOIhunt(y,n_cases,n_samples,a,sex,f_age_foi,m_age_foi,age_lookup_f,age_lookup_m,space,sect)",
        types = c(
            "value = integer(0)",
            "y = double(1)",
            "n_cases = double(1)",
            "n_samples = integer(0)",
            "a = double(1)",
            "sex = double(1)",
            "f_age_foi = double(1)",
            "m_age_foi = double(1)",
            "age_lookup_f = double(1)",
            "age_lookup_m = double(1)",
            "space = double(1)",
            "sect = double(1)",
            "returnType = double(0)",
            "log = double(0)"
        ),
        discrete = TRUE
    )
))

### for a user-defined distribution
assign("dFOIhunt",
    dFOIhunt,
    envir = .GlobalEnv
)
# starttime <- Sys.time()
# test <- dFOIhunt(
#     x = 1,
#     y = d_fit_hunt$teststatus,
#     n_cases = d_fit_hunt$n_cases,
#     n_samples = nrow(d_fit_hunt),
#     a = d_fit_hunt$ageweeks,
#     sex = d_fit_hunt$sex,
#     f_age_foi = f_age_foi,
#     m_age_foi = m_age_foi,
#     age_lookup_f = age_lookup_f,
#     age_lookup_m = age_lookup_m,
#     space = c(0, -.55),
#     sect = d_fit_hunt$ew,
#     log = TRUE
# )
# (end <- Sys.time() - starttime)
# test

#######################################################################
###
###   User defined distribution for likelihood for
###  harvested deer, revised for multiple deer
###  the following version includes period effects
###
###   d_fit_hunt
###
#######################################################################


# dFOIhunt <- nimble::nimbleFunction( # nolint
#     run = function( # nolint
#                    ### argument type declarations
#                    x = integer(0),
#                    y = double(1),
#                    n_cases = double(1),
#                    n_samples = integer(0), # number of samples in dataset
#                    a = double(1), # age (weeks) at harvest
#                    sex = double(1),
#                    age2date = double(1),
#                    f_age_foi = double(1),
#                    m_age_foi = double(1),
#                    age_lookup_f = double(1),
#                    age_lookup_m = double(1),
#                    period_lookup_foi = double(1),
#                    f_period_foi = double(1),
#                    m_period_foi = double(1),
#                    space = double(1),
#                    sect = double(1),
#                    log = double(0)) {

#         # start the loop through individuals
#         sumllik <- 0
#         for (i in 1:n_samples) {

#             # intitialize scalars

#             #################################################
#             ### loop over ages and accumulate sums over 1:a-1
#             ### have to loop separately for lam_inf
#             #################################################

#             if (sex[i] == 0) { # age loops for females
#                 lik_foi <- 0
#                 lam_foij <- 0
#                 for (j in 1:(a[i] - 1)) {
#                     # sum up foi hazard from 1  to j
#                     lam_foij <- exp(space[sect[i]] +
#                         f_age_foi[age_lookup_f[j]] +
#                         f_period_foi[period_lookup_foi[age2date[i] + j]])
#                     # sum up like_temp (no sus hazard when j=1)
#                     lik_foi <- lik_foi + lam_foij 
#                 }
#                 p <- 1 - exp(-lik_foi)
#                 lik_temp <- dbinom(y[i],1,p,log=TRUE)
#             } else { # age loops for males
#                 lik_foi <- 0
#                 lam_foij <- 0
#                 for (j in 1:(a[i] - 1)) {
#                     # sum up foi hazard from 1  to j
#                     lam_foij <- exp(space[sect[i]] +
#                         m_age_foi[age_lookup_f[j]] +
#                         m_period_foi[period_lookup_foi[age2date[i] + j]])
#                     # sum up like_temp (no sus hazard when j=1)
#                     lik_foi <- lik_foi + lam_foij 
#                 }
#                 p <- 1 - exp(-lik_foi)
#                 lik_temp <- dbinom(y[i],1,p,log=TRUE)
#             } # end if(sex)

#             #######################################
#             ### accumulate the joint likelihood
#             #######################################
#             # if(is.na(lik_temp)){stop("ack")}
#             sumllik <- sumllik + lik_temp * n_cases[i]
#         } # end the loop for individual i
#         returnType(double(0))
#         if (log) {
#             return(sumllik)
#         } else {
#             return(exp(sumllik))
#         }
#     }
# )

# nimble::registerDistributions(list(
#     dFOIhunt = list(
#         BUGSdist = "dFOIhunt(y,n_cases,n_samples,a,sex,age2date,f_age_foi,m_age_foi,age_lookup_f,age_lookup_m,f_period_foi,m_period_foi,period_lookup_foi,space,sect)",
#         types = c(
#             "value = integer(0)",
#             "y = double(1)",
#             "n_cases = double(1)",
#             "n_samples = integer(0)",
#             "a = double(1)",
#             "sex = double(1)",
#             "age2date = double(1)",
#             "f_age_foi = double(1)",
#             "m_age_foi = double(1)",
#             "age_lookup_f = double(1)",
#             "age_lookup_m = double(1)",
#             "f_period_foi = double(1)",
#             "m_period_foi = double(1)",
#             "period_lookup_foi = double(1)",
#             "space = double(1)",
#             "sect = double(1)",
#             "returnType = double(0)",
#             "log = double(0)"
#         ),
#         discrete = TRUE
#     )
# ))

# ### for a user-defined distribution
# assign("dFOIhunt",
#     dFOIhunt,
#     envir = .GlobalEnv
# )
# # starttime <- Sys.time()
# # test <- dFOIhunt(
# #     x = d_fit_hunt$teststatus,
# #     n_cases = d_fit_hunt$n_cases,
# #     n_samples = nrow(d_fit_hunt),
# #     a = d_fit_hunt$ageweeks,
# #     sex = d_fit_hunt$sex,
# #     age2date = d_fit_hunt$birthweek - 1,
# #     f_age_foi = f_age_foi,
# #     m_age_foi = m_age_foi,
# #     age_lookup_f = age_lookup_f,
# #     age_lookup_m = age_lookup_m,
# #     period_lookup_foi = period_lookup_foi_hunt,
# #     f_period_foi = f_period_foi,
# #     m_period_foi = m_period_foi,
# #     space = c(0, -.55),
# #     sect = d_fit_hunt$ew,
# #     log = TRUE
# # )
# # (end <- Sys.time() - starttime)
# # test
# #     x = d_fit_hunt$teststatus
# #     n_cases = d_fit_hunt$n_cases
# #     n_samples = nrow(d_fit_hunt)
# #     a = d_fit_hunt$ageweeks
# #     sex = d_fit_hunt$sex
# #     age2date = d_fit_hunt$birthweek - 1
# #     f_age_foi = f_age_foi
# #     m_age_foi = m_age_foi
# #     age_lookup_f = age_lookup_f
# #     age_lookup_m = age_lookup_m
# #     period_lookup_foi = period_lookup_foi
# #     f_period_foi = f_period_foi
# #     m_period_foi = m_period_foi
# #     space = c(0, -.55)
# #     sect = d_fit_hunt$ew


