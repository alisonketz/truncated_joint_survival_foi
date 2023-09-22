calc_surv_harv_post <- nimble::nimbleFunction(
    run = function(
        ### argument type declarations
        nT_age = double(0),
        nT_age_short_f = double(0),
        nT_age_short_m = double(0),
        nT_age_surv_aah_f = double(0),
        nT_age_surv_aah_m = double(0),
        nT_period = double(0),
        yr_start_age = double(1),
        yr_start_pop = double(1),
        ng_start = double(1),
        gun_start = double(1),
        gun_end = double(1),
        ng_end = double(1),
        n_sex = double(0),
        n_year = double(0),
        n_agef = double(0),
        n_agem = double(0),
        n_iter = double(0),
        beta0 = double(1),
        beta_male = double(1),
        age_effect = double(2),
        period_effect = double(2),
        p_nogun_f = double(1),
        p_nogun_m = double(1),
        p_gun_f = double(1),
        p_gun_m = double(1)
        ) {

    ###################################################################
    ###
    ### General Survival Surfaces for Susceptible/Infected Individuals
    ###
    ###################################################################

    s_hunt <- nimArray(NA, c(n_iter, 2 ,n_sex, n_agef, n_year))

    for(k in 1:n_iter){

        # if(n_iter>1){
            mu_old_age_effect_f <- mean(age_effect[k,(nT_age_short_f + 1):nT_age])
            mu_old_age_effect_m <- mean(age_effect[k,(nT_age_short_m + 1):nT_age])
        # } else{
        #     mu_old_age_effect_f <- mean(age_effect[(nT_age_short_f + 1):nT_age])
        #     mu_old_age_effect_m <- mean(age_effect[(nT_age_short_m + 1):nT_age])
        #     # age_effect[1,1:nT_age] <- nimMatrix(age_effect[1:nT_age], nrow = 1,ncol = nT_age)
        #     # period_effect[1,1:nT_period] <- nimMatrix(period_effect[1:nT_period], nrow = 1,ncol = nT_period)
        # }


        ############################################
        ### Initialize hazard array
        ############################################

        UCH <- nimArray(NA, c(n_sex, nT_age_surv_aah_f, nT_period))
        UCH_hunt <- nimArray(NA, c(n_sex, nT_age_surv_aah_f, nT_period))

        ############################################
        ### Calculate hazards
        ############################################

        ### Females
        for(j in 1:nT_period) {
            for(i in 1:nT_age_short_f) {
                UCH[1, i, j] <- exp(beta0[k] +
                                    age_effect[k,i] +
                                    period_effect[k,j])
            }
            for(i in (nT_age_short_f + 1):(nT_age_surv_aah_f)) {
                UCH[1, i, j] <- exp(beta0[k] +
                                    mu_old_age_effect_f +
                                    period_effect[k,j])
            }
        }
        ### Males
        for(j in 1:nT_period) {
            for(i in 1:nT_age_short_m) {
                UCH[2, i, j] <- exp(beta0[k] +
                                    beta_male[k] +
                                    age_effect[k,i] +
                                    period_effect[k,j])
            }
            for(i in (nT_age_short_m + 1):(nT_age_surv_aah_m)) {
                UCH[2, i, j] <- exp(beta0[k] +
                                    beta_male[k] +
                                    mu_old_age_effect_m +
                                    period_effect[k,j])
            }
        }
        ############################################
        # adjust hazards to remove harvest hazards
        ############################################
        UCH_hunt[1, 1:nT_age_surv_aah_f, 1:nT_period] <- UCH[1, 1:nT_age_surv_aah_f, 1:nT_period]
        UCH_hunt[2, 1:nT_age_surv_aah_f, 1:nT_period] <- UCH[2, 1:nT_age_surv_aah_f, 1:nT_period]

        for(i in 1:n_year){
            for(j in ng_start[i]:(gun_start[i] - 1)){
                UCH_hunt[1, 1:nT_age_surv_aah_f, j] <- UCH[1, 1:nT_age_surv_aah_f, j] * p_nogun_f[k]
                UCH_hunt[2, 1:nT_age_surv_aah_m, j] <- UCH[2, 1:nT_age_surv_aah_m, j] * p_nogun_m[k]
            }
            for(j in gun_start[i]:(gun_end[i])){
                UCH_hunt[1, 1:nT_age_surv_aah_f, j] <- UCH[1, 1:nT_age_surv_aah_f, j] * p_gun_f[k]
                UCH_hunt[2, 1:nT_age_surv_aah_m, j] <- UCH[2, 1:nT_age_surv_aah_m, j] * p_gun_m[k]
            }
            if(gun_end[i] < ng_end[i]){
            for(j in (gun_end[i] + 1):(ng_end[i])){
                UCH_hunt[1, 1:nT_age_surv_aah_f, j] <- UCH[1, 1:nT_age_surv_aah_f, j] * p_nogun_f[k]
                UCH_hunt[2, 1:nT_age_surv_aah_m, j] <- UCH[2, 1:nT_age_surv_aah_m, j] * p_nogun_m[k]
        }}
        }

        ############################################
        # calculate survival from cum haz
        ############################################
        for (t in 1:n_year) {
            for (a in 1:n_agef) {
                s_hunt[k, 1, 1, a, t] <- exp(-sum(diag(UCH_hunt[1,
                                    (yr_start_age[a] + length(yr_start_pop[t]:(ng_start[t]-1))):(yr_start_age[a] + length(ng_start[t]:ng_end[t])+ length(yr_start_pop[t]:(ng_start[t]-1))),
                                    ng_start[t]:ng_end[t]])))
            }
            for(a in 1:n_agem) {
                s_hunt[k, 1, 2, a, t] <- exp(-sum(diag(UCH_hunt[2,
                                   (yr_start_age[a] + length(yr_start_pop[t]:(ng_start[t]-1))):(yr_start_age[a] + length(ng_start[t]:ng_end[t])+ length(yr_start_pop[t]:(ng_start[t]-1))),
                                    yr_start_pop[t]:ng_end[t]])))
            }
            for (a in 1:n_agef) {
                s_hunt[k, 2, 1, a, t] <- exp(-sum(diag(UCH_hunt[1,
                                    (yr_start_age[a] + length(yr_start_pop[t]:(gun_start[t]-1))):(yr_start_age[a] + length(gun_start[t]:gun_end[t]) - 1 + length(yr_start_pop[t]:(gun_start[t] - 1))),
                                    gun_start[t]:gun_end[t]])))
            }
            for(a in 1:n_agem) {
                s_hunt[k, 2, 2, a, t] <- exp(-sum(diag(UCH_hunt[2,
                                    (yr_start_age[a] + length(yr_start_pop[t]:(gun_start[t]-1))):(yr_start_age[a] + length(gun_start[t]:gun_end[t]) - 1 + length(yr_start_pop[t]:(gun_start[t] - 1))),
                                    gun_start[t]:gun_end[t]])))
            }

        }


    }
#   returnType(double(3))
#   UCH_hunt[1:n_sex, 1:nT_age_surv_aah_f, 1:nT_period]

  returnType(double(5))
  return(s_hunt[1:n_iter,1:2,1:n_sex,1:n_agef,1:n_year])
})

Ccalc_surv_harv_post <- compileNimble(calc_surv_harv_post)


n_iter <- length(beta0_survival_sus)

s_hunt_sus <- Ccalc_surv_harv_post(
        ### argument type declarations
        nT_age = nT_age_surv,
        nT_age_short_f = nT_age_short_f,
        nT_age_short_m = nT_age_short_m,
        nT_age_surv_aah_f = nT_age_surv_aah_f,
        nT_age_surv_aah_m = nT_age_surv_aah_f,
        nT_period = nT_period_collar,
        yr_start_age = yr_start_age,
        yr_start_pop = d_fit_season_pop$yr_start,
        ng_start = d_fit_season$ng_start,
        gun_start = d_fit_season$gun_start,
        gun_end = d_fit_season$gun_end,
        ng_end = d_fit_season$ng_end,
        n_sex = n_sex,
        n_year = n_year,
        n_agef = n_agef,
        n_agem = n_agem,
        n_iter = n_iter,
        beta0 = beta0_survival_sus,
        beta_male = beta_male,
        age_effect = age_effect_survival,
        period_effect = period_effect_survival,
        p_nogun_f = p_nogun_f,
        p_nogun_m = p_nogun_m,
        p_gun_f = p_gun_f,
        p_gun_m = p_gun_m
        ) 
