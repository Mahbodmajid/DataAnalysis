# Evaluate homoscedasticity
# non-constant error variance test
lmtest::bptest(fit_sig)  # Breusch-Pagan test
ncvTest(fit_sig)  # Breusch-Pagan test
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit_sig)
olsrr::ols_plot_resid_fit(fit_sig)


#No autocorrelation of residuals
acf(fit_sig$residuals)
# Method 2: Runs test to test for randomness
lawstat::runs.test(fit_sig$residuals)
# Method 3: Durbin-Watson test
lmtest::dwtest(fit_sig)
#=>   Durbin-Watson test


# Normality of Residuals
# qq plot for studentized resid
olsrr::ols_plot_resid_qq(fit_sig)
olsrr::ols_test_normality(fit_sig)


# all together
gvlma::gvlma(fit_sig)
