# Calculate MSEs
mse_final_model <- mean(final_model$residuals^2)
mse_final_model
mse_final_model_rm <- mean(final_model_rm$residuals^2)
mse_final_model_rm

#cross-validation prediction (out-of-sample error) on cross-validation set for both final models (with and without outliers included)
cv_final_model <- predict(final_model, test_data)
cv_final_model #out-of-sample error for final model incl. outliers
cv_final_model_rm <- predict(final_model_rm, test_data)
cv_final_model_rm #out-of-sample error for final model excl. outliers
