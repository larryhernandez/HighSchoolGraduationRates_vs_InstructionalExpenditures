# Load Adjusted Cohort Graduation Rates (ACGR)
fn_grad_rates = "C:/Users/Larry/graduation_rates/AGCR_2012_2013.csv"
grad_rates <-read.csv(fn_grad_rates, header = TRUE, sep = ",", dec = ".", fill = TRUE)

# Load Income and poverty data
fn_income = "C:/Users/Larry/graduation_rates/CDP03.2_105_USSchoolDistrictAll_217191048912.csv"
income_data = read.csv(fn_income, header = TRUE, sep = "|", dec = ".", fill = TRUE)

# Load demographics and instructional expenses data
fn_demographics_and_expenses = "C:/Users/Larry/graduation_rates/ELSI_csv_export_6359497038941595917333.csv"
demographics <-read.csv(fn_demographics_and_expenses, header = TRUE, sep = ",", dec = ".", skip = 6, fill = TRUE, nrows = 18567)