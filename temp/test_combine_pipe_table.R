data <- read.csv("temp/test_data.csv")
data2 <- read.csv("temp/test_data_2.csv")
combine_pipe_table(data, var_start = "Q1C1", 
                   var_end = "Q4C3",
                   mr_C_max = 3, 
                   steps = 2,
                   rep_place = "q")

combine_pipe_table(data2, var_start = "Q1C1", 
                   var_end = "Q2C6",
                   mr_C_max = 6, 
                   steps = 3,
                   rep_place = "c")
