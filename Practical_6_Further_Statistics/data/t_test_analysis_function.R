run_analysis <- function() {
  simulation_once <- simulate_data()

  # Create the data frames
  ocat_df <- tibble(group = "orange_cats",
                    speed = simulation_once$orange_cats)

  bcat_df <- tibble(group = "black_cats",
                    speed = simulation_once$black_cats)
  
  cat_df <- bind_rows(ocat_df, bcat_df)
  
  nbdog_df <- tibble(group = "dogs_no_biscuit",
                     speed = simulation_once$dogs_no_biscuit)
  
  bdog_df <- tibble(group = 'dogs_biscuit',
                    speed = simulation_once$dogs_biscuit)
  
  dog_df <- bind_rows(nbdog_df, bdog_df)
  
  # Create summary data frames
  summ_df_cat <- cat_df |>
    group_by(group) |>
    summarise(
      group_mean = mean(speed),
      group_sd = sd(speed)
    )

  summ_df_dog <- dog_df |>
    group_by(group) |>
    summarise(
      group_mean = mean(speed),
      group_sd = sd(speed)
    )

  cat ("Summary Statistics - Cat\n")
  print (gt(summ_df_cat))
  
  cat ("Summary Statistics - Dog\n")
  print (gt(summ_df_dog))

  # Do the t-tests
  t_test_cat <- t.test(speed ~ group,
                       data = cat_df,
                       var.equal = TRUE)
  
  cat ("T-test - Cat\n")
  print (summary(t_test_cat))
 
  t_test_dog <- t.test(speed ~ group,
                       data = dog_df,
                       var.equal = TRUE)

  cat ("T-test - Dog\n")
  print (summary(t_test_dog))

  # Fit the linear model
  linear_model_cat <- lm(speed ~ group, data = cat_df)
  linear_model_dog <- lm(speed ~ group, data = dog_df)
  
  # Put the residuals from the linear model into the
  # data frame
  cat_df <- cat_df |> mutate(
    residuals = linear_model_dog$residuals
  )

  dog_df <- dog_df |> mutate(
    residuals = linear_model_dog$residuals
  )
  
  # Plot a histogram showing the distribution of the
  # residuals in each group.
  distri_cat <- ggplot(cat_df, aes(x = residuals)) +
    geom_histogram(bins=15) +
    facet_wrap(~group) +
    theme_bw()
  print ("Distribution of Residuals - Cat")
  print (distri_cat)

  distri_dog <- ggplot(dog_df, aes(x = residuals)) +
    geom_histogram(bins=15) +
    facet_wrap(~group) +
    theme_bw()
  print ("Distribution of Residuals - Dog")
  print (distri_dog)

}