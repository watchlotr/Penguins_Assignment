###FUNCTIONS FOR MAKING GRAPHS###

#For making the scatter graph for culmen data with no regression line - no species considered
plot_culmen_basic <- function(penguins_culmen){
  penguins_culmen %>% 
    ggplot(aes(x = culmen_length_mm, y = culmen_depth_mm)) +
    geom_point() +
    theme_bw()
}


#For making the scatter graph for culmen data with no regression line - species considered
plot_culmen_no_reg <- function(penguins_culmen){
  penguins_culmen %>% 
    ggplot(aes(x = culmen_length_mm, y = culmen_depth_mm)) +
    geom_jitter(aes(color = species), alpha = 0.6, position = position_jitter(seed = 0)) +
    scale_color_manual(values = c("darkorange","purple","cyan4")) +
    theme_bw()
}


#For making the scatter graph for culmen data with a single regression line.
plot_culmen_one_reg <- function(penguins_culmen,
                                rVal){
  penguins_culmen %>% 
    ggplot(aes(x = culmen_length_mm, y = culmen_depth_mm)) +
    geom_jitter(aes(color = species), alpha = 0.6, position = position_jitter(seed = 0)) +
    scale_color_manual(values = c("darkorange","purple","cyan4")) +
    labs(x = "Culmen Length (mm)",
         y = "Culmen Depth (mm)",
         title = "The association between the culmen length and depth across penguin species") +
    geom_smooth(method=lm) + 
    geom_text(aes(label = rVal, y = 13, x = 57)) +
    theme_bw()
}

#For making the scatter graph for culmen data with regression lines for each species
plot_culmen_three_reg <- function(penguins_culmen,
                                  r_val_list){
  penguins_culmen %>% 
    ggplot(aes(x = culmen_length_mm, y = culmen_depth_mm)) +
    geom_jitter(aes(color = species), alpha = 0.6, position = position_jitter(seed = 0), show.legend = FALSE) +
    scale_color_manual(values = c("darkorange","purple","cyan4")) +
    labs(x = "Culmen Length (mm)",
         y = "Culmen Depth (mm)",
         title = "The association between the culmen length and depth within penguin species") +
    geom_smooth(method=lm, aes(colour = species), show.legend = FALSE) + 
    facet_wrap(~species) +
    geom_text(r_val_list,
              mapping = aes(x = x,
                            y = y,
                            label = label)) +
    theme_bw()
}





###FUNCTIONS FOR SAVING GRAPHS###

#For making/saving a SVG scatter graph image for my Exploratory figure - no species considered
save_culmen_explore1_svg <- function(penguins_culmen,
                                  filename, size, scaling){
  size_inches = size/2.54 #Units are inches
  svglite(filename, width   = size_inches, 
          height  = 2*size_inches/3, 
          scaling = scaling)
  culmen_scatter <- plot_culmen_basic(penguins_culmen)
  print(culmen_scatter)
  dev.off()
}

#For making/saving a SVG scatter graph image for my Exploratory Figure - species considered
save_culmen_explore2_svg <- function(penguins_culmen,
                                 filename, size, scaling){
  size_inches = size/2.54 #Units are inches
  svglite(filename, width   = size_inches, 
          height  = 2*size_inches/3, 
          scaling = scaling)
  culmen_scatter <- plot_culmen_no_reg(penguins_culmen)
  print(culmen_scatter)
  dev.off()
}

#For making/saving a SVG scatter graph image for my Results Figure - One Regression
save_culmen_results1_svg <- function(penguins_culmen,
                                     rVal, filename, size, scaling){
  size_inches = size/2.54 #Units are inches
  svglite(filename, width   = size_inches, 
          height  = 2*size_inches/3, 
          scaling = scaling)
  culmen_scatter <- plot_culmen_one_reg(penguins_culmen,
                                        rVal)
  print(culmen_scatter)
  dev.off()
}

#For making/saving a SVG scatter graph image for my Results Figure - One Regression for each species
save_culmen_results2_svg <- function(penguins_culmen,
                                 r_val_list, filename, size, scaling){
  size_inches = size/2.54 #Units are inches
  svglite(filename, width   = size_inches, 
          height  = 2*size_inches/3, 
          scaling = scaling)
  culmen_scatter <- plot_culmen_three_reg(penguins_culmen,
                                          r_val_list)
  print(culmen_scatter)
  dev.off()
}



