### Biểu đồ 1:
#| warning: false
#| message: false
#| echo: false
library(ggplot2)
library(gganimate)

vehicle_positions <- arrivals %>%
  mutate(
    # Add the position of the vehicle at each time step
    position = case_when(
      resource == "warehouse" ~ "Warehouse",
      resource == "truck" ~ "Truck",
      resource == "store" ~ "Store"
    ),
    time_seconds = as.numeric(difftime(datetime, 
                                       as.POSIXct("2024-11-09 08:30:00", format="%Y-%m-%d %H:%M:%S"), 
                                       units = "secs"))
  )

# Create an animated plot of vehicle movement over time
animation <- ggplot(vehicle_positions, aes(x = time_seconds, y = name, color = position)) +
  geom_point(aes(size = 3)) +
  geom_segment(aes(xend = time_seconds, yend = name, color = position), size = 1) +
  scale_color_manual(values = c("Warehouse" = "blue", "Truck" = "green", "Store" = "red")) +
  labs(title = "Vehicle Movement Over Time", x = "Time (seconds)", y = "Vehicle", color = "Position") +
  theme_minimal() +
  theme(legend.position = "top") +
  transition_reveal(time_seconds) + # Create time-based transition
  ease_aes('linear')

# Animate and save the plot as a GIF
animate(animation, 
        nframes = 100, 
        fps = 10, 
        width = 800, 
        height = 600)

##Run it to save: 
anim_save("DES.gif")


#| warning: false
#| message: false

library(ggplot2)
library(gganimate)
library(transformr)

set.seed(42)

# Initialize grid size (a 10x10 grid for this example)
grid_size <- 10

# Initial agent position
agent_position <- c(x = 5, y = 5)

# Number of time steps to simulate
num_steps <- 50

# Function to move the agent
move_agent <- function(position) {
  direction <- sample(c("up", "down", "left", "right"), 1)
  if (direction == "up" && position[2] < grid_size) {
    position[2] <- position[2] + 1
  } else if (direction == "down" && position[2] > 1) {
    position[2] <- position[2] - 1
  } else if (direction == "left" && position[1] > 1) {
    position[1] <- position[1] - 1
  } else if (direction == "right" && position[1] < grid_size) {
    position[1] <- position[1] + 1
  }
  return(position)
}

# Simulate the movement of the agent
trajectory <- matrix(NA, 
                     nrow = num_steps, 
                     ncol = 2)
trajectory[1, ] <- agent_position

for (step in 2:num_steps) {
  agent_position <- move_agent(agent_position)
  trajectory[step, ] <- agent_position
}

# Convert the trajectory matrix into a data frame for plotting
trajectory_df <- data.frame(
  step = 1:num_steps,
  x = trajectory[, 1],
  y = trajectory[, 2]
)

# Labels for pickup and dropoff
pickup_label <- trajectory_df[1, ]
dropoff_label <- trajectory_df[num_steps, ]

# Plotting the trajectory with gganimate and using transition_reveal
p <- ggplot(trajectory_df, aes(x = x, y = y, group = 1)) +
  geom_point(color = "blue", size = 3) + 
  geom_path(color = "blue", alpha = 0.5) + 
  labs(title = "Agent Movement on a 10x10 Grid", x = "X Position", y = "Y Position") +
  xlim(1, grid_size) + ylim(1, grid_size) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  transition_reveal(step) +  # Use transition_reveal to show the path over time
  ease_aes('linear') +
  # Add labels for the pickup (first point) and dropoff (last point)
  geom_text(data = pickup_label, aes(x = x, y = y, label = "Pickup"), vjust = -1, color = "red", size = 4) +
  geom_text(data = dropoff_label, aes(x = x, y = y, label = "Dropoff"), vjust = 1.5, color = "green", size = 4)

# Animate and render the plot
animate(p, nframes = num_steps, fps = 10, end_pause = 20, rewind = TRUE)

##Run it to save: 
anim_save("trajectory-animation.gif")

#| warning: false
#| message: false
#| include: false
library(gganimate)
# Plotting the trajectory with gganimate and using transition_reveal
# Add labels for pickup and dropoff locations
pickup_data <- data.frame(
  vehicle_id = 1:5,
  x = orders$pickup_x,
  y = orders$pickup_y,
  label = paste("Pickup", 1:n_customer)
)

dropoff_data <- data.frame(
  vehicle_id = 1:5,
  x = orders$dropoff_x,
  y = orders$dropoff_y,
  label = paste("Dropoff", 1:n_customer)
)

# Define the custom color palette
vehicle_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a")


# Create the plot
p <- ggplot(final, 
            aes(x = xcor, 
                y = ycor, 
                color = factor(vehicle_id),
                group = vehicle_id)) +
  geom_point(size = 3) +  # Vehicle positions
  geom_path(aes(x = xcor, 
                y = ycor), 
            color = "grey", alpha = 0.5) + 
  scale_color_discrete(name = 'Vehicle ID') +
  xlim(0, grid_size) + 
  ylim(0, grid_size) +
  theme_minimal() +
  transition_time(time) +
  ease_aes('linear')+
  # Use the same color mapping for the pickup locations
  geom_text(data = pickup_data, 
            aes(x = x, y = y, label = label, color = factor(vehicle_id)), 
            vjust = -1, size = 4) +
  # Use the same color mapping for the dropoff locations
  geom_text(data = dropoff_data, 
            aes(x = x, y = y, label = label, color = factor(vehicle_id)), 
            vjust = 1.5, size = 4)+
  theme(legend.position="bottom") +
  labs(title="Datetime: {frame_time}", 
       subtitle="The simulation of ABM for Vehicles", 
       caption="\n\nAuthor: Loccx78  \nSource: Rstudio  \n") +
  theme(
    legend.position = "bottom",           
    legend.direction = "horizontal",       
    legend.title = element_text(hjust = 0.5), 
    legend.text = element_text(size = 10), 
    plot.title = element_text(size = rel(1.5), family = "sans", face = "bold"),
    plot.subtitle = element_text(color = "#5e5855"),
    plot.caption = element_text(color = "#867e7a"),
    plot.tag = element_text(hjust = 0.5, color = "#5e5855"),
    plot.tag.position = c(0.5, 0.16)
  ) 


animated_map <- animate(p,
                        nframes = n_ticks, 
                        duration = 10, 
                        fps = 10, 
                        rewind = TRUE)

anim_save("ABM.gif")
