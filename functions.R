calc_total_time <- function(open, close) {
  open <- as.POSIXct(open, format = "%H:%M", tz = "UTC")
  close <- as.POSIXct(close, format = "%H:%M", tz = "UTC")
  
  # Calculate the difference in minutes
  difference <- difftime(close, open, units = "mins")
  
  return(difference)
}

calc_simple_arrival_times <- function(rate, minutes){
  rate <- rate/60 # convert hours to minutes
  n <- ceiling(rate*minutes) # max number of customers
  W_sample <- rexp(n, rate)
  T_sample <- numeric(n)
  
  for(i in 1:n) {
    T_sample[i] <- sum(W_sample[1:i])
  }
  
  arrival_times <- T_sample[T_sample <= minutes]
  
  return(arrival_times)
}

calc_simple_service_times <- function(arrivals, chefs) {
  # Ensure rate is per unit time
  minute_rate = (3*chefs) / 60
  services = rexp(length(arrivals), rate = minute_rate)
  return(services) # in minutes
}

calc_realistic_arrival_times <- function(
    open, close,
    lunch_peak_start,lunch_peak_end,
    dinner_peak_start, dinner_peak_end,
    lambda_down, lambda_peak
) {
  open <- as.POSIXct(open, format = "%H:%M", tz = "UTC")
  close <- as.POSIXct(close, format = "%H:%M", tz = "UTC")
  lunch_peak_start <- as.POSIXct(lunch_peak_start, format = "%H:%M", tz = "UTC")
  lunch_peak_end <- as.POSIXct(lunch_peak_end, format = "%H:%M", tz = "UTC")
  dinner_peak_start <- as.POSIXct(dinner_peak_start, format = "%H:%M", tz = "UTC")
  dinner_peak_end <- as.POSIXct(dinner_peak_end, format = "%H:%M", tz = "UTC")
  # Convert arrival rates to per minute
  rate_down <- lambda_down / 60
  rate_peak <- lambda_peak / 60
  
  # Convert peak times to minutes from opening
  lunch_peak_start_min <- difftime(lunch_peak_start, open, units = "mins")
  lunch_peak_end_min <- difftime(lunch_peak_end, open, units = "mins")
  
  dinner_peak_start_min <- difftime(dinner_peak_start, open, units = "mins")
  dinner_peak_end_min <- difftime(dinner_peak_end, open, units = "mins")
  
  total_time <- difftime(close, open, units = "mins")
  
  # Initialize list to store arrival times
  arrival_times <- numeric()
  current_time <- 0  # Start at 0 minutes (opening time)
  
  # Generate arrival times
  while (current_time < total_time) {
    # Determine the arrival rate based on current time
    if ((current_time >= lunch_peak_start_min && current_time < lunch_peak_end_min) ||
        (current_time >= dinner_peak_start_min && current_time < dinner_peak_end_min)) {
      arrival_rate <- rate_peak  # Peak time rate
    } else {
      arrival_rate <- rate_down  # Downtime rate
    }
    
    # Generate the next interarrival time from the exponential distribution
    next_arrival <- rexp(1, arrival_rate)
    
    # Update the current time
    current_time <- current_time + next_arrival
    
    # If within the operating hours, add the arrival time to the list
    if (current_time < total_time) {
      arrival_times <- c(arrival_times, current_time)
    }
  }
  
  # Return arrival_times
  return(arrival_times)
}

calc_realistic_service_times <- function(arrivals, chefs) {
  # Ensure rate is per unit time
  minute_rate = (log(chefs + 1)) / 60
  services = rexp(length(arrivals), rate = minute_rate) + 45
  return(services) # in minutes
}

restaurant_sim <- function(arrivals, services, tables, minutes) {
  # calculate service times
  service_times = services
  
  # set up tracking
  arrival_times_temp <- arrivals

  queue_size_history <- numeric(minutes)
  
  # number of tables occupied each minute
  occupied_tables_history <- rep(0, minutes)
  
  # timer to track remaining waiting time for each table in the restaurant
  # each element is one table in the restaurant
  # -1 means empty
  # otherwise, number of remaining service minutes
  tables_timer <- rep(-1, tables)
  
  # the amount of minutes each customer of that day waited
  waiting_times <- numeric(0)
  
  # the arrival_times indices of the people currently in line
  # in order to know how long their eventual service time will be
  queue <- numeric(0)
  
  # an internal counter separate from the time
  customers_entered <- 0
  
  # iterate through the day
  for (i in 1:minutes) {
    occupied_tables_history[i+1] = occupied_tables_history[i]
    
    # update the waiting timer for all occupied tables
    tables_timer[tables_timer > 0] <- tables_timer[tables_timer > 0] - 1
    # update the number of available tables in the next minute
    # based on the number of tables who have finished timers
    occupied_tables_history[i+1] = occupied_tables_history[i+1] - sum(tables_timer == 0)
    # mark the finished tables as available tables for the next minute
    tables_timer[tables_timer == 0] <- tables_timer[tables_timer == 0] - 1
    
    # has the next customer arrived?
    if(length(arrival_times_temp) > 0){
      if(arrival_times_temp[1] < i) {
        # if so, add them to the back of the queue
        queue = c(queue, as.integer(customers_entered+1)) # add 1 for 1-indexing
        # remove the 1st element of arrival_times
        arrival_times_temp = arrival_times_temp[-1]
        # start the waiting timer for this customer by appending 0
        waiting_times = c(waiting_times, 0)
        
        customers_entered = customers_entered + 1
      }
    }
    # are any tables currently open and there is a person in line?
    if(occupied_tables_history[i+1] < tables & length(queue) > 0) {
      # if so, then seat the first person in line
      # at the first available table
      for (j in 1:tables) {
        if(tables_timer[j] == -1) {
          # queue[1] has the customer index of the first person in line
          tables_timer[j] = round(service_times[queue[1]])
          break
        }
      }
      # the next minute there will be one more occupied table
      occupied_tables_history[i+1] = occupied_tables_history[i+1] + 1
      # remove the first person in the queue
      queue = queue[-1]
    }
    # update the waiting time for each person in the queue
    for (customer_index in queue) {
      waiting_times[customer_index] = waiting_times[customer_index] + 1
    }
    # keep track of how long the line is at each minute
    queue_size_history[i] = length(queue)
  }
  
  occupied_tables_history <- occupied_tables_history[-1]
  
  return(list(
    waiting_times = waiting_times, 
    queue_size_history = queue_size_history, 
    occupied_tables_history = occupied_tables_history)
  )
}

summarize_resturant_sim_s2 <- function(waiting_times, queue_size_history, 
                                    occupied_tables_history, chefs, 
                                    minutes, tables) {
  # calculate outputs (the things we actually care about) from the simulation
  
  # average waiting time across all customers
  avg_waiting_time <- mean(waiting_times)
  # number of customers who waited >30 minutes (and made us less money)
  long_waits <- length(waiting_times[waiting_times > 30])
  # average queue length throughout the day
  avg_queue_length <- mean(queue_size_history)
  # maximum queue length that day
  max_queue_length <- max(queue_size_history)
  # average table occupancy in the restaurant
  avg_tables_occupied <- mean(occupied_tables_history)
  # number of customers
  num_customers <- length(waiting_times)
  #profit
  profit <- 50*num_customers - (40/60)*chefs*as.numeric(minutes) - 1000*tables - 25*long_waits
  
  # return all of it, as a data frame with one row
  sim_output <- data.frame(
    total_customers = num_customers,
    profit = profit,
    num_chefs = chefs,
    num_tables = tables,
    avg_waiting_time = avg_waiting_time,
    long_waits = long_waits,
    avg_queue_length = avg_queue_length,
    max_queue_length = max_queue_length,
    avg_tables_occupied = avg_tables_occupied
  )
  
  return(sim_output)
}

summarize_resturant_sim_s3 <- function(waiting_times, queue_size_history, 
                                       occupied_tables_history, chefs, 
                                       minutes, tables) {
  # calculate outputs (the things we actually care about) from the simulation
  
  # average waiting time across all customers
  avg_waiting_time <- mean(waiting_times)
  # number of customers who waited >30 minutes (and made us less money)
  long_waits <- length(waiting_times[waiting_times > 30])
  # average queue length throughout the day
  avg_queue_length <- mean(queue_size_history)
  # maximum queue length that day
  max_queue_length <- max(queue_size_history)
  # average table occupancy in the restaurant
  avg_tables_occupied <- mean(occupied_tables_history)
  # number of customers
  num_customers <- length(waiting_times)
  #profit
  profit <- 50*num_customers - (40/60)*chefs*as.numeric(minutes) - 80*tables - 25*long_waits
  
  # return all of it, as a data frame with one row
  sim_output <- data.frame(
    total_customers = num_customers,
    profit = profit,
    num_chefs = chefs,
    num_tables = tables,
    avg_waiting_time = avg_waiting_time,
    long_waits = long_waits,
    avg_queue_length = avg_queue_length,
    max_queue_length = max_queue_length,
    avg_tables_occupied = avg_tables_occupied
  )
  
  return(sim_output)
}
  
  
  
  