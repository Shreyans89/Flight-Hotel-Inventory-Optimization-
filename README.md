# Flight/Hotel-Inventory Optimization
 Set of files to implement finite horizon MPC on Airline/Hospitality inventory
 
 Files-
 RunSim_Parallel.R- Run optimal pricing simulations from a random initial inventory state,includes parallelism to run batches of simulations
 create_problem_data.R- create problem weight and bias matrices
 Optimal_pricer1.R - creates CVX compliant convex problem and solves for price trajectory
 simulate_pricecontrol.R -simulates price/inventory trajectory with poisson arrivals,calling the Optimal_pricer each period
 Inventory_Graphing.R - create simulation visualization  

 Version 1.0 (15/11/2020)
