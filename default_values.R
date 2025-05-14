################################################################################
# Set Default values
params <- list(
  # Default number of fishers for each type of fishery (A, B, C, D)
  Number_fishers = list(
    A = 40,  # Number of fishers for Type A
    B = 127, # Number of fishers for Type B
    C = 130, # Number of fishers for Type C
    D = 310  # Number of fishers for Type D
  ),
  # Quotas for herring (in %)
  Quotas_herring = list(
    QhA = 72.8, # Herring quota for Type A
    QhB = 21.3, # Herring quota for Type B
    QhC = 4.2,  # Herring quota for Type C
    QhD = 1.7   # Herring quota for Type D
  ),
  # Quotas for cod (in %)
  Quotas_cod = list(
    QcA = 59.8, # Cod quota for Type A
    QcB = 25.8, # Cod quota for Type B
    QcC = 10.6, # Cod quota for Type C
    QcD = 3.8   # Cod quota for Type D
  ),
  # Quotas for other fish (in %)
  Quotas_other = list(
    QoA = 45, # Other fish quota for Type A
    QoB = 45, # Other fish quota for Type B
    QoC = 7,  # Other fish quota for Type C
    QoD = 3   # Other fish quota for Type D
  ),
  # Costs related to each type of fishery
  Costs = list(
    CfA = 91800, # Fixed annual cost for Type A fishery 
    CfB = 44600, # Fixed annual cost for Type B fishery
    CfC = 14100, # Fixed annual cost for Type C fishery
    CfD = 3400,  # Fixed annual cost for Type D fishery
    CA = 1.3,   # Variable cost per kg for Type A fishery
    CB = 0.8,   # Variable cost per kg for Type B fishery
    CC = 2.2,   # Variable cost per kg for Type C fishery
    CD = 6.3    # Variable cost per kg for Type D fishery
  ),
  
  # Fish prices (per kg) for each type of fishery and fish type
  Prices = list(
    A = list(h = 1.95, c = 2.48, o = 1.31), # Prices for herring (h), cod (c), and other fish (o) for Type A
    B = list(h = 3.78, c = 8.27, o = 4.91), # Prices for Type B
    C = list(h = 4.56, c = 10.95, o = 14.36), # Prices for Type C
    D = list(h = 4.56, c = 10.95, o = 15.50)  # Prices for Type D
  ),
  # Monthly target income (in €)
  Monthly_Target_Income = 1944.23, # Desired monthly income per fishery (after tax)
  # Stock development scenarios (in tonnes)
  Stock_development = list(
    h_positive = 50000,  # Positive stock development for herring
    h_stagnant =  6000,   # Stagnant stock development for herring 6000/ 2022: 638
    c_positive = 6000,   # Positive stock development for cod
    c_stagnant = 700,    # Stagnant stock development for cod 700 / 2022: 136
    other = 4000         # Stock of other fish
  ),
  # Depreciation costs for each type of fishery
  Depreciation = list(
    A = 0, # 32660, # Annual depreciation for Type A
    B = 0, # 5827,  # Annual depreciation for Type B
    C = 0, # 4272,  # Annual depreciation for Type C
    D = 0 # 1555   # Annual depreciation for Type D
  ),
  # Possible (German) landing quantity (in tons/year)
  Landing_share = list(
    Herring = 0.52 / 2, # Share of herring stock landed
    Cod = 0.26          # Share of cod stock landed
  )
)

