% JFinMetriX: AssetMgmt
% Christian Groll

# Type hierarchy

### Type hierarchy for return models

`RetModel`:

- `MomentsModel`
- `DistributionsModel`
- `DynamicsModel`

### `MomentsModel`

Only moments of returns:

- $\mu$s
- $\Sigma$s
- date

### `DistributionsModel`

Complete conditional distribution:

- model for conditional distribution
- date

Functionality:

- simulate discrete returns (model could be defined on log-returns
  also) 

### `DynamicsModel`

Complete distribution with dynamics:

- model for conditional distribution
- model for dynamics
- date

$\Rightarrow$ evolution over multiple periods can be simulated 

### `RetModel` input

In order to determine a `RetModel` for a given period the following
inputs could be required:

- past data
- window size (how much past data)
- future data (estimating GARCH on complete sample)
- data weighting \lambda
- factor data (Fama / French)
- additional tuning parameters?

### Desired portfolio

- there is a single best portfolio that one ideally wants to hold in
  each period (independent of the current portfolio weights)
- the rule to derive a desired portfolio for a given period is called
  investment strategy
- it depends on different input:
	- `RetModel`
	- past data (for non-parametric bootstrapping)

### Optimal portfolio

- the optimal portfolio should be chosen with regards to transaction
  costs
- optimality has to solve the tradeoff between costs and benefits of
  portfolio re-balancing
- the rule to determine optimal portfolios could depend on different
  inputs:
	- `RetModel` (to estimate future benefits of re-balancing)
	- desired portfolio (without transaction costs)
	- current weights (to calculate transaction costs)
	- "irrational input" (tuning parameters: trading every two weeks)

### Desired vs optimal portfolio

The difference in desired and optimal portfolio is the additional
current `weights` input for optimal portfolios.

### Backtesting

`Investments`:

- for each strategy we get a history of portfolio weights
	- analyze turnover
	- analyze diversification
- for each history of portfolio weights we get series of portfolio
  returns


$\Rightarrow$ analyze portfolio returns

### Return performance statistics / metrics

- $\mu$
- $\sigma$
- VaR
- min / max
- annualized quantities
- drawdown: amount, duration
- net price evolution (with trading costs)
