% Asset management:
% Extending Markowitz
% Christian Groll

# Contents

### Manual list of contents
- single period investment
	- optimal portfolio: risk target
		- volatility
		- VaR
	- estimation risk
		- possible estimators
		- adapting for estimation risk
- multi-period investment
	- risk target
		- multiple ways to achieve required target
			- single asset joined with fixed income security
		- multi-period return distribution
			- constant weights not possible
- multi-period investment with time-varying asset moments
	- turnover

# Empirical analysis

- check return asymmetry (VaR required?)
- multi-period risk target does not translate to unique single period
  risk target
- evaluate multi-period risk for given single period strategy
	- how much do weights change over time?
- adapt for estimation risk: bootstrapping


# Single period investment

### Markowitz

For any **given** expected portfolio return **$\mu^{*}$**,
**minimize** portfolio **variance**:

\begin{align*}
\underset{\xi}{\min}\, \quad\sigma_{P}^{2}&=\xi'\Sigma\xi \\
\text{ subject to } \quad\mu_{P}&=\xi'\mu =\mu^{*},\\
\xi'\bf{1} &=1
\end{align*}

. . .

$\Rightarrow$ assets with **low volatility** and **low correlations**
to other assets are preferred

### Portfolio optimality

- **assumption:** optimal portfolio can be described through
  $\mu-\sigma$ tradeoff

. . .

Common strategies:

. . .

- for given $\mu_{P}$ **minimize $\sigma_{P}$**

. . .

- for given $\sigma_{P}$ **maximize $\mu_{P}$**

. . .

- maximize **Sharpe ratio**:

$$SR_{P}=\frac{\mu_{P}}{\sigma_{P}}$$

###

- other utility functions could require different metrics

. . .

- **VaR**

### Why *VaR*

- more meaningful to private investors

- no symmetry for discrete returns

- focusing on downside risk

- coinciding with financial regulation

###

- *VaR* portfolio differ from $\mu-\sigma$ portfolio?
- is *VaR* proportional to $\sigma$?

*CHECK*: asymmetry (on weekly data)


### Markowitz

- assumption: volatility is good risk measure
- extension: different risk measure
	- VaR: is it proportional
	- asset pricing: how correlated is risk measure to risk factors?
     (to German stock market?)
	- multi-period risk target: how can it be translated to single
     period optimization?

### Skewness

- linear portfolio formula only holds for discrete returns
- discrete returns not symmetric
- how asymmetric are returns?
	- single unconditional Markowitz setting
	- bootstrap portfolio returns
	- conditional model for given point in time: copula-GARCH

### Assume Markowitz with sigma risk target

- how does mu-sigma area behave?

### Target

- there usually exists a difference between the time horizon of the
  target and the data frequency:
	- the portfolio target might be defined on a yearly basis
	- asset moments are estimated on a weekly / monthly basis in order
     to have sufficient data

### Aligning long and short term optimization targets

- how does a yearly target translate into a weekly target?

Example: MSCI World and fixed income security
- multiple possibilities: 
	- one constant weight that creates the required long term risk
     target
	- smoothing risk over time: keep short term risk constant by
     constant rebalancing

### 

Calculating long term risk:
- due to price changes weights change over time: fixed weights
  strategy not possible without rebalancing
- univariate GARCH modeling for portfolio returns not correct
- correct simulation algorithm:
	- multivariate modeling and simulation
	- simulate weight changes
- problematic:
	- square-root-of-time scaling with given conditional moments
	- example: high-volatility sigmas and square-root-of-time scaling
     will pretend that sigmas will stay high for the whole period

### Research questions
- how do univariate and multivariate yearly return distributions
  change?
- how much do weights change over time?
	- simulated data (copula GARCH?!)
	- real data

### Portfolio return distributions

- standard deviation vs VaR as risk measure:
- how do weekly return distributions look like?
	- with discrete returns (that are not symmetric)

# Multi-period investment
