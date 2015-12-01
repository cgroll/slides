% Asset management:
% Extending Markowitz
% Christian Groll

# Notes

### Missing content
- adapting for estimation risk (bootstrap)
- multi-period investment
		- multi-period return distribution
		- constant weights not possible
- taxes
- integer optimization

### Empirical analysis to be done

- check return asymmetry (VaR required?)
- multi-period risk target does not translate to unique single period
  risk target
- evaluate multi-period risk for given single period strategy
	- how much do weights change over time?
- adapt for estimation risk: bootstrapping

# Portfolio selection

###

Two tasks:

1. with given weights, **estimate** associated **portfolio return
   distribution**

. . .

2. given different portfolio return distributions, **find optimal**
   return distribution

. . .

1b. also take **confidence** in estimated distributions into account
(**estimation error**)


# Risk aversion and optimality

### 

- coin game

- concave utility: induces
	- increasing $\mu$ increases utility
	- increasing $\sigma$ decreases utility

### $\bf{\mu / \sigma}$ dogma

First two moments are sufficient to derive the utility of any given
return distribution.


### $\bf{\mu / \sigma}$ deficiencies

$\sigma$ inadequate to capture all kinds of risk:

- $\sigma$ is a symmetric measure
- reflection of asymmetric distributions at mean value does not change
  utility

### Other metrics

- VaR
- benefits: concentration on downside risk

### VaR deficiencies

- returns beyond VaR are not taken into account

### General remarks

- single value will always reduce information (equal for infinitely
  many distributions)
- people simple can not rank all possible combinations of return
  distributions (indifference)

### Additional implicit assumptions

Two **equal return** distributions are always **valued equally**

. . .

- portfolio is only one **component of overall investment** (rest with
  home bias)

. . .

- financial investments only one **component of overall wealth** (home
  bias is irrational: losing job in Germany during recession)

. . .

$\Rightarrow$ asset pricing

### Additional implicit assumptions

- focusing on distribution at single point in future: **no explicit
  path dependence**

# Single period optimization

## Portfolio return

- portfolio return calculation (with **discrete returns**):

\begin{equation*}
r_{P}=w_{1}r_{1}+...+w_{n}r_{n}
\end{equation*}

. . .

- future **$r_{P}$ unknown** $\Rightarrow$ estimate distribution

### Univariate estimation

- changing weights requires reestimation of model

### Multivariate estimation

- changing weights does not require reestimation of model

### With $\bf{\mu / \sigma}$ optimality

- **vastly simplifies** if optimality is just a function of portfolio
  return moments

. . .

\begin{align*}
\mathbb{U}(r_{P}) &= g\left(\mathbb{E}[r_{P}], \mathbb{V}(r_{P})\right)\\
&=g(\mu_{P},\sigma_{P}^{2})
\end{align*}

### Portfolio moments

- **expected portfolio return**:

\begin{align*}
\mu_{P}&=\mathbb{E}[r_{P}]\\
&=\mathbb{E}[w_{1}r_{1}+...+w_{n}r_{n}]\\
&=w_{1}\mathbb{E}[r_{1}]+...+w_{n}\mathbb{E}[r_{n}]\\
&=\bf{w}'\bf{\mu}
\end{align*}

###

- portfolio **variance**:

\begin{align*}
\sigma_{P}^{2}&=\mathbb{V}(r_{P})\\
&=\mathbb{V}(w_{1}r_{1}+...+w_{n}r_{n})\\
&=\sum_{i=1}^{n}\mathbb{V}(w_{i}r_{i})+
	2 \sum_{i=1}^{n}\sum_{j<i}\text{Cov}(w_{i}r_{i},w_{j}r_{j})\\
&=\sum_{i=1}^{n}w_{i}^{2}\mathbb{V}(r_{i})+
	2 \sum_{i=1}^{n}\sum_{j<i}w_{i}w_{j}\text{Cov}(r_{i},r_{j})\\
&= \bf{w}'\Sigma\bf{w}
\end{align*}

### Estimation

- simplified multivariate estimation
- estimate $\bf{\mu},\bf{\Sigma}$ only
- no distribution required for $r_{1},...,r_{n}$

### 

Popular optimality criterions:

. . .

- for given $\mu_{P}$ **minimize $\sigma_{P}$**

. . .

- for given $\sigma_{P}$ **maximize $\mu_{P}$**

. . .

- maximize **Sharpe ratio**:

$$SR_{P}=\frac{\mu_{P}}{\sigma_{P}}$$

### Optimization

- analytical solution without short-selling constraints
- quadratic objective, linear constraints

### Criticism

- return distribution: sum of non-symmetric discrete asset returns 

- $\sigma$ to capture risk of non-symmetric portfolio returns

### Employing VaR

- portfolio return distribution required
	- univariate: multiple models to be estimated
	- multivariate: simulation

### Logarithmic returns

- portfolio return is not a linear function anymore:

\begin{align*}
\mu_{P}&=\mathbb{E}[r_{P}]\\
&=\mathbb{E}[g(r_{1}, \ldots, r_{n})]\\
&=?
\end{align*}

### desired

- with given VaR, optimize $\mu_{P}$
- not possible to hold VaR value fixed

- maybe: translate VaR to $\sigma_{P}$, hold $\sigma_{P}$ fixed 

# Multi-period optimization

###

Assumption: optimality criterion given in each period

. . .

$\Rightarrow$ sequence of single period optimizations

### Turnover

- $\bf{\mu}$ and $\bf{\Sigma}$ change over time
- optimal portfolio changes over time (portfolio rebalancing)
- trading costs: reduce / minimize turnover
- tradeoff:
	- benefits of rebalancing
	- costs of rebalancing
- extreme example: 
	- changing distribution only in single period
	- rebalancing twice might not be worthwhile

### TODO: plain Markowitz real world example

- plain Markowitz example: 
	- simple estimation -> show changes of moments over time
	- show changes of optimal portfolio over time
	- heuristics to reduce turnover
	- comparison to buy and hold / equally weighted

- benefits:
	- reduced drawdown
	- less volatility

### Crucial tradeoff

- **long horizon** more meaningful when determining **optimality**
- **short horizon** simplifies **estimation** of return distributions

### Derive single period optimality

- only long-term optimality given
- translation to single term optimality not unique

### TODO: MSCI world + EONIA

- show two extreme cases fulfilling multi-period optimality:
	- fixed weights
	- fixed volatility
- which is better for taxes?

- top-down approach: given long-term optimality, derive short-term
  optimality 

### Different approach

- bottom-up approach: derive long-term properties for given short-term
  portfolio 

- without changing moments
- with changing moments

### TODO: changing weights for buy-and-hold strategy



# Basics

## Utility theory

- Assumption A1: the utility of any given payoff distribution can be
  derived by knowing the first two moments

- Assumption A2: the utility of two equal payoff distributions is
  equal


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

### Estimation

- best estimator should be **identifiable**
	- exponentially weighted sample moments
	- copula-GARCH
	- multivariate normal distribution
- best estimator should make use of asset pricing theory
	- factor model
	- asset pricing model

# Alternative approach

- translate long-term VaR in single period $\sigma$
- estimate single period with factor model
- find optimal $\mu$ given $\sigma$ with brute force optimization 
- find optimal single period portfolio using bootstrapping
- reduce turnover?!
- is highly fluctuating path better for tax optimization?!



