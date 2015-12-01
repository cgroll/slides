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

# Introduction

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

# Optimization

## Single period optimization

### Portfolio return

- portfolio return calculation (with **discrete returns**):

\begin{equation*}
r_{P}=w_{1}r_{1}+...+w_{n}r_{n}
\end{equation*}

. . .

- future **$r_{P}$ unknown** $\Rightarrow$ estimate distribution

### With $\bf{\mu / \sigma}$ optimality

- **vastly simplifies** optimal portfolio selection

. . .

- utility as **function of** portfolio return **moments**

\begin{align*}
\mathbb{U}(r_{P}) &= g\left(\mathbb{E}[r_{P}], \mathbb{V}(r_{P})\right)\\
&=g(\mu_{P},\sigma_{P}^{2})
\end{align*}

. . .

$\Rightarrow$ portfolio moments easy to derive from asset moments

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

### 

Popular optimality criterions for given $\bf{\mu}$ and $\bf{\Sigma}$ 

. . .

- for given $\mu_{P}$ **minimize $\sigma_{P}$** *(1)*

. . .

- for given $\sigma_{P}$ **maximize $\mu_{P}$** *(2)*

. . .

- **globally minimize $\sigma_{P}$** *(3)*

. . .

- maximize **Sharpe ratio** *(4)*

$$SR_{P}=\frac{\mu_{P}}{\sigma_{P}}$$

### Optimization challenges

Without short-selling constraints:

- analytical solution for 1, 2 and 3 (4)?

### Constraints

- single asset constraints (**no short-selling**)

. . .

- group constraints (**asset class**, asset region)

### Optimization challenges

With constraints, we have to rely on numerical optimization:

1. quadratic objective, linear constraints

1. linear objective, quadratic constraints

1. ?

1. ?

### Unclear yet

How are μ and σ derived? $\Rightarrow$ see estimation part

### Criticism

- **asymmetric** return distribution of **$r_{P}$**: sum of asymmetric
  discrete asset returns

. . .

$\Rightarrow$ **symmetric measure $\sigma_{P}$** to quantify risk of
asymmetric portfolio returns

### First alternative: logarithmic returns

- logarithmic returns are almost symmetric

. . .

- but: portfolio return is not a linear function anymore

\begin{align*}
\mu_{P}&=\mathbb{E}[r_{P}]\\
&=\mathbb{E}[g(r_{1}, \ldots, r_{n})]\\
&=?
\end{align*}


### Second alternative: employing VaR

- **distribution** of portfolio return $r_{P}$ **required**


### Why *VaR*

- more meaningful to private investors

- no symmetry for discrete returns

- focusing on downside risk

- coinciding with financial regulation


### desired

- with given VaR, optimize $\mu_{P}$
- not possible to hold VaR value fixed

- maybe: translate VaR to $\sigma_{P}$, hold $\sigma_{P}$ fixed 

## Repeated single period optimization

###

Assumption: **optimality** criterion given **in each period**

. . .

$\Rightarrow$ **sequence** of single period optimizations

### New challenge: turnover

- $\bf{\mu}$ and $\bf{\Sigma}$ change over time

. . .

$\Rightarrow$ optimal portfolio changes over time (**portfolio
rebalancing**)

. . .

$\Rightarrow$ **trading costs** occur 

### Minimize turnover

- tradeoff: rebalancing **costs vs benefits**

. . .

- **benefits** depend on all **future periods**

. . .

- extreme example: 
	- changing distribution only in single period
	- rebalancing twice might not be worthwhile

### Turnover incentives

**incentives** on turnover become **asymmetric** with **all-in-fee**:

. . .

$\Rightarrow$ customers prefer optimal portfolio at each time

### TODO: plain Markowitz real world example

- plain Markowitz example: 
	- simple estimation -> show changes of moments over time
	- show changes of optimal portfolio over time
	- heuristics to reduce turnover
	- comparison to buy and hold / equally weighted

- benefits:
	- reduced drawdown
	- less volatility

## Multi-period optimization

### Crucial tradeoff

- **long horizon** more meaningful when determining **optimality**
- **short horizon** simplifies **estimation** of return distributions

### Derive single period optimality

- only long-term optimality given
- translation to single term optimality not unique

### TODO: MSCI world + EONIA

- show two extreme cases fulfilling multi-period optimality:
	- fixed weights: one constant weight that creates the required long
     term risk target
	- fixed volatility: smoothing risk over time: keep short term risk
     constant by constant rebalancing

- which is better for 
	- taxes
	- customer
	- company

### Top-down approach

given long-term optimality, derive short-term optimality

### Bottom-up approach

for given short-term portfolio derive long-term properties

### Univariate 

- in reality not possible: weights change
- TODO: how much do weights change
- adapt for turnover?

### Multivariate

- changing weights can be taken into account
- without changing distributions: can be done
- with changing distributions: how do multivariate distributions
  change? 

### Aligning long and short term optimization targets

- how does a yearly target translate into a weekly target?

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


# Estimation

###

When should estimation be tackled?!

- in plain Markowitz example with moments only?
- in single period VaR example?
- in part on bootstrapping?

### Univariate estimation

- changing weights requires reestimation of model

### Multivariate estimation

- changing weights does not require reestimation of model

### Plain Markowitz estimation:

- simplified multivariate estimation
- estimate $\bf{\mu},\bf{\Sigma}$ only
- no distribution required for $r_{1},...,r_{n}$

### Bootstrap 

When should bootstrap be tackled?!

# Markowitz details

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


### Assume Markowitz with sigma risk target

- how does mu-sigma area behave?

###

- *VaR* portfolio differ from $\mu-\sigma$ portfolio?
- is *VaR* proportional to $\sigma$?

*CHECK*: asymmetry (on weekly data)


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

###

- translate long-term VaR in single period $\sigma$
- estimate single period with factor model
- find optimal $\mu$ given $\sigma$ with brute force optimization 
- find optimal single period portfolio using bootstrapping
- reduce turnover?!
- is highly fluctuating path better for tax optimization?!
- different optimization horizon? Daily?
- correspondence to whitepaper


