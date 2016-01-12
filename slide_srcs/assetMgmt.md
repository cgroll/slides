% Data driven 
% asset management

<!--
# Notes

### Introduction

- Asset management: 
	- into focus through scalable capital
- overview of some challenges that you face in reality
- thereby I want to lay out problems that arise if you look at these
  challenges with some mathematical rigor
- it is important to know about approximations and assumptions
  involved
- this then should set the stage for an empirical application
- where we need to make compromises
- which however I decided to not treat today
- both: 
	- to keep talk at least somewhat short
	- results are only preliminary
- some slides: also for different audience (webpage developers)
- some might notice: choice of color
- apologies upfront, if some slides appear somewhat ridiculous for
  your level of sophistication
- but I think it is an important part to make research understandable
  for a broader audience
- especially in this context: still might need to think of how we
  communicate the value added of our product 
- so some slides also try to cope with that issue, and you are very
  much invited to judge on the broader pedagogical value of these
  slides and the presentation in general

### Topics Tech Lunch

- Introduction (+ data pics: prices, returns)
- Risk aversion and utility ($\sigma$ vs VaR, risk-return tradeoff)
- Diversification (attainable portfolios, Markowitz, strategies -
  given estimated mus and sigmas)
- multi-period: varying moments -> turnover comes into play

### Missing content

- adapting for estimation risk (bootstrap / resampling methods)
- multi-period investment
	- multi-period return distribution
	- constant weights not possible
- taxes
- why Markowitz without risk-free asset?
	- no short selling of risk-free asset allowed
- integer optimization
- trading time lag: changing prices 
	- limit orders
	- feedback loops

### Empirical analysis to be done

- check return asymmetry (VaR required?)
- multi-period risk target does not translate to unique single period
  risk target
- evaluate multi-period risk for given single period strategy
	- how much do weights change over time?
- adapt for estimation risk: bootstrapping (already should smooth
  turnover)

-->


# Introduction

### Two tasks

. . .

1. with given weights, **estimate** associated **portfolio return
   distribution**

<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/weights2distribution.svg"
alt="Number of observations" width="1000px"/>
</p>


###


2. given different portfolio return distributions, **find optimal**
   return distribution

<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/compareDistributions.svg"
alt="Number of observations" width="1000px"/>
</p>

<!--
### Third task

3. also take **confidence** in estimated distributions into account
   (**estimation error**)
-->

# Risk aversion and utility

<!--
### Risk aversion

- coin game
- empirical studies



### Setting

<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/concave_utility.svg"
alt="Number of observations" width="1000px"/>
</p>

### Utility function

- hard to **define**
- hard to **evaluate**

$\Rightarrow$ simpler rules (implied by concave utility)

-->

</section>
<section data-transition="slide-in none-out">
<h2>Increasing $\bf{\mu}$</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/muIncrease.svg"
alt="Number of observations" width="1000px"/>
</p>
</section>

<section data-transition="none-in slide-out">
<h2>Increasing $\bf{\mu}$</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/muIncrease2.svg"
alt="Number of observations" width="1000px"/>
</p>
</section>


<section data-transition="slide-in none-out">
<h2>Increasing $\bf{\sigma}$</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/sigmaIncrease.svg"
alt="Number of observations" width="1000px"/>
</p>
</section>

<section data-transition="none-in slide-out">
<h2>Increasing $\bf{\sigma}$ </h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/sigmaIncrease2.svg"
alt="Number of observations" width="1000px"/>
</p>



### $\bf{\mu / \sigma}$ dogma

> First two **moments** are **sufficient** to derive the **utility**
of any given return distribution.


### $\bf{\mu / \sigma}$ deficiencies

$\sigma$ **inadequate** to capture all kinds of risk:

. . .

- as **symmetric measure** unaffected by reflection at mean value

<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/sigmaDeficiency.svg"
alt="Number of observations" width="1000px"/>
</p>


### Alternative measure: VaR

- **VaR**: value exceeded with given probability $\bf{\alpha}$
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/var.svg"
alt="Number of observations" width="300px"/>
</p>


### Benefits

- focusing on **downside risk**

$\Rightarrow$ more appropriate for asymmetric return distributions

. . .

- more **meaningful**: defining risk aversion in terms of VaR more
  intuitive to private investors (?)

. . .

- aligned with financial regulation


### Nobody's perfect

. . .

- values beyond VaR are not taken into account
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/varDeficiency.svg"
alt="Number of observations" width="1000px"/>
</p>

. . .

$\Rightarrow$ reduction to single value causes **loss of information**

### But

Does single ranking of all possible return distributions exist?

. . .

$\Rightarrow$ **indifference**

. . .

$\Rightarrow$ **irrationality**

. . .

> **A** over **B**, **B** over **C**

. . .

But:

> **C** over **A**


### Additional optimality criteria


<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/concave_utility.svg"
alt="Number of observations" width="1000px"/>
</p>

### Additional optimality criteria

Two **equal return** distributions are not always **valued equally**

. . .

- portfolio is only one **component of overall investment**:
  **diversification** preferred

. . .

- financial investments only one **component of overall wealth**:
  avoid **home bias**

. . .

$\Rightarrow$ asset pricing

### Additional optimality criteria

- focusing on distribution at single point in future insufficient:
  **path dependence**

. . .

<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/noPathDependence.svg"
alt="Number of observations" width="1000px"/>
</p>


###

**Example**: portfolio of MSCI World and EONIA

. . .

- keeping **weights fixed**: portfolio subject to volatility risk

. . .

- keeping **volatility fixed**: depending on estimated market
  volatility **adapt weights**

# Single period

### Portfolio return

- portfolio return calculation (with **discrete returns**):

\begin{equation*}
r_{P}=w_{1}r_{1}+...+w_{N}r_{N}
\end{equation*}

- future **$r_{P}$ unknown** $\Rightarrow$ estimate distribution


</section>

<section data-transition="slide-in none-out">
<h2>Univariate</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/univar_model0.svg"
alt="Number of observations" width="1000px"/>
</p>
</section>

<section data-transition="none">
<h2>Univariate</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/univar_model1.svg"
alt="Number of observations" width="1000px"/>
</p>
</section>

<section data-transition="none">
<h2>Univariate</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/univar_model2.svg"
alt="Number of observations" width="1000px"/>
</p>
</section>

<section data-transition="none">
<h2>Univariate</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/univar_model.svg"
alt="Number of observations" width="1000px"/>
</p>
</section>

<section data-transition="slide-in none-out">
<h2>Multivariate</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/multivar_model1.svg"
alt="Number of observations" width="1000px"/>
</p>
</section>

<section data-transition="none">
<h2>Multivariate</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/multivar_model2.svg"
alt="Number of observations" width="1000px"/>
</p>
</section>

<section data-transition="none">
<h2>Multivariate</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/multivar_model.svg"
alt="Number of observations" width="1000px"/>
</p>


### With $\bf{\mu / \sigma}$ optimality

- utility as **function of** portfolio return **moments**

\begin{align*}
\mathbb{U}(r_{P}) &= g\left(\mathbb{E}[r_{P}], \mathbb{V}(r_{P})\right)\\
&=g(\mu_{P},\sigma_{P}^{2})\\
&=g(\bf{w}'\bf{\mu},\bf{w}'\Sigma\bf{w})
\end{align*}


$\Rightarrow$ **vastly simplifies** optimal portfolio selection

</section>
<section data-transition="slide-in none-out">
<h2>Simplification</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/multivar_model.svg"
alt="Number of observations" width="1000px"/>
</p>
</section>

<section data-transition="none">
<h2>Simplification</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/mu_sigma_model1.svg"
alt="Number of observations" width="1000px"/>
</p>
</section>

<section data-transition="none">
<h2>Simplification</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/mu_sigma_model.svg"
alt="Number of observations" width="1000px"/>
</p>



<!--
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

### Show single period Markowitz

- show strategies in usual Markowitz $\mu$-$\sigma$ graphics

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

How are $\mu$ and $\sigma$ derived? $\Rightarrow$ see estimation part

-->

### Criticism

**Symmetric measure $\sigma_{P}$** inadequate to evaluate risk of
**asymmetric** portfolio return distribution:

- **$r_{P}$** sum of asymmetric discrete asset returns

### Alternative I: logarithmic returns

. . .

**Justification**: logarithmic returns are almost symmetric

### Problem I

Portfolio return is **not** a **linear** function of asset returns
anymore 

. . .

\begin{align*}
r_{P}^{log}&=\ln\left(w_{1}\exp(r_{1}^{log})+ \ldots + w_{N}\exp(r_{N}^{log})\right)\\
&\neq w_{1}r_{1}^{log}+ \ldots + w_{N}r_{N}^{log}
\end{align*}

. . .

\begin{align*}
\mu_{P}^{log}&=\mathbb{E}[r_{P}^{log}]\\
&=?\\
&\neq w_{1}\mu_{1}^{log}+\ldots + w_{N}\mu_{N}^{log}
\end{align*}

### Problem II

Measuring risk in terms of $\sigma^{log}_{P}$ is distorting due to
the **non-linear** relation to discrete returns:

\begin{equation*}
r_{P}=\exp(r_{P}^{log}) - 1
\end{equation*}

</section>
<section data-transition="slide-in none-out">
<h2>Non-linear transformation</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/logSigmaIncrease1.svg"
alt="Number of observations" width="1000px"/>
</p>
</section>

<section data-transition="none">
<h2>distorts
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/logSigmaIncrease2.svg"
alt="Number of observations" width="1000px"/>
</p>
</section>

<section data-transition="none">
<h2>expectations</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/logSigmaIncrease3.svg"
alt="Number of observations" width="1000px"/>
</p>
</section>

<section data-transition="none">
<h2>Increasing $\bf{\sigma^{log}}$</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/logSigmaIncrease5.svg"
alt="Number of observations" width="1000px"/>
</p>
</section>

<section data-transition="none">
<h2>also increases $\bf{\mu}$</h2>
<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/logSigmaIncrease6.svg"
alt="Number of observations" width="1000px"/>
</p>

###

**Example** 

With normally distributed log returns

\begin{equation*}
r_{P}^{log}\sim\mathcal{N}(0, \sigma^{2})
\end{equation*}

. . .

we get

$$\begin{aligned}
\mu(r)&=\exp\left( \mu(r^{log}) + \frac{\sigma^{2}(r^{log})}{2}\right)-1\\
\sigma^{2}(r)&=\left( \exp\left( \sigma^{2}(r^{log}) \right)-1\right)
\exp\left( 2\mu(r^{log})+\sigma^{2}(r^{log}) \right)
\end{aligned}$$


### Alternative II: VaR

- **distribution** of portfolio return $r_{P}$ **required**

. . .

$\Rightarrow$ evaluation **costly** for given weights 


# Iterated single period

###

Assumption: **optimality** criterion given **in each period**

. . .

$\Rightarrow$ **sequence** of single period optimizations

### Additional challenge

- $\bf{\mu}$ and $\bf{\Sigma}$ change over time

. . .

$\Rightarrow$ optimal portfolio changes over time: **rebalancing**

. . .

$\Rightarrow$ **trading costs** occur 

### Tradeoff

Rebalancing **costs vs benefits**:

. . .

- **costs** can easily be calculated as trading costs

. . .

- **benefits** depend on all **future periods**


### Example

Changing distribution in **single period** only

. . .

$\Rightarrow$ rebalancing twice might not be worthwhile

### All-in-fee

Customer and asset manager **incentives** are not aligned with
**all-in-fee**:

. . .

- only customers profit from rebalancing
- only asset manager affected by trading costs

. . .

$\Rightarrow$ customers prefer optimal portfolio at each time


# True multi-period

## Deviating horizons

- **long horizon** more meaningful when determining **optimality**

. . .

- **short horizon** simplifies **estimation** of return distributions

. . .


$\Rightarrow$ how does **yearly** target **translate** into a
**weekly** target? 


### Top-down approach

Non-unique short-term targets

<p align="center">
<img src="../AssetMgmtAnalysis/unreplicatable_pics/noPathDependence.svg"
alt="Number of observations" width="1000px"/>
</p>

### Preferences

. . .

**Customer**

- risk aversion
- tax harvesting

. . .

**Company**

-  trading costs

### Bottom-up approach

Does given short-term portfolio **align** with desired **long-term
target**?

. . .

$\Rightarrow$ derive **long-term properties**

### Simplest case

**Long-term target** defined in terms of **moments only**

\begin{align*}
\min \quad\sigma_{1:T,P}^{2}\\
\text{ subject to } \quad\mu_{1:T,P}&=\mu^{*}
\end{align*}

### Annualization

Finding **long-term** portfolio **moments**

\begin{equation*}
(\mu_{1:T,P},\sigma_{1:T,P})
\end{equation*}
**associated with** given

- current weights $\bf{w}$
- current short-term asset moments 

\begin{equation*}
(\mu_{1,i},\sigma_{1,i})
\end{equation*}


### Square-root-of-time

**First guess**:

- get single period portfolio moments

\begin{align*}
\mu_{1,P}&=\bf{w}'\bf{\mu_{1}}\\
\sigma_{1,P}&=\sqrt{\bf{w}'\Sigma_{1}\bf{w}}
\end{align*}

. . .

- **scaling**

\begin{equation*}
\mu_{1:T,P}=T\mu_{1,P}\\
\sigma_{1:T,P}=\sqrt{T}\sigma_{1,P}
\end{equation*}

### Problem I

- aggregation over time requires **multiplication** of single period
  returns in the **discrete return** case


\begin{equation*}
(1+r_{1:T})=(1+r_{1})\cdot \ldots \cdot (1+r_{T})
\end{equation*}


### Assumptions

Possible solution, using assumptions

- **independence** over time
- **normally** distributed log returns

### 

- **translate** single period discrete moments **to** moments of
  **logarithmic returns**

$$\begin{aligned}
\sigma^{2}(r^{log}_{1})&=\log\left( 1+\frac{\sigma^{2}(R_{1})}{\mu^{2}(R_{1})} \right)\\
\mu(r^{log}_{1})&=\log(\mu(R_{1}))-\frac{1}{2}\sigma^{2}(r^{log}_{1})
\end{aligned}$$


###

- **scale** logarithmic moments

$$\begin{aligned}
\mu(r^{log}_{1:T})&=T\mu(r^{log}_{1})\\
\sigma(r^{log}_{1:T})&=\sqrt{T}\sigma(r^{log}_{1})
\end{aligned}$$


###

- **translate** logarithmic return moments back **to discrete** return
  moments

$$\begin{aligned}
\mu(R_{1:T})&=\exp\left( \mu(r^{log}_{1:T}) + \frac{\sigma^{2}(r^{log}_{1:T})}{2}\right)\\
\sigma^{2}(R_{1:T})&=\left( \exp\left( \sigma^{2}(r^{log}_{1:T}) \right)-1\right)
\exp\left( 2\mu(r^{log}_{1:T})+\sigma^{2}(r^{log}_{1:T}) \right)
\end{aligned}$$



### Problem II

Asset moments are **changing** over time

. . .

$\Rightarrow$ scaling up current high-volatility moments will pretend
high-volatility **persists** over the **complete long-term horizon**


###

**First guess**:

- get **univariate portfolio return data** associated with given
  weights

. . .

- **estimate model** on univariate portfolio return data (GARCH?)

. . .

- simulate and **aggregate** future single period portfolio returns
  **to yearly portfolio returns**

### Problem III

- even without portfolio rebalancing portfolio **weights** are
  **changing** over time **due to price changes**

. . .

$$
\begin{equation*}
w_{t+1,j}=w_{t,j}\frac{(1+R_{t,j})}{1+R_{t,P}}
\end{equation*}
$$

. . .

$\Rightarrow$ keeping **weights fixed** over time requires active
portfolio **rebalancing**

### 

**Alternative**:

- estimate **multivariate** return **distribution**
- simulate multi-period returns
- calculate multi-period portfolio returns with changing weights 

<!--

### Problems current approach

- square-root-of-time scaling with given conditional moments
- example: high-volatility sigmas and square-root-of-time scaling will
  pretend that sigmas will stay high for the whole period


# Empirical application

### TODO: plain Markowitz real world example

- plain Markowitz example: 
	- simple estimation -> show changes of moments over time
	- show changes of optimal portfolio over time
	- heuristics to reduce turnover
	- comparison to buy and hold / equally weighted

- benefits:
	- reduced drawdown
	- less volatility


### Prices

<p align="center">
<img src="../AssetMgmtAnalysis/pics/scacap_descriptive_pics-1.svg"
alt="Number of observations" style="background-color:white" width="1000px"/>
</p>

### Cumulative returns

<p align="center">
<img src="../AssetMgmtAnalysis/pics/scacap_descriptive_pics-2.svg"
alt="Number of observations" style="background-color:white" width="1000px"/>
</p>

### Normed logarithmic prices

<p align="center">
<img src="../AssetMgmtAnalysis/pics/scacap_descriptive_pics-3.svg"
alt="Number of observations" style="background-color:white" width="1000px"/>
</p>

### Colored asset class

<p align="center">
<img src="../AssetMgmtAnalysis/pics/scacap_descriptive_pics-4.svg"
alt="Number of observations" style="background-color:white" width="1000px"/>
</p>

### Colored region

<p align="center">
<img src="../AssetMgmtAnalysis/pics/scacap_descriptive_pics-5.svg"
alt="Number of observations" style="background-color:white" width="1000px"/>
</p>

### Colored risk class

<p align="center">
<img src="../AssetMgmtAnalysis/pics/scacap_descriptive_pics-6.svg"
alt="Number of observations" style="background-color:white" width="1000px"/>
</p>

### Return series

<p align="center">
<img src="../AssetMgmtAnalysis/pics/scacap_descriptive_pics-7.svg"
alt="Number of observations" style="background-color:white" width="1000px"/>
</p>


# Estimation

### Bootstrap 

- conduct bootstrap example for unchanging moments
- bootstrapping time series is not easy!

###

When should estimation be tackled?!

- in plain Markowitz example with moments only?
- in single period VaR example?
- in part on bootstrapping?

### Plain Markowitz estimation:

- simplified multivariate estimation
- estimate $\bf{\mu},\bf{\Sigma}$ only
- no distribution required for $r_{1},...,r_{n}$

### Markowitz insights

- how concave is attainable set
- how do underlying weights change locally
- what is exact influence of correlation?


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
-->

# Conclusion / outlook

### Additional challenges

- **integer** volumes

. . .

- **legal constraints**

. . .

- **tax** loss harvesting: defer taxes into future

. . .

- **no real-time trading**: price changes
	- limit orders
	- feedback loops

. . .

- data **frequency**:
	- daily
	- weekly

. . .

- incoming / outgoing **cash-flows**

### Things to investigate

- discrete returns

. . .

- is VaR really required as single period target?
	- translate yearly VaR in yearly volatility
	- translate yearly volatility in weekly volatility
	- do single period optimization with weekly volatility


### Things to investigate

- brute force constrained optimization:
	- maximize μ given σ

. . .

- adapt risk target to current environment

. . .

- estimation: 
	- factor model
	- resampling

. . .

- reduce turnover



