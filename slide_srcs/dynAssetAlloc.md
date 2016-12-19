% CAPE ratios
% and what they teach us about long-term stock market dynamics

<!--
Possible titles:
- Dynamic multi-asset class investment strategies
- CAPE Ratio and what we can learn about long-term asset dynamics
- CAPE ratios and what they tell us about long-term asset dynamics
- CAPE ratios and what they teach us about stock market returns
- Dynamic asset allocation strategies: risk analysis
-->


<!--
# Notes

## Outline

- task: multiple asset class data
- stochastic model: portfolio components vs risk factors
- bond price modeling problem
- stock market experiences
- general problems
	- Markowitz (over reliance)
	- currency
- performance analysis

###

| Bloomberg Ticker | Asset Class      | Region | CCY |
|------------------+------------------+--------+-----|
| LEATTREU Index   | government bonds | eu     | EUR |
| BCEY4T Index     | government bonds | us     | USD |
| JPEICORE Index   | government bonds | em     | USD |
| IBXXDECT Index   | covered bonds    | eu     | EUR |
| IBOXIG Index     | corporate bonds  | us     | USD |
| LECPTREU Index   | corporate bonds  | eu     | EUR |
| DAX Index        | equities         | de     | EUR |
| SXXR Index       | equities         | eu     | EUR |
| SPTR500N Index   | equities         | us     | USD |
| NDDLJN Index     | equities         | jp     | JPY |
| NDUEEGF Index    | equities         | em     | USD |
| NDDUPXJ Index    | equities         | as     | USD |
| CRYTR Index      | commodities      | glob   | USD |
| RNGL Index       | real estate      | glob   | EUR |

-->

# Introduction

### Target

Analyze **risk and return** profiles of **dynamic** asset management
**strategies**. 

### Dynamic vs static

Static portfolios are unrealistic, as they do not allow portfolio
adaptions to **react to changing market environment**.

### Possible approaches

- **backtest**: apply strategy to single observable path of historic asset
  trajectories

. . .

- **simulation**: simulate asset trajectories in order to evaluate risk
  profile of strategy



### Model requirements: backtest

- **no model** at all for some strategies (**fixed weights**)

. . .

- **short-term model** for strategies that need to **identify**
  current **market situation** at each point in time 

. . .

⇒ **conditional distribution**

### Model requirements: simulation

- **static**: uni-variate model as **proxy** to
  buy-and-hold 

<!--
daily re-balanced portfolio in reality) 
-->

. . .

- **dynamic**: multi-dimensional asset model

. . .

- identify and **replicate** long-term market patterns for simulation

. . .

⇒ conditional distributions together with **dynamics** of how they
evolve over time

<!--  
- dynamic vs static: what's risk of missing a turn and skidding off
  road? Static framework would not allow for adaptions to changing
  environment
--> 


# Universe

### Portfolio components

Portfolio comprises the following **asset classes**:

- government bonds (**gov**)
- covered bonds (**cov**)
- corporate bonds (**corp**)
- equities (**eq**)
- commodities (**com**)
- real estate (**real**)


### Government bonds


| Bloomberg Ticker | Region | CCY | Asset label |
|------------------+--------+-----+-------------|
| LEATTREU Index   | eu     | EUR | **govEu**   |
| BCEY4T Index     | us     | USD | **govUs**   |
| JPEICORE Index   | em     | USD | **govEm**   |

### Covered and corporate bonds 

| Bloomberg Ticker | Region | CCY | Asset label |
|------------------+--------+-----+-------------|
| IBXXDECT Index   | eu     | EUR | **covEu**       |
| IBOXIG Index     | us     | USD | **corpUs**      |
| LECPTREU Index   | eu     | EUR | **corpEu**      |

### Equities

| Bloomberg Ticker | Region | CCY | Asset label |
|------------------+--------+-----+-------------|
| DAX Index        | de     | EUR | **eqDe**    |
| SXXR Index       | eu     | EUR | **eqEu**    |
| SPTR500N Index   | us     | USD | **eqUs**    |
| NDDLJN Index     | jp     | JPY | **eqJp**    |
| NDUEEGF Index    | em     | USD | **eqEm**    |
| NDDUPXJ Index    | as     | USD | **eqAs**    |

### Other asset classes

| Bloomberg Ticker | Asset Class | Region | CCY | Asset label  |
|------------------+-------------+--------+-----+--------------|
| CRYTR Index      | commodities | glob   | USD | **globCom**  |
| RNGL Index       | real estate | glob   | EUR | **globReal** |


### Price trajectories

<p align="center">
<iframe frameborder="0" seamless="seamless" width="100%" height="650" src="../dissDataAndPics/assetAllocation/plotlyFigs/normalizedAssetTrajectories.html"></iframe>
</p>

<!--

### Unconditional sample moments

<p align="center">
<iframe frameborder="0" seamless="seamless" width="100%" height="650" 
src="../dissDataAndPics/assetAllocation/plotlyFigs/uncondAssetMoments.html"></iframe>
</p>

-->

### Unconditional sample moments

<p align="center">
<img src="../dissDataAndPics/assetAllocation/pics/euroAssetMoments.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

. . .

- why do some equity markets have such small mean returns?

### Long-term model

Is this 16 year sample representative for:

. . .

- current market situation?

. . .

- true unconditional long-term distribution?

<!--

### Missing data

\begin{equation*}


\text{prices} = 
\begin{bmatrix}
110\\
120\\
\text{NA}\\
\text{NA}\\
150\\
160
\end{bmatrix} \quad\Rightarrow\quad
\Delta=\begin{bmatrix}
\text{NA}\\
10\\
\text{NA}\\
\text{NA}\\
\text{NA}\\
10
\end{bmatrix}

\end{equation*}

. . .

\

\begin{equation*}
\Delta=50 \quad vs\quad \sum=20
\end{equation*}

### LOCF

\begin{equation*}


\text{prices} = 
\begin{bmatrix}
110\\
120\\
\textbf{120}\\
\textbf{120}\\
150\\
160
\end{bmatrix} \quad\Rightarrow\quad
\Delta=\begin{bmatrix}
\text{NA}\\
10\\
0\\
0\\
30\\
10
\end{bmatrix}

\end{equation*}

. . .

\

\begin{equation*}
\Delta=50 \quad vs\quad \sum=50
\end{equation*}

. . .

- zero return inflation
- de facto use of multi-period return


### What I do

Filling small gaps:

\begin{equation*}


\text{prices} = 
\begin{bmatrix}
110\\
120\\
\text{NA}\\
130\\
140\\
150
\end{bmatrix} \quad\Rightarrow\quad
\Delta=\begin{bmatrix}
\text{NA}\\
10\\
\text{NA}\\
\textbf{10}\\
10\\
10
\end{bmatrix}

\end{equation*}

. . .

- no zero return inflation
- differences sum up to overall difference

###

Keeping large gaps:

\begin{equation*}


\text{prices} = 
\begin{bmatrix}
110\\
120\\
\text{NA}\\
\text{NA}\\
150\\
160
\end{bmatrix} \quad\Rightarrow\quad
\Delta=\begin{bmatrix}
\text{NA}\\
10\\
\text{NA}\\
\text{NA}\\
\text{NA}\\
10
\end{bmatrix}

\end{equation*}

. . .

\ 

- no zero return inflation
- no multi-period returns
- but: individual differences do not aggregate to full difference


### Missing observations

<p align="center">
<img src="../dissDataAndPics/assetAllocation/pics/missingValuesPerAsset.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Unconditional mean

<p align="center">
<img src="../dissDataAndPics/assetAllocation/pics/musWithMissingValues.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>


### Unconditional mean

<p align="center">
<iframe frameborder="0" seamless="seamless" width="100%" height="650" 
src="../dissDataAndPics/assetAllocation/plotlyFigs/musWithMissingValues.html"></iframe>
</p>

-->

### Unconditional sample moments

<p align="center">
<img src="../dissDataAndPics/assetAllocation/pics/euroAssetMoments.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

. . .

- why are US bonds so risky?

### Fx rates

<p align="center">
<iframe frameborder="0" seamless="seamless" width="100%" height="650" src="../dissDataAndPics/assetAllocation/plotlyFigs/normalizedFxRateTrajectories.html"></iframe>
</p>


### Fx returns

<p align="center">
<img src="../dissDataAndPics/assetAllocation/pics/fxReturns.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Asset moments: local currencies

<p align="center">
<img src="../dissDataAndPics/assetAllocation/pics/localAssetMoments.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Asset moments

<p align="center">
<img src="../dissDataAndPics/assetAllocation/pics/fxAssetMoments.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

. . .

- home-bias justified?

### Asset moments

<p align="center">
<iframe frameborder="0" seamless="seamless" width="100%" height="650" src="../dissDataAndPics/assetAllocation/plotlyFigs/fxAssetMoments.html"></iframe>
</p>


### Correlation effects

<p align="center">
<img src="../dissDataAndPics/assetAllocation/pics/corrComparison.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>


# Long-term asset returns

### Price vs performance index

<p align="center">
<img src="../dissDataAndPics/value/pics/sp500PricesVsPerf.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Annual risk

<p align="center">
<img src="../dissDataAndPics/value/pics/VaR95.png"
alt="Number of observations" width="500px" style="background-color:white" />
</p>

- empirical quantile: -0.1995
- t-location-scale: -0.2074
- parameters, log-returns: $(\mu,\sigma,\nu)=(0.09, 0.15, 9.36)$


### Investment periods

<p align="center">
<img src="../dissDataAndPics/value/pics/SP500_TR_InvestmentPeriods.png"
alt="Number of observations" width="700px" style="background-color:white" />
</p>


</section>
<section data-transition="slide-in none-out">

<p align="center">
<img src="../dissDataAndPics/value/pics/SP500_perf_scaling.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>


</section>
<section data-transition="none-in none-out">

<p align="center">
<img src="../dissDataAndPics/value/pics/SP500_perf_scaling_simulated.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Model

$$\begin{aligned}
X_{t}&=\sigma_{t}\epsilon_{t}, \quad \epsilon_{t}\sim \mathcal{N}(0, 1)\\
\sigma_{t}^{2}&=0.01 + 0.94\sigma_{t-1}^{2}+0.05X_{t-1}^{2}\\
\end{aligned}$$


\begin{equation*}
Y_{t}=\mu + X_{t}
\end{equation*}


### Investment periods

<p align="center">
<img src="../dissDataAndPics/value/pics/SP500_TR_InvestmentPeriods_simulated.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>


# CAPE ratio

### Definition

- cyclically adjusted earnings
\begin{equation*}
\bar{E}_{t}^{\langle 10 \rangle}:=\frac{1}{10}\left( \sum_{i=1}^{10}\bar{E}_{t-i} \right)
\end{equation*}

- CAPE ratio
\begin{equation*}
\text{CAPE}_{t}=\frac{\bar{P}_{t}}{\bar{E}_{t}^{\langle 10 \rangle}}
\end{equation*}

### SP500 in real prices

<p align="center">
<img src="../dissDataAndPics/value/pics/realSP500.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Smoothed earnings

<p align="center">
<img src="../dissDataAndPics/value/pics/cycAdjEarn.png"
alt="Number of observations" width="700px" style="background-color:white" />
</p>

### Historic CAPE

<p align="center">
<img src="../dissDataAndPics/value/pics/CAPEoverTime.png"
alt="Number of observations" width="700px" style="background-color:white" />
</p>

### TR returns, real prices

<p align="center">
<img src="../dissDataAndPics/value/pics/SP500_TR_Real.png"
alt="Number of observations" width="700px" style="background-color:white" />
</p>

### Annual returns

Average annual returns (discrete) for the full sample:

|         | Price Index | Total Return |
|---------+-------------+--------------|
| Nominal |         4.2 | 8.7          |
| Real    |         2.1 | **6.5**      |


### CAPE - predictive power

<p align="center">
<img src="../dissDataAndPics/value/pics/capeFutureReturns.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### CAPE - predictive power

<p align="center">
<img src="../dissDataAndPics/value/pics/capeReturnCorrelations.png"
alt="Number of observations" width="700px" style="background-color:white" />
</p>

### CAPE - predictive power

<p align="center">
<img src="../dissDataAndPics/value/pics/capeFutureTrajectories.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Predictive power: internationally

<p align="center">
<img src="../dissDataAndPics/value/copiedElsewhere/historic_CAPE_return_relation.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### 

- when mean returns are **constant** over time, you can **not gain from
  forecasting** values other than the mean

. . .

- the fact that there do exist **forecasting possibilities** does
  **rule out constant means**

. . .

⇒ **mean returns are changing** over time

### Current CAPE ratios

<p align="center">
<img src="../dissDataAndPics/value/copiedElsewhere/CAPE_Ratios_World.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Current country forecast

<p align="center">
<img src="../dissDataAndPics/value/copiedElsewhere/CAPE_country_forecasts.png"
alt="Number of observations" width="500px" style="background-color:white" />
</p>

### Germany potential

<p align="center">
<img src="../dissDataAndPics/value/copiedElsewhere/CAPE_potential_DE.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### US potential

<p align="center">
<img src="../dissDataAndPics/value/copiedElsewhere/CAPE_potential_US.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>


# Modeling CAPE


### CAPE

<p align="center">
<img src="../dissDataAndPics/value/pics/logCAPE.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Prices and earnings

<p align="center">
<img src="../dissDataAndPics/value/pics/logPricesEarnings.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Autocorrelations

<p align="center">
<img src="../dissDataAndPics/value/pics/autocorrs.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Cross-correlations

<p align="center">
<img src="../dissDataAndPics/value/pics/crossCorrsEarnings.png"
alt="Number of observations" width="700px" style="background-color:white" />
</p>

### Cross-correlations

<p align="center">
<img src="../dissDataAndPics/value/pics/crossCorrsCyclAdjEarnings.png"
alt="Number of observations" width="700px" style="background-color:white" />
</p>

### CAPE model

\begin{equation*}
X_{t}=c + aX_{t-1}+\epsilon_{t}, \quad \epsilon_{t}\sim \mathcal{N}(0, \sigma^{2})
\end{equation*}

. . .

\begin{equation*}
X_{t}=0.3 + 0.89X_{t-1}+\epsilon_{t}, \quad \epsilon_{t}\sim \mathcal{N}(0, 0.031)
\end{equation*}

### Simulation

<p align="center">
<img src="../dissDataAndPics/value/pics/simUnivCape.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Cycl. adj. earnings

<p align="center">
<img src="../dissDataAndPics/value/pics/linearTrendCycAdjEarn.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Cycl. adj. earnings

<p align="center">
<img src="../dissDataAndPics/value/pics/autoCorrTrendDeviations.png"
alt="Number of observations" width="700px" style="background-color:white" />
</p>


### Cycl. adj. earnings model


$$\begin{aligned}
Y_{t}&= c_{1} + a_{1} t + X_{t}\\
X_{t}&= c_{2} + a_{2} X_{t-1} + \varepsilon_{t}, \quad \varepsilon_{t}\sim \mathcal{N}(0, \sigma^{2})
\end{aligned}$$

### Cycl. adj. earnings model


$$\begin{aligned}
Y_{t}&= -25.37 + 0.015 t + X_{t}\\
X_{t}&= 0.001 + 0.985 X_{t-1} + \varepsilon_{t}, \quad \varepsilon_{t}\sim \mathcal{N}(0, 0.00127)
\end{aligned}$$

### Cross-dependence: innovations

<p align="center">
<img src="../dissDataAndPics/value/pics/crossDepEarnInnovCAPE.png"
alt="Number of observations" width="700px" style="background-color:white" />
</p>

### Innovations model

\begin{equation*}
\left( \epsilon_{t}, \varepsilon_{t} \right) \sim C^{Ga}_{\rho}
\end{equation*}

. . .

\begin{equation*}
\left( \epsilon_{t}, \varepsilon_{t} \right) \sim C^{Ga}_{\rho=0.18}
\end{equation*}

### Simulated values

<p align="center">
<img src="../dissDataAndPics/value/pics/simCAPEModel.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Simulated price index

<p align="center">
<img src="../dissDataAndPics/value/pics/simAnnualRetsPriceIndex.png"
alt="Number of observations" width="700px" style="background-color:white" />
</p>


### Simulated CAPE return relation

<p align="center">
<img src="../dissDataAndPics/value/pics/simCAPE_ReturnRelation.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Simulated price index

<p align="center">
<img src="../dissDataAndPics/value/pics/simCAPE_returnForecast.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Annual predictions

<p align="center">
<img src="../dissDataAndPics/value/pics/annualPredictions.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Price vs equilibrium price

<p align="center">
<img src="../dissDataAndPics/value/pics/equilibriumPrice.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Expected annual returns

<p align="center">
<img src="../dissDataAndPics/value/pics/simExpReturnsVsReturns.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

### Simulated annual returns

<p align="center">
<img src="../dissDataAndPics/value/pics/simAnnualReturns.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>



# Stochastic model

### Risk factors to be modelled

- **mathematical tractability**: model discrete or logarithmic returns?
- **estimation error**: make use of known structures (covariance matrix)
- **frequency**: lower bound given by trading strategy
- **stationarity**: bond portfolio returns
- fractionally integrated: multiples*earnings

### How to make model?

- directly capture portfolio components
- derive portfolio component properties from low-level risk factors
- why? -> bond properties have deterministic components (non-stationarity)

</section>
<section data-transition="slide-in none-out">

<p align="center">
<img src="../dissDataAndPics/assetAllocation/unreplicatablePics/stochModel.svg"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

</section>
<section data-transition="none-in none-out">

<p align="center">
<img src="../dissDataAndPics/assetAllocation/unreplicatablePics/stochModel1.svg"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

</section>
<section data-transition="none-in none-out">


<p align="center">
<img src="../dissDataAndPics/assetAllocation/unreplicatablePics/stochModel2.svg"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

</section>
<section data-transition="none-in none-out">


<p align="center">
<img src="../dissDataAndPics/assetAllocation/unreplicatablePics/stochModel4.svg"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

</section>


# Thoughts

### Frequency problem

- one year VaR with over-lapping one year data: 
	- spuriousity
- one year VaR required at higher frequencies
	- saw-tooth patterns
- adjusting frequency to required frequency:
	- one year VaR from daily data
	- one year VaR from non-overlapping yearly data with adjustments
