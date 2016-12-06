% Dynamic asset allocation strategies:
% risk analysis

<!--
Possible titles:
- Dynamic multi-asset class investment strategies
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


### Data quality

<p align="center">
<img src="../dissDataAndPics/assetAllocation/pics/missingValuesPerAsset.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>


### Price trajectories

<p align="center">
<iframe frameborder="0" seamless="seamless" width="100%" height="650" src="../dissDataAndPics/assetAllocation/plotlyFigs/normalizedAssetTrajectories.html"></iframe>
</p>

### Unconditional sample moments

<p align="center">
<iframe frameborder="0" seamless="seamless" width="100%" height="650" 
src="../dissDataAndPics/assetAllocation/plotlyFigs/uncondAssetMoments.html"></iframe>
</p>

### Unconditional sample moments

<p align="center">
<img src="../dissDataAndPics/assetAllocation/pics/euroAssetMoments.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>


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

### Asset moments

<p align="center">
<iframe frameborder="0" seamless="seamless" width="100%" height="650" src="../dissDataAndPics/assetAllocation/plotlyFigs/fxAssetMoments.html"></iframe>
</p>


### Correlation effects

<p align="center">
<img src="../dissDataAndPics/assetAllocation/pics/corrComparison.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>


###

- Scalable data
- currencies

# Stochastic model

### How to make model?

- directly capture portfolio components
- derive portfolio component properties from low-level risk factors
- why? -> bond properties have deterministic components

# Stock market properties


### ACWI data analysis

- common properties of stocks (mu / sigma)

### ACWI data filtering

- which filters where applied

### ACWI risk / return results

- unconditionally

### Time-varying risk / return

- how do conditional mus relate to conditional sigmas?

### General co-movements

- co-movements between different assets

\begin{equation*}
\begin{bmatrix}
\mu_{1}\\
\sigma_{1}\\
\text{VaR}_{1}
\end{bmatrix}\text{vs.}
\begin{bmatrix}
\mu_{2}\\
\sigma_{2}\\
\text{VaR}_{2}
\end{bmatrix}
\end{equation*}

