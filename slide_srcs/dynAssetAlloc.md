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

- true unconditional 16 year distribution?

. . .

- true unconditional long-term distribution?

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
- but: individual differences to not aggregate to full difference


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

. . .

- still: additional explanation required
- changes in valuation? CAPE ratio

<!--

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

### Asset moments

<p align="center">
<iframe frameborder="0" seamless="seamless" width="100%" height="650" src="../dissDataAndPics/assetAllocation/plotlyFigs/fxAssetMoments.html"></iframe>
</p>


### Correlation effects

<p align="center">
<img src="../dissDataAndPics/assetAllocation/pics/corrComparison.png"
alt="Number of observations" width="800px" style="background-color:white" />
</p>

- TODO: make axes squared


# Stochastic model

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

