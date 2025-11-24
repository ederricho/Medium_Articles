# ----- Code for Ratio Function -----

import numpy as np
from scipy.stats import ks_2samp
from numpy.random import default_rng

rng = default_rng(123)

def compare_correlations(n=300, mc=1000, rho=0.8, plots=True):

    ratios_uncor = np.zeros(mc)
    ratios_cor   = np.zeros(mc)

    # Covariance matrix
    Sigma = np.array([[1, rho],
                      [rho, 1]])

    for i in range(mc):

        # -------- Uncorrelated --------
        X_unc = rng.normal(0, 1, n)
        Y_unc = rng.normal(0, 1, n)
        ratios_uncor[i] = np.mean(X_unc) / np.mean(Y_unc)

        # -------- Correlated --------
        XY = rng.multivariate_normal([0,0], Sigma, size=n)
        X_cor = XY[:,0]
        Y_cor = XY[:,1]
        ratios_cor[i] = np.mean(X_cor) / np.mean(Y_cor)

    # -------- KS Test --------
    ks_stat, p_value = ks_2samp(ratios_uncor, ratios_cor)

    # -------- Optional Plots --------
    if plots:
        import matplotlib.pyplot as plt

        # Boxplot
        plt.figure(figsize=(8,4))
        plt.boxplot([ratios_cor, ratios_uncor],
                    labels=['Correlated', 'Uncorrelated'])
        plt.title("Ratio of Means")
        plt.ylim(-10,10)
        plt.show()

        # Histograms
        plt.figure(figsize=(8,6))
        plt.subplot(2,1,1)
        plt.hist(ratios_cor, bins=50)
        plt.title("Correlated Ratios")

        plt.subplot(2,1,2)
        plt.hist(ratios_uncor, bins=50)
        plt.title("Uncorrelated Ratios")
        plt.show()

        # QQ Plots
        import scipy.stats as st
        plt.figure(figsize=(10,4))
        plt.subplot(1,2,1)
        st.probplot(ratios_cor, dist="norm", plot=plt)
        plt.title("QQ Plot: Correlated")

        plt.subplot(1,2,2)
        st.probplot(ratios_uncor, dist="norm", plot=plt)
        plt.title("QQ Plot: Uncorrelated")
        plt.show()

        print("\nKS Test p-value:", p_value)

    return p_value




# ---- Code for Simulation -----
pvalues = np.zeros(1000)
for i in range(1000):
    pvalues[i] = compare_correlations(plots=False)
