from src.function import (
    download_prices,
    compute_monthly_returns,
    plot_normalized_prices,
    minimize_volatility,
    compute_tangency_portfolio,
    plot_efficient_frontier,
    plot_efficient_frontiers_n_assets,
    portfolio_return,
    portfolio_risk,
    display_summary
)
import pandas as pd
import numpy as np

risk_free_rate = 0.00025
start_date = "2015-01-01"
end_date = "2024-03-31"

# ─────────────────────────────────────────────────────────────
# PART 1 : VEUSX vs VBLIX
# ─────────────────────────────────────────────────────────────
print("═══════════════════════════════════════════════════")
print(" PART 1: Two Risky Assets (VEUSX & VBLIX)")
print("═══════════════════════════════════════════════════")

tickers_part1 = ["VEUSX", "VBLIX"]
prices_2assets = download_prices(tickers_part1, start=start_date, end=end_date)
plot_normalized_prices(prices_2assets)

returns_2assets = compute_monthly_returns(prices_2assets)
mean_2 = returns_2assets.mean()
cov_2 = returns_2assets.cov()

min_vol_2_long = minimize_volatility(mean_2, cov_2, allow_short=False)
tan_port_2_long = compute_tangency_portfolio(mean_2, cov_2, risk_free_rate, allow_short=False)

min_vol_2_short = minimize_volatility(mean_2, cov_2, allow_short=True)
tan_port_2_short = compute_tangency_portfolio(mean_2, cov_2, risk_free_rate, allow_short=True)

plot_efficient_frontier(mean_2, cov_2, risk_free_rate,
                        min_vol_2_long, tan_port_2_long,
                        min_vol_2_short, tan_port_2_short)

summary_2 = pd.DataFrame({
    "Portfolio": ["GMV — No Short", "Tangency — No Short", "GMV — Short OK", "Tangency — Short OK"],
    tickers_part1[0]: [
        round(min_vol_2_long.x[0], 4),
        round(tan_port_2_long.x[0], 4),
        round(min_vol_2_short.x[0], 4),
        round(tan_port_2_short.x[0], 4)
    ],
    tickers_part1[1]: [
        round(min_vol_2_long.x[1], 4),
        round(tan_port_2_long.x[1], 4),
        round(min_vol_2_short.x[1], 4),
        round(tan_port_2_short.x[1], 4)
    ],
    "Return": [
        round(portfolio_return(min_vol_2_long.x, mean_2), 4),
        round(portfolio_return(tan_port_2_long.x, mean_2), 4),
        round(portfolio_return(min_vol_2_short.x, mean_2), 4),
        round(portfolio_return(tan_port_2_short.x, mean_2), 4)
    ],
    "Volatility": [
        round(portfolio_risk(min_vol_2_long.x, cov_2), 4),
        round(portfolio_risk(tan_port_2_long.x, cov_2), 4),
        round(portfolio_risk(min_vol_2_short.x, cov_2), 4),
        round(portfolio_risk(tan_port_2_short.x, cov_2), 4)
    ],
    "Sharpe Ratio": [
        round((portfolio_return(min_vol_2_long.x, mean_2) - risk_free_rate) / portfolio_risk(min_vol_2_long.x, cov_2), 4),
        round((portfolio_return(tan_port_2_long.x, mean_2) - risk_free_rate) / portfolio_risk(tan_port_2_long.x, cov_2), 4),
        round((portfolio_return(min_vol_2_short.x, mean_2) - risk_free_rate) / portfolio_risk(min_vol_2_short.x, cov_2), 4),
        round((portfolio_return(tan_port_2_short.x, mean_2) - risk_free_rate) / portfolio_risk(tan_port_2_short.x, cov_2), 4)
    ]
})

print("──────────────── No Short Selling ────────────────")
display_summary(mean_2, cov_2, min_vol_2_long, tan_port_2_long, rf=risk_free_rate)

print("\n──────────────── With Short Selling ────────────────")
display_summary(mean_2, cov_2, min_vol_2_short, tan_port_2_short, rf=risk_free_rate)

# ─────────────────────────────────────────────────────────────
# PART 2 : 5 Stocks (GOOG, CSCO, LOGI, AMZN, AAPL)
# ─────────────────────────────────────────────────────────────
print("═══════════════════════════════════════════════════")
print(" PART 2: Five Risky Assets + Risk-Free Asset")
print("═══════════════════════════════════════════════════")

tickers_part2 = ["GOOG", "CSCO", "LOGI", "AMZN", "AAPL"]
prices_5assets = download_prices(tickers_part2, start=start_date, end=end_date)
returns_5assets = compute_monthly_returns(prices_5assets)

mean_5 = returns_5assets.mean()
cov_5 = returns_5assets.cov()

min_vol_5_long = minimize_volatility(mean_5, cov_5, allow_short=False)
tan_port_5_long = compute_tangency_portfolio(mean_5, cov_5, risk_free_rate, allow_short=False)

min_vol_5_short = minimize_volatility(mean_5, cov_5, allow_short=True)
tan_port_5_short = compute_tangency_portfolio(mean_5, cov_5, risk_free_rate, allow_short=True)
plot_efficient_frontiers_n_assets(mean_5, cov_5, risk_free_rate,
                                  min_vol_5_long, tan_port_5_long,
                                  min_vol_5_short, tan_port_5_short)

summary_5 = pd.DataFrame({
    "Portfolio": ["GMV — No Short", "Tangency — No Short", "GMV — Short OK", "Tangency — Short OK"],
    tickers_part1[0]: [
        round(min_vol_5_long.x[0], 4),
        round(tan_port_5_long.x[0], 4),
        round(min_vol_5_short.x[0], 4),
        round(tan_port_5_short.x[0], 4)
    ],
    tickers_part1[1]: [
        round(min_vol_5_long.x[1], 4),
        round(tan_port_5_long.x[1], 4),
        round(min_vol_5_short.x[1], 4),
        round(tan_port_5_short.x[1], 4)
    ],
    "Return": [
        round(portfolio_return(min_vol_5_long.x, mean_5), 4),
        round(portfolio_return(tan_port_5_long.x, mean_5), 4),
        round(portfolio_return(min_vol_5_short.x, mean_5), 4),
        round(portfolio_return(tan_port_5_short.x, mean_5), 4)
    ],
    "Volatility": [
        round(portfolio_risk(min_vol_5_long.x, cov_5), 4),
        round(portfolio_risk(tan_port_5_long.x, cov_5), 4),
        round(portfolio_risk(min_vol_5_short.x, cov_5), 4),
        round(portfolio_risk(tan_port_5_short.x, cov_5), 4)
    ],
    "Sharpe Ratio": [
        round((portfolio_return(min_vol_5_long.x, mean_5) - risk_free_rate) / portfolio_risk(min_vol_5_long.x, cov_5), 4),
        round((portfolio_return(tan_port_5_long.x, mean_5) - risk_free_rate) / portfolio_risk(tan_port_5_long.x, cov_5), 4),
        round((portfolio_return(min_vol_5_short.x, mean_5) - risk_free_rate) / portfolio_risk(min_vol_5_short.x, cov_5), 4),
        round((portfolio_return(tan_port_5_short.x, mean_5) - risk_free_rate) / portfolio_risk(tan_port_5_short.x, cov_5), 4)
    ]
})

print("──────────────── No Short Selling ────────────────")
display_summary(mean_5, cov_5, min_vol_5_long, tan_port_5_long, rf=risk_free_rate)

print("\n──────────────── With Short Selling ────────────────")
display_summary(mean_5, cov_5, min_vol_5_short, tan_port_5_short, rf=risk_free_rate)