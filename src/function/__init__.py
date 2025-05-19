import numpy as np
import pandas as pd
import yfinance as yf
import matplotlib.pyplot as plt
from scipy.optimize import minimize
from tabulate import tabulate

def download_prices(tickers, start, end):
    data = yf.download(tickers, start=start, end=end, auto_adjust=True)['Close']
    return data.resample('M').last()

def normalize_prices(df):
    return df / df.iloc[0]

def compute_monthly_returns(price_data):
    return price_data.pct_change().dropna()

def portfolio_return(weights, mean_returns):
    return np.dot(weights, mean_returns)

def portfolio_risk(weights, cov_matrix):
    return np.sqrt(np.dot(weights.T, np.dot(cov_matrix, weights)))

def minimize_volatility(mean_returns, cov_matrix, allow_short=False):
    num_assets = len(mean_returns)
    constraints = ({'type': 'eq', 'fun': lambda x: np.sum(x) - 1})
    bounds = tuple((-1.5, 1.5) if allow_short else (0, 1) for _ in range(num_assets))

    result = minimize(portfolio_risk, num_assets * [1. / num_assets],
                      args=(cov_matrix,), method='SLSQP', bounds=bounds,
                      constraints=constraints)
    return result

def compute_tangency_portfolio(mean_returns, cov_matrix, risk_free_rate, allow_short=False):
    num_assets = len(mean_returns)

    def neg_sharpe(weights):
        port_return = portfolio_return(weights, mean_returns)
        port_risk = portfolio_risk(weights, cov_matrix)
        return - (port_return - risk_free_rate) / port_risk

    constraints = ({'type': 'eq', 'fun': lambda x: np.sum(x) - 1})
    bounds = tuple((-1.5, 1.5) if allow_short else (0, 1) for _ in range(num_assets))

    result = minimize(neg_sharpe, num_assets * [1. / num_assets],
                      method='SLSQP', bounds=bounds, constraints=constraints)
    return result

def plot_normalized_prices(prices):
    norm = normalize_prices(prices)

    plt.figure(figsize=(14, 7))
    plt.rcParams.update({
        'font.family': 'serif',
        'font.serif': ['Georgia'],
        'axes.edgecolor': 'gray',
        'axes.linewidth': 0.8,
    })

    colors = {'VBLIX': '#1f77b4', 'VEUSX': '#d62728'}

    for column in norm.columns:
        plt.plot(norm.index, norm[column], label=column, color=colors[column], linewidth=2)

    events = [
        {"start": "2015-12-01", "end": "2016-07-01", "label": "Brexit Referendum", "color": "#96c2ff", "alpha": 0.3},
        {"start": "2018-08-01", "end": "2019-06-01", "label": "Trade War Tension", "color": "#b5e48c", "alpha": 0.3},
        {"start": "2020-01-01", "end": "2020-09-01", "label": "COVID-19 shock", "color": "#adb5bd", "alpha": 0.4},
        {"start": "2021-11-01", "end": "2022-09-01", "label": "Ukraine war", "color": "#f08080", "alpha": 0.3},
    ]

    for event in events:
        plt.axvspan(pd.to_datetime(event["start"]), pd.to_datetime(event["end"]),
                    color=event["color"], alpha=event["alpha"], label=event["label"])

    plt.title("Normalized Monthly Prices — VEUSX vs VBLIX (2015–2024)", fontsize=20, weight='bold', pad=15)
    plt.xlabel("Date", fontsize=15)
    plt.ylabel("Normalized Price", fontsize=15)
    plt.grid(True, linestyle='--', alpha=0.5)

    handles, labels = plt.gca().get_legend_handles_labels()
    by_label = dict(zip(labels, handles))  # supprime doublons
    plt.legend(by_label.values(), by_label.keys(), loc='upper left', frameon=True, fontsize=15, framealpha=0.9)

    plt.tight_layout()
    plt.show()

def plot_efficient_frontier(mean_returns, cov_matrix, rf,
                                    min_vol_long, tan_port_long,
                                    min_vol_short, tan_port_short):
    def generate_frontier(mean_returns, cov_matrix, rf, allow_short=False):
        weights_range = np.linspace(-0.5, 1.5, 300) if allow_short else np.linspace(0, 1, 200)
        risks, returns, sharpes = [], [], []

        for w in weights_range:
            weights = np.array([w, 1 - w])
            if np.abs(np.sum(weights) - 1) > 1e-5:
                continue
            port_ret = np.dot(weights, mean_returns)
            port_risk = np.sqrt(np.dot(weights.T, np.dot(cov_matrix, weights)))
            if port_risk > 0:
                risks.append(port_risk)
                returns.append(port_ret)
                sharpes.append((port_ret - rf) / port_risk)

        return np.array(risks), np.array(returns), np.array(sharpes)

    r_long, re_long, s_long = generate_frontier(mean_returns, cov_matrix, rf, allow_short=False)
    r_short, re_short, s_short = generate_frontier(mean_returns, cov_matrix, rf, allow_short=True)

    # Tangency et GMV (long)
    tan_ret_long = np.dot(tan_port_long.x, mean_returns)
    tan_risk_long = np.sqrt(np.dot(tan_port_long.x.T, np.dot(cov_matrix, tan_port_long.x)))
    gmv_ret = np.dot(min_vol_long.x, mean_returns)
    gmv_risk = np.sqrt(np.dot(min_vol_long.x.T, np.dot(cov_matrix, min_vol_long.x)))

    # Tangency (short)
    tan_ret_short = np.dot(tan_port_short.x, mean_returns)
    tan_risk_short = np.sqrt(np.dot(tan_port_short.x.T, np.dot(cov_matrix, tan_port_short.x)))

    # Capital Market Lines
    slope_long = (tan_ret_long - rf) / tan_risk_long
    slope_short = (tan_ret_short - rf) / tan_risk_short
    cml_x = np.linspace(0, max(r_short.max(), r_long.max()) * 1.2, 100)
    cml_y_long = rf + slope_long * cml_x
    cml_y_short = rf + slope_short * cml_x

    # Plot
    plt.figure(figsize=(12, 7))
    risks_all = np.concatenate([r_long, r_short])
    returns_all = np.concatenate([re_long, re_short])
    sharpes_all = np.concatenate([s_long, s_short])

    scatter = plt.scatter(risks_all, returns_all, c=sharpes_all, cmap='plasma', s=18, edgecolors='black', linewidths=0.3, alpha=0.75)

    plt.plot(cml_x, cml_y_long, color='#FF0000', linestyle='--', linewidth=2, label='CML (No Short)')
    plt.plot(cml_x, cml_y_short, color='green', linestyle='--', linewidth=2, label='CML (Short OK)')

    plt.scatter(gmv_risk, gmv_ret, color='gold', edgecolor='black', s=110, marker='o', label="GMV")
    plt.scatter(tan_risk_long, tan_ret_long, color='#FF0000', edgecolor='black', marker='*', s=220, label="Tangency (No Short)")
    plt.scatter(tan_risk_short, tan_ret_short, color='green', edgecolor='black', marker='*', s=220, label="Tangency (Short OK)")

    plt.title("Efficient Frontier — With and Without Short Selling", fontsize=20, weight='bold')
    plt.xlabel("Volatility (Std Dev)", fontsize=15)
    plt.ylabel("Expected Monthly Return", fontsize=15)
    cbar = plt.colorbar(scatter)
    cbar.set_label("Sharpe Ratio")
    plt.grid(True, linestyle='--', alpha=0.5)
    plt.legend(loc='upper left', frameon=True, fontsize=15)
    plt.tight_layout()
    plt.show()

def plot_efficient_frontiers_n_assets(mean_returns, cov_matrix, rf,
                                            min_vol_long, tan_port_long,
                                            min_vol_short, tan_port_short):
    def generate_clean_portfolios(mean_returns, cov_matrix, rf, allow_short=False, num_points=3000):
        np.random.seed(42)
        num_assets = len(mean_returns)
        risks, returns, sharpes = [], [], []

        if allow_short:
            weights_array = np.random.uniform(-1.5, 2.5, size=(num_points, num_assets))
            weights_array = weights_array / weights_array.sum(axis=1, keepdims=True)
        else:
            weights_array = np.random.dirichlet(np.ones(num_assets), num_points)

        for weights in weights_array:
            if np.abs(np.sum(weights) - 1) > 1e-4:
                continue
            port_ret = np.dot(weights, mean_returns)
            port_risk = np.sqrt(np.dot(weights.T, np.dot(cov_matrix, weights)))
            if 0 < port_risk < 0.2:  # filtre anti-outliers
                risks.append(port_risk)
                returns.append(port_ret)
                sharpes.append((port_ret - rf) / port_risk)

        return np.array(risks), np.array(returns), np.array(sharpes)

    r_long, re_long, s_long = generate_clean_portfolios(mean_returns, cov_matrix, rf, allow_short=False)
    r_short, re_short, s_short = generate_clean_portfolios(mean_returns, cov_matrix, rf, allow_short=True)

    risks_all = np.concatenate([r_long, r_short])
    returns_all = np.concatenate([re_long, re_short])
    sharpes_all = np.concatenate([s_long, s_short])

    tan_ret_long = np.dot(tan_port_long.x, mean_returns)
    tan_risk_long = np.sqrt(np.dot(tan_port_long.x.T, np.dot(cov_matrix, tan_port_long.x)))
    gmv_ret = np.dot(min_vol_long.x, mean_returns)
    gmv_risk = np.sqrt(np.dot(min_vol_long.x.T, np.dot(cov_matrix, min_vol_long.x)))

    tan_ret_short = np.dot(tan_port_short.x, mean_returns)
    tan_risk_short = np.sqrt(np.dot(tan_port_short.x.T, np.dot(cov_matrix, tan_port_short.x)))

    slope_long = (tan_ret_long - rf) / tan_risk_long
    slope_short = (tan_ret_short - rf) / tan_risk_short
    cml_x = np.linspace(0, max(risks_all) * 1.1, 100)
    cml_y_long = rf + slope_long * cml_x
    cml_y_short = rf + slope_short * cml_x

    plt.figure(figsize=(12, 7))
    scatter = plt.scatter(risks_all, returns_all, c=sharpes_all, cmap='plasma', s=18, edgecolors='black', linewidths=0.3, alpha=0.75)

    plt.plot(cml_x, cml_y_long, color='#FF0000', linestyle='--', linewidth=2, label='CML (No Short)')
    plt.plot(cml_x, cml_y_short, color='green', linestyle='--', linewidth=2, label='CML (Short OK)')

    plt.scatter(gmv_risk, gmv_ret, color='gold', edgecolor='black', s=110, marker='o', label="GMV")
    plt.scatter(tan_risk_long, tan_ret_long, color='#FF0000', edgecolor='black', marker='*', s=200, label="Tangency (No Short)")
    plt.scatter(tan_risk_short, tan_ret_short, color='green', edgecolor='black', marker='*', s=200, label="Tangency (Short OK)")

    plt.title("Efficient Frontier — 5 Assets With and Without Short Selling", fontsize=20, weight='bold')
    plt.xlabel("Volatility (Std Dev)", fontsize=15)
    plt.ylabel("Expected Monthly Return", fontsize=15)
    cbar = plt.colorbar(scatter)
    cbar.set_label("Sharpe Ratio")
    plt.grid(True, linestyle='--', alpha=0.5)
    plt.legend(loc='upper left', frameon=True, fontsize=15)
    plt.tight_layout()
    plt.show()

def display_summary(mean_returns, cov_matrix, min_vol, tan_port, rf):
    tickers = mean_returns.index.tolist()
    min_weights = min_vol.x.round(4)
    tan_weights = tan_port.x.round(4)

    summary = pd.DataFrame({
        "Portfolio": ["Global Min Var", "Tangency"],
        **{ticker: [min_weights[i], tan_weights[i]] for i, ticker in enumerate(tickers)},
        "Return": [
            round(np.dot(min_weights, mean_returns), 4),
            round(np.dot(tan_weights, mean_returns), 4)
        ],
        "Volatility": [
            round(np.sqrt(np.dot(min_weights.T, np.dot(cov_matrix, min_weights))), 4),
            round(np.sqrt(np.dot(tan_weights.T, np.dot(cov_matrix, tan_weights))), 4)
        ],
        "Sharpe Ratio": [
            round((np.dot(min_weights, mean_returns) - rf) /
                  np.sqrt(np.dot(min_weights.T, np.dot(cov_matrix, min_weights))), 4),
            round((np.dot(tan_weights, mean_returns) - rf) /
                  np.sqrt(np.dot(tan_weights.T, np.dot(cov_matrix, tan_weights))), 4)
        ]
    })

    print("\n Sample Mean Monthly Returns")
    print(tabulate(mean_returns.round(4).to_frame("Mean Return").reset_index(), headers="keys", tablefmt="github"))

    print("\n Covariance Matrix")
    print(tabulate(cov_matrix.round(4), headers="keys", tablefmt="github", floatfmt=".4f"))

    print("\n Portfolio Summary")
    print(tabulate(summary, headers="keys", tablefmt="github"))

    return summary