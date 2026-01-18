# Cost of Equity Framework

A fundamental, non-CAPM approach to cost of equity derived from credit spreads and capital structure.

## Core Principle

Equity and debt are claims on the same cash flows. Cost of equity = cost of debt + subordination premium scaled by leverage.

---

## Model 1: Credit Spread (Quarterly)

Cross-sectional regression explaining corporate credit spreads from fundamentals.

```
Spread_i,t = α_t + Σ(X_i,k × f_k,t) + ε_i,t
```

**Factors:**
- LGD (loss given default)
- log(Market Cap)
- Interest Coverage (EBIT / Interest)
- Debt / EBITDA
- ROA
- Sales Growth
- Current Ratio
- Sector / sub-sector dummies

**Calculated inputs:**
```
Tangible Assets = Total Assets − Goodwill − Intangible Assets
LGD = max(0, 1 − Tangible Assets / Total Debt)
```

---

## Model 2: Nowcast to Weekly

Update quarterly spreads using BofA bond index data from FRED.

```
Spread_i,w = Spread_i,t + (Index_w − Index_t)
```

- `Spread_i,t` = fitted spread from Model 1 (quarter-end)
- `Index_w` = BofA index spread at week w
- `Index_t` = BofA index spread at quarter-end

---

## Model 3: Cost of Equity

Two methods based on business model:

### Asset-Based (for asset-heavy / cyclical)

```
Cost of Equity = r_f + Spread / (1 − D/TA)
```

- D/TA = Total Debt / Tangible Assets
- Use when: industrials, utilities, materials, REITs, deep cyclicals
- Breaks when D ≥ TA

### Flows-Based (for asset-light / stable earnings)

```
Cost of Equity = r_f + Spread × Coverage / (Coverage − 1)
```

- Coverage = EBIT / Interest Expense
- Use when: software, tech, services, brands, pharma
- Breaks when Coverage ≤ 1

---

## Method Selection

| Criterion | Asset-Based | Flows-Based |
|-----------|-------------|-------------|
| Intangibles / Assets | < 30% | > 30% |
| EBIT stability | Volatile | Stable |
| D/TA | < 1 | ≥ 1 or unreliable |

---

## Data Flow

```
Quarterly financials → [Model 1] → Spread (quarterly)
                                        ↓
Weekly bond index   → [Model 2] → Spread (weekly)
                                        ↓
Leverage measure    → [Model 3] → Cost of Equity (weekly)
```

---

## Rationale

- Avoids CAPM (no price-based beta)
- Rooted in first principles (MM leverage, capital structure)
- Credit spreads are less noisy than equity returns
- Works for both asset-heavy and asset-light businesses
