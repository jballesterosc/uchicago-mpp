# Problem Set 3 Quiz Study Guide
PPHA 30545: Bootstrap, Cross-Validation, and Subset Selection

The quiz covers what you just turned in: Question 1 (bootstrap), Question 2 (LOOCV on simulated data), and Question 3 (best subset, forward, and backward selection on Boston). It's in person with no AI, so practice answering in your own words.

Slide numbers are the ones printed at the bottom of each slide. "R" means the Resampling deck and "MS" means the Model Selection & Regularization deck.

How to use this: read a section, then cover the answers and try the practice questions yourself.

---

## If you only have 30 minutes

1. Section 0, how to phrase answers for full credit.
2. Section 2, the comparison table of the validation set approach, LOOCV, and k-fold CV, plus why the seed doesn't matter for LOOCV.
3. Section 4, counting models (2^p versus 1 + p(p+1)/2) and why forward and backward can miss the best model.
4. Practice questions A6, B3, C5, D4, and D7. These are the trickiest ones.

---

## 0. How to write answers that get full credit

These habits come from how your earlier quizzes were graded.

1. Put the answer first, in plain words: "Yes." "No." "Model 2." "Best subset." Rubrics take big deductions when the choice isn't written out.
2. Follow it with one "because" sentence. A correct answer with no reason rarely gets full points.
3. For significance, say both parts: "p = 0.023 < 0.05, so we reject H₀ that β₄ = 0."
4. Check the direction before you write. Lower MSE, RMSE, AIC, and BIC are better. Higher adjusted R² is better. On an earlier quiz you cited a lower adjusted R² as support for the wrong model.
5. Make sure your words match your numbers. If you write "Model A is better," look again at the number you're citing.
6. Never leave a box blank. A partly right sentence can earn partial credit, and a blank earns nothing.
7. Don't overclaim tiny differences. An AIC gap of 0.4 or an MSE gap of 0.08 means the models are essentially tied. Say so.

---

## 1. The bootstrap (Question 1)

### The idea in plain words

You have one sample, but you'd like to know how much a coefficient would bounce around if you could collect many samples. You can't collect more, so you fake it. You draw new samples of the same size from your own data, with replacement, re-estimate the coefficient each time, and measure how spread out those estimates are. That spread is the bootstrap standard error. (R slides 55 to 70.)

### The algorithm (R slide 70)

1. Start with your one sample of size n.
2. Draw a bootstrap sample of size n with replacement.
3. Compute the estimate (for example, β̂ for balance) on that sample.
4. Repeat steps 2 and 3 B times. You used B = 1,000.
5. SE = the standard deviation of the B estimates, dividing by B − 1.

SE_B = √[ (1/(B−1)) Σ (β̂*_b − β̄*)² ]

### Why "with replacement" matters

If you drew n observations without replacement from n, you'd get the same data back every time, just in a different order. Every estimate would be identical and the SE would be zero. Replacement is what makes each bootstrap sample different: some rows show up twice and some not at all.

### Your results

| Coefficient | GLM formula SE | Bootstrap SE |
|---|---|---|
| balance | 0.000227 | 0.000229 |
| income | 0.00000499 | 0.00000496 |

They're very close. The GLM formula relies on the logistic model's assumptions being right, and the bootstrap doesn't rely on them. When two methods with different assumptions agree, that suggests the formula's assumptions are reasonable here, helped by the large sample (n = 10,000).

They could disagree if the model were misspecified (for example, the true relationship isn't logistic in balance and income) or if the sample were small, so the large-sample formula is a poor approximation.

### Code you should be able to read

```python
rng = np.random.default_rng(23)
idx = rng.choice(df_default.index, size=len(df_default), replace=True)
boot_fn(df_default, idx)          # coefficients from one bootstrap sample
boot_coefs.std(axis=0, ddof=1)    # SE; ddof=1 means divide by B − 1
```

`.loc[index]` in `boot_fn` is what lets duplicated row labels pull the same row more than once.

---

## 2. Validation methods and the seed (Questions 2c and 2d)

### Three ways to estimate test error

The validation set approach splits the data once, at random, into a training set and a validation set. You fit on one part and measure error on the other.

LOOCV leaves out one observation, fits on the other n − 1, predicts the one left out, and repeats until every observation has been left out once. It does n fits and averages the n errors.

k-fold CV splits the data at random into k folds. It leaves out one fold, fits on the rest, and repeats k times. The usual choices are k = 5 or k = 10 (R slide 37).

### The comparison table (R slide 34)

| | Best | Middle | Worst |
|---|---|---|---|
| Bias (overstating test error) | LOOCV | k-fold | Validation set |
| Noise from the random split | LOOCV (none) | k-fold | Validation set |
| Computational cost | Validation set | k-fold | LOOCV |
| Variance | k-fold | LOOCV? | Validation set? |

Why LOOCV has low bias: each fit uses n − 1 observations, almost the full data, so the fitted model is nearly as good as one fit on all n.

Why LOOCV can have high variance (R slide 29): the n training sets overlap almost completely, so the n estimates are highly correlated. Averaging correlated numbers doesn't reduce variance much.

Picking k is a bias-variance tradeoff (R slide 37). A bigger k means less bias and more variance.

### Why the seed doesn't change LOOCV

A seed only fixes the outcome of random draws. LOOCV has no random draws: every observation gets left out exactly once, in a fixed order. Same data plus same splits gives the same error, whatever the seed. The slides list this as a LOOCV advantage: it isn't noisy, because the estimate is always the same (R slide 29).

Compare k-fold with `shuffle=True`. Which rows land in which fold is random, so a different `random_state` gives different folds and a slightly different CV error.

The trap from your pset: setting a seed and generating new data are separate steps. Writing `rng = np.random.default_rng(10)` and then `x = rng.normal(...)` draws a brand-new data set. The numbers change because the data changed, not because LOOCV depends on the seed.

### Why not just use training error? (R slide 10, MS slide 26)

Training error almost always underestimates test error, and it never gets worse when you add flexibility. Picking a model by training error leads you to choose the most flexible model, which overfits.

### Reading `cross_val_score` output

`scoring="neg_mean_squared_error"` returns negative numbers, because sklearn always treats bigger as better. Flip the sign to get MSE. The best model has the value closest to zero, which becomes the smallest MSE once you flip it.

---

## 3. Polynomial models on simulated data (Question 2)

### The setup

y = x − 2x² + ε, with x ~ N(0, 1) and ε ~ N(0, 1), so n = 100 and p = 1. x² is calculated from x, so it isn't a separate variable in the data. The scatterplot is an upside-down parabola, with its peak near x = 0.25 (where the derivative 1 − 4x equals 0).

A polynomial model is still a linear model, because it's linear in the β's. You just add x², x³, and so on as extra columns (ISLP Section 3.3.2).

### Your results

| Model | LOOCV MSE | Significant terms (at 5%) |
|---|---|---|
| 1. Linear | 6.633 | X |
| 2. Quadratic | 1.123 (lowest) | X, X² |
| 3. Cubic | 1.302 | X, X² (X³ has p = 0.29) |
| 4. Quartic | 1.332 | X, X², and X⁴ (p = 0.023); X³ has p = 0.64 |

The quadratic wins, which is what you'd expect, because the true model is quadratic. The linear model underfits (high bias). The cubic and quartic terms can't capture anything real beyond the quadratic, but they add variance, so the LOOCV error rises a little.

### The X⁴ surprise

X⁴ comes out significant at the 5% level even though its true coefficient is 0. That's a Type I error, a false positive. At α = 0.05, each truly-zero term has a 5% chance of looking significant by luck, and you tested several. The two methods mostly agree: both point to the quadratic as the right model, and LOOCV shows X⁴ doesn't help prediction, because the error goes up from 1.12 to 1.33.

---

## 4. Subset selection (Question 3)

### The dog show idea (MS slide 14)

You don't compare every dog against every other dog at once. First you pick the best in each class (the best 1-variable model, the best 2-variable model, and so on). Then you pick the best in show among those class winners.

### The three methods

Best subset (MS slide 18) fits every combination of k predictors for each k, keeps the best one at each size, then picks among those. It's guaranteed to find the best model of each size, but it's slow.

Forward stepwise (MS slide 32) starts with no predictors. At each step it adds whichever predictor improves the fit most, and once a predictor is in, it stays in.

Backward stepwise (MS slides 39 and 40) starts with all p predictors and removes the least useful one at each step. Once a predictor is out, it stays out.

Both stepwise methods are greedy: they make the best single move at each step and can't undo it. That's why they can end up on a different path from best subset and miss the best model (MS slide 37).

### Counting models (MS slides 30 and 36)

| p | Best subset (2^p) | Stepwise (1 + p(p+1)/2) |
|---|---|---|
| 4 | 16 | 11 |
| 10 | 1,024 | 56 |
| 13 (your Boston data) | 8,192 | 92 |
| 20 | 1,048,576 | 211 |

The counts include the null model with no predictors. Your best-subset run fit 8,191 models, because it started at 1 feature.

Two more cons of best subset: it's slow, and the huge search space makes it more likely to find a model that fits the training data well by luck (MS slide 30).

### Backward versus forward when p > n (MS slide 40)

Backward can't be used when p > n, because it starts from the full model, and OLS can't be fit with more predictors than observations (there's no unique solution). Forward can still work, because it starts small.

### How to score models (MS slides 22 and 26)

Within one size, RSS or R² is fine, since all the models have the same number of predictors. Across sizes, RSS and R² always favor the biggest model, because they're training-error measures. So the final comparison uses CV or a penalized measure: adjusted R², Cp, AIC, or BIC.

The textbook approach uses RSS within each size and then Cp, AIC, BIC, or adjusted R² for the final pick. sklearn's `SequentialFeatureSelector` uses k-fold CV for every comparison. mlxtend's `ExhaustiveFeatureSelector`, as you used it, also scored every combination with CV.

For non-OLS models like logistic regression, use deviance = −2 ln(L̂) instead of RSS (MS slide 41).

### AIC versus CV

AIC is a formula: a fit term plus a fixed penalty of 2 per parameter. It relies on assumptions (a correctly specified model, normal errors, large-sample approximations). BIC works the same way, with a penalty of ln(n) per parameter. With n = 506, ln(506) ≈ 6.2, which is bigger than 2, so BIC prefers smaller models than AIC does.

CV estimates test error directly from the data. It needs no distributional assumptions, but it depends on the random fold split, and it costs more computation.

Because they estimate test error in different ways and penalize complexity differently, AIC and CV can rank models differently. That happened in your results.

### Your results

| Method | Predictors | CV MSE | CV RMSE | AIC | Time |
|---|---|---|---|---|---|
| Full model (baseline) | 13 | 43.31 | | | |
| Best subset | 8 | 42.80 | 6.542 | 3334.78 | 10.4 s (parallel) |
| Forward | 10 | 42.88 | 6.548 | 3334.38 | 2.7 s |
| Backward | 8 (same as best subset) | 42.80 | 6.542 | 3334.78 | 2.9 s |

CV slightly prefers best subset and backward, while AIC slightly prefers forward. Both gaps are tiny (an AIC difference under 2 is generally treated as no real difference), so the models are essentially tied. You chose the 8-predictor model because it's simpler and wins on CV.

The 8-predictor model drops CHAS, RM, AGE, TAX, and B. That doesn't mean they're unrelated to crime. TAX has a 0.58 correlation with CRIM, but it's 0.91 correlated with RAD, which is already in the model, so it adds little new information.

### One-standard-error rule (R slide 45)

When several models have similar CV error, pick the simplest one whose error is within one standard error of the minimum.

---

## 5. Code traps you hit in this pset

1. `KFold` doesn't accept `scoring`. KFold only cuts the folds, and `scoring` goes to `cross_val_score` or the selector.
2. `random_state=42` versus `24`. Read the question's numbers carefully.
3. Redrawing `x` and `y` after setting a new seed replaces your original data.
4. `neg_root_mean_squared_error` averages the RMSE across folds, which isn't the same as √(CV MSE), and the two can rank models differently.
5. Hidden notebook state, like the phantom `CONSTANT` column. Always restart the kernel and run everything before trusting your outputs.
6. sklearn's `SequentialFeatureSelector` with an integer `n_features_to_select` needs a number smaller than p, so the full model has to be added separately.

---

## Practice questions

Cover the answers and try each one first.

### A. Bootstrap

A1. Why must bootstrap samples be drawn with replacement?
> Without replacement, a sample of size n from n observations is just the original data in a new order, so every estimate would be identical and the SE would be 0. Replacement makes each bootstrap sample different.

A2. Four bootstrap estimates of a coefficient are 1.2, 1.5, 0.9, and 1.4. What is the bootstrap SE?
> The mean is 1.25. The squared deviations are 0.0025, 0.0625, 0.1225, and 0.0225, which sum to 0.21. Dividing by B − 1 = 3 gives 0.07, and √0.07 ≈ 0.265.

A3. Give one situation where the bootstrap SE and the formula SE could differ a lot.
> When the model's assumptions don't hold (for example, the model is misspecified), or when the sample is small, so the large-sample formula is a poor approximation. The bootstrap doesn't rely on those assumptions.

A4. What does `ddof=1` do in `boot_coefs.std(axis=0, ddof=1)`?
> It divides by B − 1 instead of B, which matches the bootstrap SE formula on R slide 70.

A5. In a bootstrap sample of size n = 10,000, what's the probability that a particular observation is not included at all?
> (1 − 1/n)^n ≈ 0.368. For large n this approaches e⁻¹, so roughly a third of the original observations are missing from each bootstrap sample.

A6. Would raising B from 1,000 to 10,000 make the bootstrap SE much smaller?
> No. More bootstrap samples make your estimate of the SE more precise (less simulation noise), but the SE itself reflects how much the coefficient varies across samples of size n. Only a larger n would shrink it.

A7. In one sentence, what does the bootstrap estimate in Question 1?
> It estimates the standard errors of the balance and income coefficients by measuring how much those coefficients vary across 1,000 resamples of the data.

### B. Cross-validation

B1. With n = 100, how many models does LOOCV fit? How many does 5-fold CV fit?
> LOOCV fits 100, one per left-out observation. 5-fold fits 5.

B2. Rank the validation set approach, LOOCV, and k-fold CV from least to most biased.
> LOOCV, then k-fold, then the validation set approach. Less training data per fit means a worse fit, which means the test error gets overstated more.

B3. You run LOOCV with seed 10 and get an MSE of 1.123. You rerun it with seed 50. What do you get, and why?
> Exactly 1.123. LOOCV leaves out each observation once in a fixed order, so there's no random step for the seed to affect.

B4. You run 5-fold CV with `shuffle=True, random_state=24`, then again with `random_state=25`. Same results?
> No, though probably similar. The shuffle randomly assigns rows to folds, so a different random state gives different folds and a slightly different error.

B5. Why does LOOCV tend to have higher variance than 10-fold CV?
> LOOCV's n training sets overlap almost completely, so its n error estimates are highly correlated, and averaging correlated estimates doesn't reduce variance much. 10-fold's training sets overlap less.

B6. `cross_val_score` returns [−40, −45, −44, −42, −39]. What is the CV MSE? What is the RMSE if you take the square root of the CV MSE?
> CV MSE = 42 (the mean is −42; flip the sign). RMSE = √42 ≈ 6.48.

B7. Why shouldn't you choose a model by its training MSE?
> Training MSE never increases when you add predictors or flexibility, because the model can use extra terms to fit noise. It would always pick the most complex model, which overfits.

B8. Why can't you use the test set to choose between models?
> Once you pick a winner based on its test error, that error is no longer an honest estimate. It's optimistic, because you selected the model that happened to do best on that particular test set.

### C. Polynomials and significance

C1. Data come from y = 3 + 2x − x³ + ε, with x of size 200. What are n and p? Among degree 1 to 4 polynomial models, which would you expect to have the lowest LOOCV error?
> n = 200 and p = 1 (x³ is computed from x). The cubic, since that's the true form. Lower degrees underfit, and the quartic adds variance without reducing bias.

C2. Is y = β₀ + β₁X + β₂X² + ε a linear model?
> Yes. It's linear in the coefficients β, with X² as an extra column, so ordinary least squares can fit it.

C3. The quartic model has a lower training MSE than the quadratic, but a higher LOOCV MSE. Explain.
> Training MSE can only go down when you add terms, because X³ and X⁴ can fit noise in this sample. LOOCV measures error on left-out points, where the fitted noise doesn't help, and the extra variance from the added terms makes predictions slightly worse.

C4. In the cubic model, X³ has p = 0.29. State your conclusion at the 5% level.
> p = 0.29 > 0.05, so we fail to reject H₀ that β₃ = 0. X³ is not statistically significant.

C5. In the quartic model, X⁴ has p = 0.023, but the true model has no X⁴ term. What happened?
> p = 0.023 < 0.05, so we reject H₀ that β₄ = 0, but since the true β₄ is 0, this is a Type I error (a false positive). At α = 0.05, each truly-zero term has a 5% chance of looking significant by luck. LOOCV shows X⁴ doesn't improve prediction, since the error rises from 1.12 to 1.33.

C6. Do the significance results and the LOOCV results agree in Question 2?
> Mostly yes. X and X² are significant in every model that includes them, and the quadratic has the lowest LOOCV error, so both point to the quadratic. The exception is X⁴'s false positive, which LOOCV shows doesn't help out of sample.

### D. Subset selection

D1. With p = 4, how many models does best subset consider? How many does forward stepwise consider?
> Best subset: 2⁴ = 16. Forward: 1 + 4(5)/2 = 11.

D2. With p = 13, as in Boston?
> Best subset: 2¹³ = 8,192. Stepwise: 1 + 13(14)/2 = 92.

D3. Which method is guaranteed to find the model with the lowest training RSS for each number of predictors?
> Best subset, because it checks every combination. Forward and backward are greedy and can miss it.

D4. Here are training RSS values with p = 3. One-variable models: X1 = 50, X2 = 60, X3 = 70. Two-variable models: {X1, X2} = 40, {X1, X3} = 45, {X2, X3} = 30. Which two-variable model does forward stepwise choose? Which does best subset choose?
> Forward picks X1 first (RSS 50), then can only add to X1, so it chooses {X1, X2} with RSS 40. Best subset chooses {X2, X3} with RSS 30. Forward misses it because it can't drop X1.

D5. You have 50 observations and 80 predictors. Can you use backward stepwise? Forward?
> Backward: no. It starts from the full 80-predictor model, which OLS can't fit with only 50 observations, because there's no unique solution. Forward: yes, since it starts with no predictors and only fits small models.

D6. Why can't you use R² to choose between a 5-predictor model and an 8-predictor model?
> R² always rises (or stays the same) when you add predictors, because it's a training-fit measure. It would always prefer the bigger model, even if the extra predictors only fit noise.

D7. Model A has AIC = 3334.38 and Model B has AIC = 3334.78. Which is better, and how much would you trust that ranking?
> Model A, because lower AIC is better. But the difference is only 0.4, well under the usual threshold of about 2, so the two are essentially tied, and you shouldn't claim A is meaningfully better.

D8. With n = 506, does BIC or AIC penalize an extra predictor more?
> BIC. Its penalty is ln(n) ≈ 6.2 per parameter, compared with 2 for AIC, so BIC tends to choose smaller models.

D9. Give two reasons 5-fold CV and AIC might rank the same models differently.
> AIC is a formula that assumes a correctly specified model with normal errors and uses a fixed penalty of 2 per parameter. CV estimates test error from the data with no distributional assumptions, but its result depends on the random fold split.

D10. In your results, best subset took about 10 seconds and the stepwise methods about 3, even though best subset ran on all CPU cores. Why?
> Best subset fit 8,191 models, each with 5-fold CV. The stepwise methods only consider a small number of models at each step. Running in parallel makes best subset look faster than it really is, so the true gap is even bigger.

D11. Your chosen model leaves out TAX. Does that mean TAX is unrelated to crime?
> No. TAX has a 0.58 correlation with CRIM. But it's 0.91 correlated with RAD, which is already in the model, so it adds little new information, and dropping it didn't raise the CV error.

D12. How would subset selection change for a logistic regression?
> The process is the same, but you rank models by deviance (−2 ln L̂) instead of RSS (MS slide 41).

### E. Code reading

E1. What's wrong with `KFold(5, shuffle=True, random_state=24, scoring="neg_mean_squared_error")`?
> KFold has no `scoring` argument, so this raises a TypeError. KFold only splits the data. Pass `scoring` to `cross_val_score` or the selector instead.

E2. Someone runs:
```python
rng = np.random.default_rng(1)
x = rng.normal(size=100); y = x - 2*x**2 + rng.normal(size=100)
rng = np.random.default_rng(10)
x = rng.normal(size=100); y = x - 2*x**2 + rng.normal(size=100)
```
then computes LOOCV. Is that LOOCV on the data from part (a)?
> No. The second pair of lines draws brand-new x and y, overwriting the seed-1 data, so the LOOCV is on a different data set.

E3. What does `rng.choice(df.index, size=len(df), replace=True)` produce?
> A list of n row labels drawn at random with replacement: one bootstrap sample's worth of row indices, with some rows repeated and some missing.

E4. `efs.best_score_` is −42.80. What is the CV MSE, and is a score of −43.31 better or worse?
> The CV MSE is 42.80. −43.31 is worse: it corresponds to a higher MSE of 43.31.

---

Before the quiz, spend 10 minutes skimming your lab notebook from this week too, especially the code patterns, since I don't have those materials.
