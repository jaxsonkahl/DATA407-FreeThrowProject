# COSC 407 Final Project  
## Simulation Study of Free Throw Percentage Estimation Using NBA Data

---

## Project Overview

This project studies how sample size affects the estimation of a basketball player's free throw percentage. Each free throw attempt is modeled as a Bernoulli trial with two outcomes: success or failure. Simulation methods generate repeated samples to examine the sampling distribution of the estimator of the free throw probability.

The analysis uses NBA player data obtained from Basketball Reference. The study focuses on Stephen Curry because his free throw percentage exceeds 0.90 and his career includes a large number of attempts. The simulation evaluates how the variability of the estimator changes as the number of observed free throws increases.

---

## Methods

The project applies several statistical techniques:

- Binomial modeling of free throw attempts   
- Comparison of simulated and theoretical estimator variance  
- Construction of 95 percent confidence intervals  
- Coverage rate analysis of confidence intervals  
- Bootstrap resampling to approximate the sampling distribution  

---

## Data Source

NBA player statistics were collected from:

Basketball Reference  
https://www.basketball-reference.com

---

## Repository Contents

- `COSC407FinalProjject.R`  
  R script containing the simulation code and analysis

- `PlayerData.txt`  
  Dataset containing NBA player free throw statistics

---


## Results

The simulation shows that small samples produce large variability in the estimated free throw percentage. As the sample size increases, the variance of the estimator decreases and confidence intervals become narrower. Coverage rates for confidence intervals approach the expected 95 percent level as the number of attempts increases.

These results demonstrate how sample size affects statistical reliability when estimating probabilities from observed data.
