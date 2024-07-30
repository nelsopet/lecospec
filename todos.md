# Tasks for Paper

## Deep Learning
- [ ] Add logging/metrics to the training run
- [ ] Re-run Jigsaw with new logging and more epochs
- [ ] Run resnet model
- [ ] Run linear feedforward model
- [ ] Compare results
- [ ] Add results to paper


## Other
- [ ] Add something about different seeds/random effects 
    - [ ] Effect of seed related to differences in class size (samples per class)
    - [ ] 5 runs of random forest with different seeds, and see the differences
    - [ ] Figure showing effect of seed: accuracy line with 
        - possible response: should oversample instead of undersampling
    - [ ] seed was thought to be important; decided only important with class imbalance.  
    - [ ] May have something from before
    - [ ] should be able to modify the gs3 random forest script; should loop over seeds setting globally.
- [ ] Bias-variance tradeoff 
    - [ ] effectively pareto-optimality: r^2 and accuracy
- [ ] Tradeoff of model complexity, overfitting, etc.
- [ ] Quantitative discussion of sources of error
- [ ] Small model issue (2-4 trees too few)
    - [ ] Should look at the log files (how far in)
    - [ ] Adaboost typically has far fewer weak learners than random forest (CART is better than vanilla decision trees)
    - [ ] Reply to run longer: should be okay
        - [ ] instability goes down with balanced sample size, and 
        - [ ] smaller models have more variance, but that's worth it to avoid systematic bias
        - [ ] intertwined with seed issue; 
        - [ ] can generate a variety of small models and check stability (with balanced samples)
        - [ ] What about the dataset produces this phenomenon?
        - [ ] Can demonstrate stability of small models; compare to large models and associated r^2; basically demonstrate that smalle model trade-offs (higher variance) is worth it
        - [ ] demonstrate depth is needed 
        - Given high imbalance in variable importance, wide and shallow leads to more duplicated trees?



Note to self: We could look into this - can we simulate this based on variable importance?

Random Seeds
6265
4041
3621
942
3143
9017
1764
1378
3964
4270
5542
1816
2833
4024
3031
6389
1368
4900
4075
6232
7118
7590
7928
2725
2422
7475
3857
1652
2041
6447