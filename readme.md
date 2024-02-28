# Intro
1. This is code of paper:
> Liguang Wang#, Yujia Wang#, Yi Fu, Yunge Gao, Jiawei Du, Chen Yang, and Jianxiao Liu*. “AFSBN: A Method of Artificial Fish Swarm Optimizing Bayesian Network for Epistasis Detection”, IEEE/ACM Transactions on Computational Biology and Bioinformatics, vol. 18, no. 4 (2019): 1369-1383.
2. Paper link: https://ieeexplore.ieee.org/document/8884123/
3. Special thanks to Yang Guo and `bnlearn`. 

# How to run
1. `hill-climbing.R` is the most important file. All other files (except for `argSet.R`) are called here. It is based on `hill-climbing` designed in `bnlearn` pkg. So we keep the name.
2. `distance.R` is for distance calculation.
3. `follow.R`, `prey.R`, `swarm.R` are different behavior of fish.
4. `bulletin.R` is for saving and comparing the fish with the max score.
5. `argSet.R` is arguments we use.