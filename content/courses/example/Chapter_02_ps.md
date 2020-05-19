---
linktitle: "2"
draft: false
author: "Mitch"
date: "2020-04-26"
menu:
  example:
    parent: Problem Sets
    weight: 1
title: Chapter 2 problem set
toc: true
float_toc: true
type: docs
weight: 1
---



## R Markdown

## 2M1 

First we'll start off by defining our grid using seq(), and defining a uniform prior, just make sure the amount of times you repeat the prior is equal to the lenght.out.

Defining the likelihoods used the dbinom or density binomial distribution. We start by inputing how many observations were waters, size is equal to the total number of tosses, and prob is equal to our grid.

Comupting the posterior is the product of likelihood and prior divided by the sum of all those values.


```r
# Define the grid
p_grid <- seq(from=0, to=1, length.out = 1000)

# Assume a uniform prior 
prior1 <- rep(1, 1000)

# Get the likelihood given: 3 waters and 3 tosses
#                           3 waters and 4 tosses
#                           5 waters and 7 tosses
likelihood1 <- dbinom(3, size=3, prob=p_grid)
likelihood2 <- dbinom(3, size=4, prob=p_grid)
likelihood3 <- dbinom(5, size=7, prob=p_grid)

# Compute posterior
posterior1 <- likelihood1 * prior1
posterior2 <- likelihood2 * prior1
posterior3 <- likelihood3 * prior1

# Standardize
posterior1 <- posterior1 / sum(posterior1)
posterior2 <- posterior2 / sum(posterior2)
posterior3 <- posterior3 / sum(posterior3)

# Plotting the posteriors
plot(p_grid, posterior1, type = "b",
     xlab = "probability of water",
     ylab = "posterior probability")
lines(p_grid, posterior2, type = "b", col = "red")
lines(p_grid, posterior3, type = "b", col = "blue")
legend("topleft", 
       c("posterior1", "posterior2", "posterior3"),
       fill = c("black", "red", "blue"))
mtext("1000 points")
```

![](/img/statistical-rethinking/ps/Chapter_02_files/figure-html/unnamed-chunk-1-1.png)<!-- -->


## 2M2
This problem is identical to the one above, the only thing that's changed is that we have changed our prior to be 0 when p < 0.5 and 1 otherwise. This can all be done in r using the ifelse() function.

![](/img/statistical-rethinking/ps/Chapter_02_files/figure-html/pressure-1.png)<!-- -->


## 2M3
Since earth is 70% water then there's only a 30% or 0.3 chance that it will land on 'land'. Mars has a 100% or 1.0 chance of landing on 'land' if tossed. 

Below are two identical ways of solving the problem, the second was included as it might not be as obvious how the code in the first part works for people new to r.

```r
ways <- c(.3, 1)
probability <- ways/sum(ways)
probability[1]
```

```
## [1] 0.2307692
```

```r
# Alternative way
earth_land = 1-.7
mars_land = 1
probability_earth = earth_land/(earth_land + mars_land)
probability_earth
```

```
## [1] 0.2307692
```


## 2M4

For the following set of questions I'll be using [bb], [bw], and [ww] to refer to a card that is black on both, black on one side/ white on the other, and white on both sides respectively.

If [bb] is picked there's a 100% or 1.0 chance that when it's put on the table a black side is facing up.
If [bw] is picked it's a 50% or 0.5 chance.
If [ww] is picked there's a 0% chance that a black side can be facing up.

Another way of thinking about it is there are two ways a [bb] card can show a black side up, only one way a [bw] card can, and no ways for a [ww] card. 

```r
ways <- c(1, 0.5, 0)
probability <- ways/sum(ways)
probability[1]
```

```
## [1] 0.6666667
```

```r
# ways <- c(2, 1, 0)
# p <- ways/sum(ways)
# p[1]
```


## 2M5
Now there is an extra [bb] card. I like thinking about it in terms of percentages, so there are two cards that have a 100% chance of showing a black side face up, everything else is the same from 2M4.

Alternatively you can think of it as two ways a [bb] card show a black side face up, one way for a [bw] card, no ways for [ww], and again two ways for the second [bb] card. Since we can pick either of the [bb] cards we add their probabilities together to get the final result.


```r
ways <- c(2*1, 0.5, 0)
probability <- ways/sum(ways)
probability[1]
```

```
## [1] 0.8
```

```r
# ways <- c(2, 1, 0, 2)
# probability <- ways/sum(ways)
# sum(probability[1], probability[4])
```


## 2M6
Now we are informed that for every way we can pull [bb] there are two ways we can pull [bw] and three ways to pull [ww]. In the previous examples we were making the implicit assumption that there were equal chances of drawing each card meaning that our prior was 1 for each card. Now we need to adjust our probability given a new prior.


```r
# You could also use: ways <- c(2, 1, 0)
ways <- c(1, 0.5, 0)
prior <- c(1, 2, 3)
likelihood <- ways * prior 
probability <- likelihood/sum(likelihood)
probability
```

```
## [1] 0.5 0.5 0.0
```


## 2M7
To solve this problem we'll look at all the possible combinations of draws that can lead to a black side showing face up on the first draw followed by a white side showing up on the second. Since a [bb] card has two ways it can show a black side face up, I will introduce a new notation [Bb] and [bB] to represent the two different states of the same card. The same logic will be applied to [ww], [bw] need not change because there's only one way a [bw] card can show either black or white.


```r
x <- data.frame(c(rep("Bb", 3), rep("bB", 3), c("bw", "bw")), 
                c(rep(c("bw", "Ww", "wW"), 2), c("Ww", "wW")))
knitr::kable(x, align = 'cc', col.names = c("First Draw", "Second Draw"))
```



 First Draw    Second Draw 
------------  -------------
     Bb            bw      
     Bb            Ww      
     Bb            wW      
     bB            bw      
     bB            Ww      
     bB            wW      
     bw            Ww      
     bw            wW      

From this we can see that there are six ways a [bb] card can be picked first that would satisfy our observations, and only two ways for the [bw] card to be picked first. There are still no ways a [ww] could be picked first.

```r
# possibilities [Bb, bw], [Bb, Ww], [Bb, wW], [bw, Ww], [bw, wW]
#               [bB, bw], [bB, Ww], [bB, wW]

ways <- c(6, 2, 0)
probability <- ways/sum(ways)
probability[1]
```

```
## [1] 0.75
```


## 2H1
The relevant information here is that the probability of a panda giving birth to twins given that it's from species A is 10% and species B is 20%. Also, that both species of panda are equally common so. From this we know the following:

$$Pr(twins | A) = 0.1\\
Pr(twins | B) = 0.2\\
Pr(A) = Pr(B) = 0.5$$

Using Bayes Theorem we can find the following:

$$Pr(A | twins) = \frac{Pr(twins | A)*Pr(A)}{Pr(twins)}\\
Pr(A | twins) = \frac{0.1*0.5}{0.1*0.5+0.2*0.5}=\frac{1}{3}$$


```r
likelihood <- c(0.1, 0.2)
prior <- c(1,1)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
posterior[1]
```

```
## [1] 0.3333333
```


From this we can see that
$$Pr(B | twins) = 1 - Pr(A | twins) = \frac{2}{3}$$

Now we can calculate Pr(twins)

$$Pr(twins) = Pr(twins|A)*P(A) + Pr(twins|B)*Pr(B)\\
= 0.1*\frac{1}{3} + 0.2*\frac{2}{3}\\
= 0.1667$$


```r
0.1*posterior[1]+0.2*posterior[2]
```

```
## [1] 0.1666667
```


## 2H2
We actually already computed this in our solution above:
$$Pr(A | twins) = \frac{Pr(twins | A)*Pr(A)}{Pr(twins)}\\
Pr(A | twins) = \frac{0.1*0.5}{0.1*0.5+0.2*0.5}=\frac{1}{3}$$


```r
posterior[1]
```

```
## [1] 0.3333333
```



## 2H3
Since we know the probability of each species giving birth to twins, we can assume that the probability of giving birth to a singleton is just the complement of our previous probabilities. Therefore:
$$Pr(singleton|A) = 0.9\\
Pr(singleton | B) = 0.8$$

Since we're assuming that the first birth was from twins, we can reuse our probabilities for A and B.
$$Pr(A) = \frac{1}{3}\\
Pr(B) = \frac{2}{3}$$

Now we can use Bayes Theorem again to solve the problem:
$$Pr(A | singleton) = \frac{Pr(singleton|A)*Pr(A)}{Pr(singleton)} \\
= \frac{0.9*\frac{1}{3}}{0.9*\frac{1}{3} + 0.8*\frac{2}{3}} \\
= \frac{0.3}{0.3 + 0.8333} \\
= 0.36$$


```r
likelihood <- c(0.9, 0.8)
prior <- c(1/3, 2/3)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
posterior[1]
```

```
## [1] 0.36
```


## 2H4
I want to preface this solution, this question gave me some trouble and after going to check my answer I felt like there was a lack of consesus over how it should be done I'll do my best to explain my thought process and hopefully someone will be kind enough to give me some feedback . 

We know that the probability this test correctly identifies a panda from species A is 0.8, we also know that the probability this test correctly identifies a panda from species B is 0.65. I've included the following type error chart to help visualize the test.


```r
include_graphics("ch2_type_errors.png")
```

<img src="ch2_type_errors.png" width="561" />
We can interpret this as
$$Pr(test\space says \space A| A) = 0.8 \\
Pr(test \space says \space B | B) = 0.65$$

Since TP + FN = 1 and FP + TN = 1 we can see that

$$Pr(test\space says \space A| B) = 0.35 \\
Pr(test \space says \space B| A) = 0.2$$

I hope the chart above helps illustrate why this is the case. Now back to the question. We are first ignoring the previous information about births and computing the posterior probability that the panda is species A given the test tells us it's species A.

$$Pr(A| test \space says \space A) = \frac{Pr(test \space says \space A | A)*Pr(A)}{Pr(test \space says \space A)} \\
= \frac{Pr(test \space says \space A | A)*Pr(A)}{Pr(test \space says \space A|A)*Pr(A) + Pr(test \space says \space A|B)*Pr(B)} \\
= \frac{0.8*0.5}{0.8*0.5 + 0.35*0.5} \\
= \frac{0.4}{0.575} \\
= 0.696$$


```r
likelihood <- c(0.8, 0.35)
prior <- c(1, 1)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
posterior[1]
```

```
## [1] 0.6956522
```

Now we will use the birth data
$$Pr(twins | A) = 0.1\\
Pr(twins | B) = 0.2 \\
Pr(A|twins) = \frac{Pr(twins|A)*Pr(A)}{Pr(twins)}\\
=\frac{0.1*0.696}{0.1*0.696 + 0.2*(1-0.696)} \\
= 0.533$$


```r
likelihood <- c(0.1, 0.2)
prior <- c(0.696, 0.304)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
posterior[1]
```

```
## [1] 0.5337423
```

