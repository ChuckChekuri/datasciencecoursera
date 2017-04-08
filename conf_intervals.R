mean.diff = 3 - 5
df = 10 + 10 - 2
pooled.var = (0.68 * 9 + 0.60 * 9) / df
se.diff = sqrt(pooled.var/10 + pooled.var/10)
t.obt = mean.diff / se.diff
t.obt
p.value = 2 * pt(abs(t.obt), df=df)        # two-tailed
p.value
# sqrt(n) * ( X' - mu) / s > Z_{1-alpha}.
#Our test statistic is (X'-mu) / (s/sqrt(n)) 
]