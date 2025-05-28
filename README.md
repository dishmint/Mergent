# Mergent

Consolidate expressions with the same head.

---

Deconstruct an expression:

```wolfram
In[1]:= DeconstructExpression[head[a,b,c]]

Out[1]= <|head-><|a->{1},b->{2},c->{3}|>|>
```

Reconstruct a deconstructed expression:

```wolfram
In[2]:= decon=DeconstructExpression[head[a,b,c]]

Out[2]= <|head-><|a->{1},b->{2},c->{3}|>|>


In[3]:= ReconstructExpression[decon]

Out[3]= head[a,b,c]
```

Merge expressions:

```wolfram
In[4]:= MergeExpression[head[a,b,c],head[d,e,f]]

Out[4]= {head[{a,d},{b,e},{c,f}]}
```
