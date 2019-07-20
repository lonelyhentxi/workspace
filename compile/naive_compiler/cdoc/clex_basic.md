D

```viz
digraph G {
    rankdir = LR;
    node [shape=point,color=white,fontcolor=white]; I[label="init"];
    node [shape=doublecircle,color=black,fontcolor=black]; B[label="end"];
    node[shape=circle]; A[label="start"];
    I->A
    A->B[label="0-9"]
    
}
```

H

```viz
digraph G {
    rankdir = LR;
    node [shape=point,color=white,fontcolor=white]; I[label="init"];
    node [shape=doublecircle,color=black,fontcolor=black]; B[label="end"];
    node[shape=circle]; A[label="start"];
    I->A
    A->B[label="a-fA-F0-9"]
}
```

L

```viz
digraph G {
    rankdir = LR;
    node [shape=point,color=white,fontcolor=white]; I[label="init"];
    node [shape=doublecircle,color=black,fontcolor=black]; B[label="end"];
    node[shape=circle]; A[label="start"];
    I->A
    A->B[label="a-zA-Z_"]
}
```

E:

```viz
digraph G {
    
    subgraph cluster_0 {
        node[style=filled]; N3,N4;
        N3->N4[color=lightgrey,style=dotted,label="D"]
        color=lightgrey;
    }
    rankdir = LR;
    node [shape=point,color=white,fontcolor=white]; I[label="init"];
    node [shape=doublecircle,color=black,fontcolor=black]; B[label="end"];
    node[shape=circle]; A[label="start"]; N1,N2;
    I->A
    A->N1[label="eE"]
    N1->N2[label="ε"]
    N1->N2[label="+-"]
    N2->N3[label="ε"]
    N4->B[label="ε"]
    N4->N3[label="ε"]
}
```

FS

```viz
digraph G {
    
    rankdir = LR;
    node [shape=point,color=white,fontcolor=white]; I[label="init"];
    node [shape=doublecircle,color=black,fontcolor=black]; B[label="end"];
    node[shape=circle]; A[label="start"]; N1,N2,N3,N4,N5,N6,N7,N8;
    I->A
    A->N1[label="ε"]
    A->N2[label="ε"]
    A->N3[label="ε"]
    A->N4[label="ε"]
    N5->B[label="ε"]
    N6->B[label="ε"]
    N7->B[label="ε"]
    N8->B[label="ε"]
    N1->N5[label="f"]
    N2->N6[label="F"]
    N3->N7[label="l"]
    N4->N8[label="L"]
}
```

IS

```viz
digraph G {
    
    rankdir = LR;
    node [shape=point,color=white,fontcolor=white]; I[label="init"];
    node [shape=doublecircle,color=black,fontcolor=black]; B1[label="end"];
    node[shape=circle]; A1[label="start"]; N1,N2,N3,N4,N5,N6,N7,N8; A[label="N0"]; B[label="N9"];
    A1->A[label="ε"]
    I->A1
    A->N1[label="ε"]
    A->N2[label="ε"]
    A->N3[label="ε"]
    A->N4[label="ε"]
    N5->B[label="ε"]
    N6->B[label="ε"]
    N7->B[label="ε"]
    N8->B[label="ε"]
    N1->N5[label="u"]
    N2->N6[label="U"]
    N3->N7[label="l"]
    N4->N8[label="L"]
    B->B1[label="ε"]
    B->A[label="ε"]
    A1->B1[label="ε"]
}
```