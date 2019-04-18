sep

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
    N1->N5[label="\\f"]
    N2->N6[label="\\r"]
    N3->N7[label="\\t"]
    N4->N8[label="\\v"]
    N9->N11[label="\\n"]
    N10->N12[label=" "]
    A->N9[label="ε"]
    A->N10[label="ε"]
    N11->B[label="ε"]
    N12->B[label="ε"]
}
```


\]

```viz
digraph G {
    rankdir = LR;
    node [shape=point,color=white,fontcolor=white]; I[label="init"];
    node [shape=doublecircle,color=black,fontcolor=black]; B[label="end"];
    node[shape=circle]; A[label="start"];N1,N2;
    I->A
    A->N1[label="ε"]
    A->N4[label="ε"]
    N1->N2[label=":"]
    N2->N3[label=">"]
    N4->N5[label="]"]
    N5->B[label="ε"]
    N3->B[label="ε"]
}
```


">>="

```viz
digraph G {
    rankdir = LR;
    node [shape=point,color=white,fontcolor=white]; I[label="init"];
    node [shape=doublecircle,color=black,fontcolor=black]; B[label="end"];
    node[shape=circle]; A[label="start"];N1,N2;
    I->A
    A->N1[label=">"]
    N1->N2[label=">"]
    N2->B[label="="]
    
}
```


auto

```viz
digraph G {
    rankdir = LR;
    node [shape=point,color=white,fontcolor=white]; I[label="init"];
    node [shape=doublecircle,color=black,fontcolor=black]; B[label="end"];
    node[shape=circle]; A[label="start"];N1,N2,N3;
    I->A
    A->N1[label="a"]
    N1->N2[label="u"]
    N2->N3[label="t"]
    N3->B[label="o"]
    
}
```

id

```viz
digraph G {
    subgraph cluster_0 {
        node[style=filled]; A[label=start];N1;
        A->N1[color=lightgrey,style=dotted,label="L"]
        color=lightgrey;
    }

    subgraph cluster_1 {
        node[style=filled]; N2,N4;
        N2->N4[color=lightgrey,style=dotted,label="L"]
        color=lightgrey;
    }

    subgraph cluster_2 {
        node[style=filled]; N3,N5;
        N3->N5[color=lightgrey,style=dotted,label="D"]
        color=lightgrey;
    }

    rankdir = LR;
    node [shape=point,color=white,fontcolor=white]; I[label="init"];
    node [shape=doublecircle,color=black,fontcolor=black]; B[label="end"];
    node[shape=circle]; A[label="start"];N0,N1,N6;
    I->A
    N0->N2[label="ε"]
    N0->N3[label="ε"]
    N4->N6[label="ε"]
    N5->N6[label="ε"]
    N1->N0[label="ε"]
    N6->B[label="ε"]
    N6->N0[label="ε"]
    N1->B[label="ε"]
}
```