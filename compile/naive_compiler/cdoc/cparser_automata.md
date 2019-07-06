HEX:

```viz
digraph G {
    
    subgraph cluster_0 {
        node[style=filled]; N2,N3;
        N2->N3[color=lightgrey,style=dotted,label="H"]
        color=lightgrey;
    }

    subgraph cluster_1 {
        node[style=filled]; N5,N6;
        N5->N6[color=lightgrey,style=dotted,label="IS"]
        color=lightgrey;
    }

    rankdir = LR;
    node [shape=point,color=white,fontcolor=white]; I[label="init"];
    node [shape=doublecircle,color=black,fontcolor=black]; B[label="end"];
    node[shape=circle]; A[label="start"];N0, N1,N2;
    I->A
    A->N0[label="0"]
    N0->N1[label="xX"]
    N1->N2[label="ε"]
    N3->N2[label="ε"]
    N6->B[label="ε"]
    N4->N5[label="ε"]
    N3->N4[label="ε"]
    N4->B[label="ε"]
}
```