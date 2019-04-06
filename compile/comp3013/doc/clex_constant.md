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

OCT：

```viz
digraph G {
    
    subgraph cluster_0 {
        node[style=filled]; N2,N3;
        N2->N3[color=lightgrey,style=dotted,label="D"]
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
    node[shape=circle]; A[label="start"];N0,N2;
    I->A
    A->N0[label="0"]
    N0->N2[label="ε"]
    N3->N2[label="ε"]
    N6->B[label="ε"]
    N4->N5[label="ε"]
    N3->N4[label="ε"]
    N4->B[label="ε"]
}
```

DEC:

```viz
digraph G {
    
    subgraph cluster_0 {
        node[style=filled]; N2,N3;
        N2->N3[color=lightgrey,style=dotted,label="D"]
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
    node[shape=circle]; A[label="start"];N2;
    I->A
    A->N2[label="ε"]
    N3->N2[label="ε"]
    N6->B[label="ε"]
    N4->N5[label="ε"]
    N3->N4[label="ε"]
    N4->B[label="ε"]
}
```

CLC:

```viz
digraph G {


    subgraph cluster_2 {
        node[style=filled]; N0,N1;
        N0->N1[color=lightgrey,style=dotted,label="L"]
        color=lightgrey;
    }

    rankdir = LR;
    node [shape=point,color=white,fontcolor=white]; I[label="init"];
    node [shape=doublecircle,color=black,fontcolor=black]; B[label="end"];
    node[shape=circle]; A[label="start"];N0, N1,N2,N3;
    I->A
    A->N0[label="ε"]
    N1->N0[label="ε"]
    N1->N2[label="\'"]
    N2->N3[label="ε"]
    N3->N4[label="\\"]
    N4->N5[label="#any"]
    N5->N6[label="ε"]
    N6->B[label="\'"]
    N2->N7[label="ε"]
    N7->N8[label="#noneof (\\')"]
    N8->N6[label="ε"]
    N6->N2[label="ε"]
}
```

SLC:

```viz
digraph G {


    subgraph cluster_2 {
        node[style=filled]; N0,N1;
        N0->N1[color=lightgrey,style=dotted,label="L"]
        color=lightgrey;
    }

    rankdir = LR;
    node [shape=point,color=white,fontcolor=white]; I[label="init"];
    node [shape=doublecircle,color=black,fontcolor=black]; B[label="end"];
    node[shape=circle]; A[label="start"];N0, N1,N2,N3;
    I->A
    A->N0[label="ε"]
    N1->N0[label="ε"]
    N2->N21[label="ε"]
    N1->N2[label="\""]
    N21->N3[label="ε"]
    N3->N4[label="\\"]
    N4->N5[label="#any"]
    N5->N6[label="ε"]
    N9->B[label="\""]
    N21->N7[label="ε"]
    N7->N8[label="#noneof (\\\")"]
    N8->N6[label="ε"]
    N6->N21[label="ε"]
    N6->N9[label="ε"]
    N2->N9[label="ε"]
}
```

DEFS

```viz
digraph G {
    
    subgraph cluster_0 {
        node[style=filled]; N2,N3;
        N2->N3[color=lightgrey,style=dotted,label="D"]
        color=lightgrey;
    }

    subgraph cluster_1 {
        node[style=filled]; N4,N5;
        N4->N5[color=lightgrey,style=dotted,label="E"]
        color=lightgrey;
    }

    subgraph cluster_2 {
        node[style=filled]; N5,N6;
        N6->N7[color=lightgrey,style=dotted,label="FS"]
        color=lightgrey;
    }

    rankdir = LR;
    node [shape=point,color=white,fontcolor=white]; I[label="init"];
    node [shape=doublecircle,color=black,fontcolor=black]; B[label="end"];
    node[shape=circle]; A[label="start"];N2;
    I->A
    A->N2[label="ε"]
    N3->N2[label="ε"]
    N7->B[label="ε"]
    N5->N9[label="ε"]
    N9->N6[label="ε"]
    N3->N4[label="ε"]
    N9->B[label="ε"]
}
```


RDEFS

```viz
digraph G {
    rankdir = LR;
    subgraph cluster_0 {
        node[style=filled]; N2,N3;
        N2->N3[color=lightgrey,style=dotted,label="D"]
        color=lightgrey;
    }

    subgraph cluster_3 {
        node[style=filled]; N10,N11;
        N10->N11[color=lightgrey,style=dotted,label="D"]
        color=lightgrey;
    }

    subgraph cluster_1 {
        node[style=filled]; N4,N5;
        N4->N5[color=lightgrey,style=dotted,label="E"]
        color=lightgrey;
    }

    subgraph cluster_2 {
        node[style=filled]; N5,N6;
        N6->N7[color=lightgrey,style=dotted,label="FS"]
        color=lightgrey;
    }

    node [shape=point,color=white,fontcolor=white]; I[label="init"];
    node [shape=doublecircle,color=black,fontcolor=black]; B[label="end"];
    node[shape=circle]; A[label="start"];N2;
    I->A
    A->N2[label="ε"]
    N3->N2[label="ε"]
    N3->N31[label="ε"]
    N31->N32[label="."]
    N7->B[label="ε"]
    N5->N9[label="ε"]
    N9->N6[label="ε"]
    N32->N10[label="ε"]
    N9->B[label="ε"]
    N11->N12[label="ε"]
    N12->N4[label="ε"]
    N12->N9[label="ε"]
    N11->N10[label="ε"]
    A->N31[label="ε"]
}
```

```viz
digraph G {
    rankdir = LR;
    subgraph cluster_0 {
        node[style=filled]; N2,N3;
        N2->N3[color=lightgrey,style=dotted,label="D"]
        color=lightgrey;
    }

    subgraph cluster_3 {
        node[style=filled]; N10,N11;
        N10->N11[color=lightgrey,style=dotted,label="D"]
        color=lightgrey;
    }

    subgraph cluster_1 {
        node[style=filled]; N4,N5;
        N4->N5[color=lightgrey,style=dotted,label="E"]
        color=lightgrey;
    }

    subgraph cluster_2 {
        node[style=filled]; N5,N6;
        N6->N7[color=lightgrey,style=dotted,label="FS"]
        color=lightgrey;
    }

    node [shape=point,color=white,fontcolor=white]; I[label="init"];
    node [shape=doublecircle,color=black,fontcolor=black]; B[label="end"];
    node[shape=circle]; A[label="start"];N2;
    I->A
    A->N2[label="ε"]
    N3->N2[label="ε"]
    N3->N31[label="ε"]
    N31->N32[label="."]
    N7->B[label="ε"]
    N5->N9[label="ε"]
    N9->N6[label="ε"]
    N32->N10[label="ε"]
    N9->B[label="ε"]
    N11->N12[label="ε"]
    N12->N4[label="ε"]
    N12->N9[label="ε"]
    N11->N10[label="ε"]
    N32->N12[label="ε"]
}
```