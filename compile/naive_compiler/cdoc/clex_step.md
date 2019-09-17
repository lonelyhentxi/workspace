```viz
digraph G {
    node[shape=circle]; 
        R[label="ANSI c \n regex\n gramma"];
        LI[label="constraint\n info"];
        P[label="lex regex\n patterns"]
        RI[label="rank info"]
        TT[label="type\n table"]
        ARGS[label="cmd\n args"]
        IS[label="input\n stream"]
        T[label="tokens"]
        IT[label="ident\n table"]
        OS[label="out\n stream"]
    node[shape=rec]; 
        RE[label="regex engine"];
        LE[label="lex engine"]
        AP[label="args\n parser"]
        FS[label="filesystem"]
        MM[label="match\n machine"]
    R -> RE[label="input"]
    LI -> RE[label="input"]
    RE -> P[label="output"]
    RI -> LE[label="input"]
    P -> LE[label="input"]
    LE -> MM[label="build"]
    LE -> TT[label="register"]
    ARGS -> AP[label="parse"]
    AP -> FS[label="pass"]
    FS -> IS[label="read"]
    IS -> MM[label="read"]
    MM -> T[label="yield"]
    TT -> IT
    T -> IT
    IT -> T
    IT -> OS[label="write"]
    T -> OS[label="write"]
    TT -> OS[label="write"]
    OS -> FS[label="write"]

}
```