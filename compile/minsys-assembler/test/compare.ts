const source1 = `3c01ffff,
343cf000,
8c0a0014,
8c0b0018,
8c0c001c,
8c0d0020,
8c0e0024,
8c0f0028,
8c15002c,
8f810c70,
00011342,
104a0016,
104b0005,
104c0009,
104d000d,
104e0015,
104f0019,
08000031,
20840001,
8c1d0000,
23bdffff,
17a0fffe,
08000031,
2484ffff,
8c1d0004,
23bdffff,
17a0fffe,
08000031,
00042040,
00952024,
8c1d0008,
23bdffff,
17a0fffe,
08000031,
8f810c70,
30241fff,
08000031,
00042042,
8c1d000c,
23bdffff,
17a0fffe,
08000031,
00042c00,
00052843,
00052402,
8c1d0010,
23bdffff,
17a0fffe,
08000031,
af840c60,
08000009,`;
const source2 = `3c01ffff, 343cf000, 8c0a0014, 8c0b0018, 8c0c001c, 8c0d0020, 8c0e0024, 8c0f0028, 8c15002c, 8f810c70, 00011342, 104a0016, 104b0005, 104c0009, 104d000d, 104e0015, 104f0019, 08000031, 20840001, 8c1d0000, 23bdffff, 17a0fffe, 08000031, 2484ffff, 8c1d0004, 23bdffff, 17a0fffe, 08000031, 00042040, 00952024, 8c1d0008, 23bdffff, 17a0fffe, 08000031, 8f810c70, 30241fff, 08000031, 00042042, 8c1d000c, 23bdffff, 17a0fffe, 08000031, 00042c00, 00052843, 00052402, 8c1d0010, 23bdffff, 17a0fffe, 08000031, af840c60, 08000009`;

function handle(content:string) {
    return content.split(/[\s*,;]/g).map(val=>val.trim().toLowerCase()).filter(val=>val!=='');
}

function test(source1:string[],source2:string[]) {
    source1.forEach((sentence,index)=>{
        if(sentence!==source2[index]){
            console.debug(`
            index: ${index};
            source1:${sentence};
            source2:${source2[index]};      
            `)
        }
    })
}

test(handle(source1),handle(source2))