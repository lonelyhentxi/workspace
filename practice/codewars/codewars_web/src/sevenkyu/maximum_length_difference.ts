export class G964 {
    public static mxdiflg = (a1:string[], a2:string[]): number => {
        if(a1.length===0||a2.length===0) {
            return -1;
        }
        const a1Length = a1.map(arr=>arr.length);
        const a2Length = a2.map(arr=>arr.length);
        const a1Max = Math.max(...a1Length);
        const a2Max = Math.max(...a2Length);
        const a1Min = Math.min(...a1Length);
        const a2Min = Math.min(...a2Length);
        return Math.max(Math.abs(a1Max-a2Min),Math.abs(a2Max-a1Min));
    }
}