export class G964 {
    public static doubles(maxk: number, maxn: number): number {
        let s = 0;
        for (let i = 1; i <= maxk; i++) {
            for (let j = 1; j <= maxn; j++) {
                s+= 1/(i*Math.pow((j+1),(2*i)))
            }
        }
        return s;
    }
}