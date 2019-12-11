export class G964 {
    public static sqInRect(l: number, w: number): number[] | null {
        if (l == w) {
            return null;
        }
        const res = [];
        do {
            let max_num = Math.max(l,w);
            let min_num = Math.min(l,w);
            res.push(min_num);
            l = max_num - min_num;
            w = min_num;
        } while(l!==0&&w!==0);
        return res;
    }
}