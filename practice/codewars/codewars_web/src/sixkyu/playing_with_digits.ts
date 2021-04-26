export class PlayingWithDigits {
    public static digPow(n: number, p: number): number {
        let sum = 0;
        for(const ch of n.toString(10)) {
            sum += Math.pow(parseInt(ch),p);
            p+=1;
        }
        const k = Math.floor(sum/n);
        if(k*n===sum) {
            return k;
        } else {
            return -1;
        }
    }
}