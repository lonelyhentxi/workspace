export class ARuleOfDivisibilityBy13 {
    private static thirtStorage = [1, 10, 9, 12, 3, 4];
    
    private static thirtNext(n: number): number {
        const n_list = n.toString(10).split('').map(ch=>parseInt(ch));
        n_list.reverse();
        const length = this.thirtStorage.length;
        return n_list.reduce((acc,curr,index)=>{
            return acc+curr*ARuleOfDivisibilityBy13.thirtStorage[index%length];
        }, 0);
    }

    public static thirt(n:number): number {
        let next = n;
        let current;
        do {
            current = next;
            next = ARuleOfDivisibilityBy13.thirtNext(current);
        } while(next!==current);
        return next;
    }
}