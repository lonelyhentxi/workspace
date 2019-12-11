export class WeightForWeight {
    public static weight(numWord: string): number {
        let sum = 0;
        for(const ch of numWord) {
            sum += parseInt(ch);
        }
        return sum;
    }

    public static orderWeight(input: string) {
         const orders = [];
         const inputs = input.split(' ').filter(val=>val!=='');
         for(const num of inputs) {
             orders.push({
                 value: num,
                 weight: this.weight(num)
             })
         }
         orders.sort((a,b)=>{
             if(a.weight>b.weight) {
                 return 1;
             } else if (a.weight===b.weight) {
                 const aValue = a.value.toString();
                 const bValue = b.value.toString();
                 if(aValue>bValue) {
                     return 1;
                 } else if(aValue===bValue) {
                     return 0;
                 } else {
                     return -1
                 }
             } else {
                 return -1;
             }
         });
         return orders.map(val=>val.value).join(' ');
    }
}