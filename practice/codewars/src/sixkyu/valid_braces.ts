export class Challenge {
    static openBraces = new Set(['(','[','{']);
    static closeBraces = new Set([')',']','}']);
    static braceMap = new Map([[')','('],[']','['],['}','{']]);
    static validBraces(braces: string): boolean {
        const braceStack = [];
        for(const ch of braces) {
            if(Challenge.openBraces.has(ch)) {
                braceStack.push(ch);
            } else if (Challenge.closeBraces.has(ch)) {
                if (Challenge.braceMap.get(ch)!==braceStack.pop()) {
                    return false;
                }
            }
        }
        return (braceStack.length===0);
    }
}