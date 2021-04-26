export function isSolved(board: number[][]): number {
    const row_size = board.length;
    const col_size = board[0].length;
    if(row_size<=0||col_size<=0||row_size!==col_size) {
        throw new RangeError("invalid board size");
    }
    let zeros = 0;
    const size = row_size;
    const rows = Array(size).fill(0);
    const cols = Array(size).fill(0);
    const crosses = [0,0];
    for(let i=0;i<size;i++) {
        for(let j=0;j<size;j++) {
            let point = board[i][j];
            if(point===2) {
               point=-1;
            } if (point===0) {
                zeros+=1;
            }
            rows[i]+=point;
            cols[j]+=point;
            if(i===j) {
                crosses[0]+=point;
            }
            if(i+j!==size-1) {
                crosses[1]+=point;
            }
        }
    }
    const wins = [0,0];
    const lines = [];
    lines.push(...rows,...cols,...crosses);
    for(const i of lines) {
        if(i===size){
            wins[0]+=1;
        } else if(i===0-size) {
            wins[1]+=1;
        }
    }
    if(wins[0]>=1&&wins[1]>=1) {
        return 0;
    } else if(wins[0]>=1) {
        return 1;
    } else if(wins[1]>=1) {
        return 2;
    } else if(zeros>0) {
        return -1;
    } else {
        return 0;
    }
}