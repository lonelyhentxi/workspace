import { RegRepository } from '../assembler-lib/index';
import {Injectable} from '@angular/core';

@Injectable()
export class MiniSys32RegRepository implements RegRepository {
  dict = {
    $zero: 0, $at: 1,
    $v0: 2, $v1: 3,
    $a0: 4, $a1: 5, $a2: 6, $a3: 7,
    $t0: 8, $t1: 9, $t2: 10, $t3: 11, $t4: 12, $t5: 13, $t6: 14, $t7: 15,
    $s0: 16, $s1: 17, $s2: 18, $s3: 19, $s4: 20, $s5: 21, $s6: 22, $s7: 23,
    $t8: 24, $t9: 25, $i0: 26, $i1: 27, $gp: 28, $sp: 29, $s8: 30, $fp: 30, $ra: 31,
    $0: 0, $1: 1, $2: 2, $3: 3, $4: 4, $5: 5, $6: 6, $7: 7, $8: 8, $9: 9, $10: 10,
    $11: 11, $12: 12, $13: 13, $14: 14, $15: 15, $16: 16, $17: 17, $18: 18, $19: 19,
    $20: 20, $21: 21, $22: 22, $23: 23, $24: 24, $25: 25, $26: 26, $27: 27, $28: 28, $29: 29, $30: 30, $31: 31,$s9:28
  };

  get(prop: string) {
    return this.dict[prop.toLowerCase()];
  }
}