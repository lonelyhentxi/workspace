import {ParseFragment} from '../interface/index';
import {Injectable} from '@angular/core';

@Injectable()
export class NumericParseService {
  public validate(operand:ParseFragment):boolean {
    return Number.isNaN(Number(operand.code));
  }

  public neg(literal:string) {
    let temp = literal.replace(/0/g,'a');
    temp = temp.replace(/1/g,'0');
    return temp.replace(/a/g,'1');
  }

  public fill(value:string,length:number):string {
    return '0'.repeat(length-value.length)+value;
  }

  public comp(value:number,length:number):string {
    if(value<0){
      const tempStr = (Math.abs(value)-1).toString(2);
      return this.neg(this.fill(tempStr,length));
    } else {
      const tempStr = value.toString(2);
      return this.fill(tempStr,length);
    }
  }

  public parse(operand:ParseFragment,length:number):string {
    const value = parseInt(operand.code);
    return this.comp(value,length);
  }
}
