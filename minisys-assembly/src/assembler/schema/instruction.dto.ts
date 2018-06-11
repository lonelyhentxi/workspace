import { InstructionTypeDto } from './instruction-type.dto';
import { IsNil } from 'lodash';
const RegParses =  {
  $zero:0, $at:1,
  $v0:2,
};

export class Operand {
  content: string;

  public isReg():boolean {
    return /^\$/.test(this.content);
  }
  public isNumber():boolean {
    return !this.isReg(); // TODO
  }
  private parseNumber():string {
      return Number(this.content).toString(2);
  }
  private parseReg(regParses:{[prop:string]:number}):number {
    if(IsNil(regParses[this.content])) {
      throw new Error('invalid reg');
    }
    return regParses[this.content];
  }
}


export class Instruction {
  input:string;
  raw?:string;
  rs?:string;
  rt?:string;
  rd?:string;
  shamt?:string;
  func?:string;
  output:string;
  type:InstructionTypeDto;

  private isReg() {

  }

  regParse(literal:string) {

  }

  getRender() {
    const temp = this.input.substr(6,5);
  }
}