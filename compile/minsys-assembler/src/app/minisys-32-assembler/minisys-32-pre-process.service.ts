import {AssemblyFragment} from './assembler-lib/interface/index';
import {Injectable} from '@angular/core';

@Injectable()
export class Minisys32PreProcessService {
  private readonly globalSegmentPattern = /(?:\.(?:DATA|TEXT))\s*\w*\s*\n\s*([\s\S]*?)(?=\.(DATA|TEXT|END))/ig;
  private readonly segmentPattern = /(?:\.(DATA|TEXT))\s*(\w*)\s*\n\s*([\s\S]+)/i;
  private readonly labeledInstructionPattern: RegExp = /^\s*(\w+)\s*:\s*(\w+(?:\s*[\-$\w()]+\s*,?){1,3})\s*$/;
  private readonly preInstructionPattern: RegExp = /(\w+\s*:)\s*(\w+)/g;
  private readonly wordReg = /(\w+)\s*:\s*\.word\s*(.*)/i;
  private readonly operandSplitter = /[(),\[\]]/g;
  private readonly commentPattern = /[#;]+.*(?=\n)/g;

  constructor() {
  }

  preProcess(asm: string): { code: [AssemblyFragment, AssemblyFragment[]][], data: AssemblyFragment[] } {
    const me = this;
    const marks = new Map<string, string>();
    asm = this.commentRemove(asm);
    const segmentsMap = new Map<string, string>();
    const segments = this.asmSplit(asm);
    const splitSegs = segments.map(val => me.segmentSplit(val));
    splitSegs.forEach(seg => {
      segmentsMap.set(seg[0].toLowerCase(), seg[2]);
    });
    const dataFrags = this.dataMark(segmentsMap.get('data'), marks);
    const splitCode = this.codeSplit(segmentsMap.get('text'));
    const instructionsWithOutLabel = this.labelMark(splitCode, marks);
    return {
      code: instructionsWithOutLabel.map((ins,index) => {
        return me.instructionSplitAndReplaceLabel(ins,index, marks);
      }), data: dataFrags
    };
  }

  commentRemove(asm: string): string {
    return asm.replace(this.commentPattern, '');
  }

  asmSplit(asm: string): string[] {
    const tempAsm = asm + '.END';
    return tempAsm.match(this.globalSegmentPattern);
  }

  segmentSplit(segment: string): string[] {
    return segment.match(this.segmentPattern).slice(1, 4);
  }

  codeSplit(code: string): string[] {
    code = code.replace(this.preInstructionPattern, '$1$2');
    return code.split('\n').filter(val => val.trim() !== '');
  }

  dataMark(data: string, marks: Map<string, string>): AssemblyFragment[] {
    const me = this;
    const matchGRes = data.split('\n').filter(val => val.trim() !== '');
    let index = 0 ;
    const res: AssemblyFragment[] = [];
    for(let i=0;i<matchGRes.length;i++) {
      const matchRes = matchGRes[i].match(me.wordReg);
      const dataList = matchRes[2].split(',').map(val2 => val2.trim()).filter(val3 => val3 !== '');
      res.push(...dataList.map(val1 => {
        return {type: 'data', code: val1};
      }));
      marks.set(matchRes[1], (index * 4).toString());
      index = index + dataList.length;
    }
    return res;
  }

  labelMark(instructions: string[], marks: Map<string, string>, instructionSize: number = 1): string[] {
    const me = this;
    return instructions.map((ins, index) => {
      const match = ins.match(me.labeledInstructionPattern);
      if (match) {
        marks.set(match[1], ((index) * instructionSize).toString());
        return match[2].trim();
      } else {
        return ins.trim();
      }
    });
  }

  instructionSplitAndReplaceLabel(instruction: string,index:number, marks: Map<string, string>): [AssemblyFragment, AssemblyFragment[]] {
    const me = this;
    const slices = instruction.split(/\s+/);
    const operator = {
      type: 'operator',
      code: slices[0],
    };
    const post = slices.slice(1, slices.length).join('').trim();
    const operands = post.split(this.operandSplitter).map(val => val.trim()).filter(val => val !== '').map(operand => {
      let operandCode = operand;
      if(marks.has(operand)) {
        if(operator.code==='beq'||operator.code==='bne') {
          operandCode=(parseInt(marks.get(operand))-index-1).toString();
        } else {
          operandCode=marks.get(operand);
        }
      }
      return {
        type: 'operand',
        code: operandCode
      };
    });
    return [operator, operands];
  }
}
