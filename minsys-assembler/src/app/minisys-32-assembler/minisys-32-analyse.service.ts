import {AssemblyFragment, OperatorFormattor, OperatorRepositoryToken, ParseFragment} from './assembler-lib/index';
import { Minisys32OperatorRepository } from './schema/index';
import { isNil } from 'lodash';
import {Inject, Injectable} from '@angular/core';

@Injectable()
export class Minisys32AnalyseService {
  constructor(
    @Inject(OperatorRepositoryToken)
    public readonly operatorRepo: Minisys32OperatorRepository,
  ) {
  }

  private rPosition = ['rs', 'rt', 'rd', 'shamt'];
  private splitter = /[(),\[\]]/.compile();

  public instructionFormat([operator, operands]: [AssemblyFragment, AssemblyFragment[]]): ParseFragment[] {
    const opRender = this.operatorRepo.get(operator.code);
    if (opRender.type === 'r') {
      return this.rFormat(opRender, operator, operands);
    } else if (opRender.type === 'i') {
      return this.iFormat(opRender, operator, operands);
    } else if (opRender.type === 'j') {
      return this.jFormat(opRender, operator, operands);
    }
    throw new Error('invalid operator');
  }

  public dataFormat(frag:AssemblyFragment):ParseFragment {
    return frag;
  }

  private rFormat(opRender: OperatorFormattor, operator: AssemblyFragment, operands: AssemblyFragment[]): ParseFragment[] {
    const slices = operands.map(val => val.code);
    if (isNil(opRender.shamt)) {
      return [{ type: 'op', code: operator.code }, { type: 'reg', code: opRender.rs }, { type: 'reg', code: slices[1] },
        { type: 'reg', code: slices[0] }, { type: 'shamt', code: slices[2] }, { type: 'func', code: operator.code }];
    } else if (operator.code === 'jr') {
      return [{ type: 'op', code: operator.code }, { type: 'reg', code: slices[0] }, { type: 'reg', code: opRender.rt },
        { type: 'reg', code: opRender.rd }, { type: 'shamt', code: opRender.shamt }, { type: 'func', code: operator.code }];
    } else if (operator.code === 'sllv' || operator.code === 'srlv' || operator.code === 'srav') {
      return [{ type: 'op', code: operator.code }, { type: 'reg', code: slices[2] }, { type: 'reg', code: slices[1] },
        { type: 'reg', code: slices[0] }, { type: 'shamt', code: opRender.shamt }, { type: 'func', code: operator.code }];
    } else {
      return [{ type: 'op', code: operator.code }, { type: 'reg', code: slices[1] }, { type: 'reg', code: slices[2] },
        { type: 'reg', code: slices[0] }, { type: 'shamt', code: opRender.shamt }, { type: 'func', code: operator.code }];
    }
  }

  private iFormat(opRender: OperatorFormattor, operator: AssemblyFragment, operands: AssemblyFragment[]): ParseFragment[] {
    const slices = operands.map(val => val.code);
    if (operator.code === 'lui') {
      return [{ type: 'op', code: operator.code }, { type: 'reg', code: opRender.rs },
        { type: 'reg', code: slices[0] }, { type: 'immediate', code: slices[1] }];
    } else if (operator.code === 'lw' || operator.code === 'sw') {
      return [{ type: 'op', code: operator.code }, { type: 'reg', code: slices[2] }, { type: 'reg', code: slices[0] }, {
        type: 'immediate',
        code: slices[1],
      }];
    } else if (opRender.immediate === 'offset') {
      return [{ type: 'op', code: operator.code }, { type: 'reg', code: slices[0] }, { type: 'reg', code: slices[1] }, {
        type: 'immediate',
        code: slices[2],
      }];
    } else {
      return [{ type: 'op', code: operator.code }, { type: 'reg', code: slices[1] }, { type: 'reg', code: slices[0] }, {
        type: 'immediate',
        code: slices[2],
      }];
    }
  }

  private jFormat(opRender: OperatorFormattor, operator: AssemblyFragment, operands: AssemblyFragment[]): ParseFragment[] {
    const slices = operands.map(val => val.code);
    return [{ type: 'op', code: operator.code }, { type: 'address', code: slices[0] }];
  }
}
