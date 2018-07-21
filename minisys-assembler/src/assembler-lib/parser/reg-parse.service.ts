import { ParseFragment,RegRepository } from '../interface';
import { NumericParseService } from './numeric-parse.service';
import { isNil } from 'lodash';

export class RegParseService {
  constructor(
    public readonly regRepo:RegRepository,
    public readonly numericPareService:NumericParseService,
  ) {}

  public parse(operand:ParseFragment,length:number):string {
    if(isNil(this.regRepo.get(operand.code))) {
      return operand.code;
    }
    return this.numericPareService.comp(this.regRepo.get(operand.code),length);
  }
}