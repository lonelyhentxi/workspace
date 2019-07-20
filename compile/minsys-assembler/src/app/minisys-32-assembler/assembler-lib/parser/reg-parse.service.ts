import {ParseFragment, RegRepository, RegRepositoryToken} from '../interface/index';
import { NumericParseService } from './numeric-parse.service';
import { isNil } from 'lodash';
import { Inject, Injectable} from '@angular/core';

@Injectable()
export class RegParseService {
  constructor(
    @Inject(RegRepositoryToken)
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
