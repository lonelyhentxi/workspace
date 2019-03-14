import {
  OperatorRepository,
  NumericParseService,
  ParseFragment,
  RegParseService,
  RegRepositoryToken,
  OperatorRepositoryToken
} from './assembler-lib/index';
import {Inject, Injectable} from '@angular/core';

@Injectable()
export class Minisys32ParseService {
  constructor(
    public readonly regParseService: RegParseService,
    public readonly numericParseService: NumericParseService,
    @Inject(OperatorRepositoryToken)
    public readonly operatorRepo: OperatorRepository,
  ) {}

  public parse(fragment: ParseFragment): string {
    if (fragment.type === 'reg') {
      return this.regParseService.parse(fragment,5);
    } else if (fragment.type === 'op') {
      return this.operatorRepo.get(fragment.code).op;
    } else if (fragment.type === 'func') {
      return this.operatorRepo.get(fragment.code).func;
    } else if (fragment.type === 'immediate'||fragment.type==='offset') {
      return this.numericParseService.parse(fragment, 16);
    } else if (fragment.type === 'address') {
      return this.numericParseService.parse(fragment,26);
    } else if(fragment.type==='shamt') {
      return this.numericParseService.parse(fragment,5);
    } else if(fragment.type==='data') {
      return this.numericParseService.parse(fragment,32);
    }
    throw new Error('error parse fragment');
  }
}
