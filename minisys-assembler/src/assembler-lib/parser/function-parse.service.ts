import { OperatorRepository } from '../interface';
import { AssemblyFragment } from '../interface';

export class FunctionParseService {
  constructor(
    private readonly operatorRepo:OperatorRepository,
  ) {}

  public parse(operator:AssemblyFragment):string {
    return this.operatorRepo.get(operator.code).func;
  }
}