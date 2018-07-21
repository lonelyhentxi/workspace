import { OperatorRepository } from '../interface';
import { AssemblyFragment} from '../interface';

export class OperatorParseService {
  constructor(
    private readonly operatorRepo:OperatorRepository,
  ) {}

  parse(operator:AssemblyFragment):string {
    return this.operatorRepo.get(operator.code).op;
  }
}