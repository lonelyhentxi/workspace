export interface OperatorFormattor {
  op:string;
  type:string;
  func?:string;
  shamt?:string;
  immediate?:string;
  rs?:string;
  rt?:string;
  rd?:string;
}

export interface OperatorRepository {
  get:(prop:string)=>OperatorFormattor;
}

export const OperatorRepositoryToken = 'OperatorRepositoryToken';
