export interface InstructionTypeDto {
  key: string;
  op: string;
  type: string;
  func?: string;
  shamt?: string;
  rs?: string;
  rt?: string;
  rd?: string;
  immediate?:string;
}