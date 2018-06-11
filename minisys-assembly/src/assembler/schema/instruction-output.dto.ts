import { IsString, Length } from 'class-validator';

export class InstructionOutputDto {
  @IsString()
  @Length(6)
  op: string;

  @IsString()
  @Length(5)
  rs: string;

  @IsString()
  @Length(5)
  rt: string;

  @IsString()
  @Length(5)
  rd: string;

  @IsString()
  @Length(5)
  shamt: string;

  @IsString()
  @Length(5)
  func: string;
}