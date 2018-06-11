import { Injectable } from '@nestjs/common';
import { InstructionTypeDto } from './schema/instruction-type.dto';
import { InstructionOutputDto } from './schema/instruction-output.dto';
import { InstructionContentDto } from './schema/instruction.dto';

@Injectable()
export class AssemblerService {
  private instructions = {
    add: {
      op: '000000',
      type: 'R',
      func: '100000',
      shamt: '00000',
    },
    addu: {
      op: '000000',
      type: 'R',
      func: '100001',
      shamt: '00000',
    },
    sub: {
      op: '000000',
      type: 'R',
      func: '100010',
      shamt: '00000',
    },
    subu: {
      op: '000000',
      type: 'R',
      func: '100011',
      shamt: '00000',
    },
    and: {
      op: '000000',
      type: 'R',
      func: '100100',
      shamt: '00000',
    },
    or: {
      op: '000000',
      type: 'R',
      func: '100101',
      shamt: '00000',
    },
    xor: {
      op: '000000',
      type: 'R',
      func: '100110',
      shamt: '00000',
    },
    nor: {
      op: '000000',
      type: 'R',
      func: '100111',
      shamt: '00000',
    },
    slt: {
      op: '000000',
      type: 'R',
      func: '101010',
      shamt: '00000',
    },
    sltu: {
      op: '000000',
      type: 'R',
      func: '101011',
      shamt: '00000',
    },
    sll: {
      op: '000000',
      type: 'R',
      func: '000000',
      rs: '000000',
      shamt: 'shamt',
    },
    srl: {
      op: '000000',
      type: 'R',
      func: '000010',
      rs: '000000',
      shamt: 'shamt',
    },
    sra: {
      op: '000000',
      type: 'R',
      func: '000011',
      rs:'000000',
      shamt: 'shamt',
    },
    sllv: {
      op: '000000',
      type: 'R',
      func: '000100',
      shamt: '000000',
    },
    srlv: {
      op: '000000',
      type: 'R',
      func: '000110',
      shamt: '000000',
    },
    srav: {
      op: '000000',
      type: 'R',
      func: '000111',
      shamt: '000000',
    },
    jr: {
      op: '000000',
      type: 'R',
      func: '001000',
      rt: '000000',
      rd: '000000',
      shamt: '000000',
    },
    addi: {
      op: '001000',
      type: 'I',
      immediate:'immediate',
    },
    addiu: {
      op: '001001',
      type: 'I',
      immediate:'immediate',
    },
    andi: {
      op: '001100',
      type: 'I',
      immediate:'immediate',
    },
    ori: {
      op: '001101',
      type: 'I',
      immediate:'immediate',
    },
    xori: {
      op: '001110',
      type: 'I',
      immediate:'immediate',
    },
    lui: {
      op: '001111',
      type: 'I',
      rs:'000000',
      immediate:'immediate',
    },
    lw: {
      op: '100011',
      type: 'I',
      immediate:'offset',
    },
    sw: {
      op: '101011',
      type: 'I',
      immediate:'offset',
    },
    beq: {
      op: '000100',
      type: 'I',
      immediate:'offset',
    },
    bne: {
      op: '000101',
      type: 'I',
      immediate:'offset',
    },
    slti: {
      op: '001010',
      type: 'I',
      immediate:'immediate',
    },
    sltiu: {
      op: '001011',
      type: 'I',
      immediate:'immediate',
    },
    j: {
      op: '000010',
      type: 'J',
    },
    jal: {
      op: '000011',
      type: 'J',
    },
  };

  public parse(type:InstructionTypeDto,ins:InstructionContentDto) {
    let type: InstructionOutputDto;
    if(type.func==='R'&&type.shamt!=='shamt'&&type.key!=='jr')
    {

    }
  }
}