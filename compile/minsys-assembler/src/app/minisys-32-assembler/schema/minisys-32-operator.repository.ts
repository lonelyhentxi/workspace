import { OperatorFormattor, OperatorRepository } from '../assembler-lib/index';

export class Minisys32OperatorRepository implements OperatorRepository {
  dict:{[prop:string]:OperatorFormattor} = {
    add: {
      op: '000000',
      type: 'r',
      func: '100000',
      shamt: '00000',
    },
    addu: {
      op: '000000',
      type: 'r',
      func: '100001',
      shamt: '00000',
    },
    sub: {
      op: '000000',
      type: 'r',
      func: '100010',
      shamt: '00000',
    },
    subu: {
      op: '000000',
      type: 'r',
      func: '100011',
      shamt: '00000',
    },
    and: {
      op: '000000',
      type: 'r',
      func: '100100',
      shamt: '00000',
    },
    or: {
      op: '000000',
      type: 'r',
      func: '100101',
      shamt: '00000',
    },
    xor: {
      op: '000000',
      type: 'r',
      func: '100110',
      shamt: '00000',
    },
    nor: {
      op: '000000',
      type: 'r',
      func: '100111',
      shamt: '00000',
    },
    slt: {
      op: '000000',
      type: 'r',
      func: '101010',
      shamt: '00000',
    },
    sltu: {
      op: '000000',
      type: 'r',
      func: '101011',
      shamt: '00000',
    },
    sll: {
      op: '000000',
      type: 'r',
      func: '000000',
      rs: '00000',
    },
    srl: {
      op: '000000',
      type: 'r',
      func: '000010',
      rs: '00000',
    },
    sra: {
      op: '000000',
      type: 'r',
      func: '000011',
      rs: '00000',
    },
    sllv: {
      op: '000000',
      type: 'r',
      func: '000100',
      shamt: '00000',
    },
    srlv: {
      op: '000000',
      type: 'r',
      func: '000110',
      shamt: '00000',
    },
    srav: {
      op: '000000',
      type: 'r',
      func: '000111',
      shamt: '00000',
    },
    jr: {
      op: '000000',
      type: 'r',
      func: '001000',
      rt: '00000',
      rd: '00000',
      shamt: '00000',
    },
    addi: {
      op: '001000',
      type: 'i',
      immediate: 'immediate',
    },
    addiu: {
      op: '001001',
      type: 'i',
      immediate: 'immediate',
    },
    andi: {
      op: '001100',
      type: 'i',
      immediate: 'immediate',
    },
    ori: {
      op: '001101',
      type: 'i',
      immediate: 'immediate',
    },
    xori: {
      op: '001110',
      type: 'i',
      immediate: 'immediate',
    },
    lui: {
      op: '001111',
      type: 'i',
      rs: '00000',
      immediate: 'immediate',
    },
    lw: {
      op: '100011',
      type: 'i',
      immediate: 'offset',
    },
    sw: {
      op: '101011',
      type: 'i',
      immediate: 'offset',
    },
    beq: {
      op: '000100',
      type: 'i',
      immediate: 'offset',
    },
    bne: {
      op: '000101',
      type: 'i',
      immediate: 'offset',
    },
    slti: {
      op: '001010',
      type: 'i',
      immediate: 'immediate',
    },
    sltiu: {
      op: '001011',
      type: 'i',
      immediate: 'immediate',
    },
    j: {
      op: '000010',
      type: 'j',
    },
    jal: {
      op: '000011',
      type: 'j',
    },
  };
  get(prop:string){
    return this.dict[prop.toLowerCase()];
  }
}
