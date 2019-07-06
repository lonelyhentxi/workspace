import {NumericParseService} from './assembler-lib/index';
import {Injectable} from '@angular/core';

@Injectable()
export class Minisys32PostProcessService {
  constructor(
    public readonly numericParseService: NumericParseService,
  ) {
  }

  public convertRadix(source: string, sourceRadix: number, targetRadix: number, length: number) {
    const value = parseInt(source, sourceRadix);
    const target = value.toString(targetRadix);
    return this.numericParseService.fill(target, length);
  }

  public fill(outputs: string[], size: number, length: number) {
    for (let i = 0; i < size - outputs.length; i++) {
      outputs.push('0'.repeat(length));
    }
    return outputs;
  }
}
