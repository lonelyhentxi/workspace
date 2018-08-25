import { Minisys32PreProcessService } from './minisys-32-pre-process.service';
import { Minisys32AnalyseService } from './minisys-32-analyse.service';
import { Minisys32ParseService } from './minisys-32-parse.service';
import { Minisys32PostProcessService } from './minisys-32-post-process-service';
import {Injectable} from '@angular/core';

@Injectable()
export class Minisys32AssemblerService {
  constructor(
    private readonly preProcessService: Minisys32PreProcessService,
    private readonly analyseService: Minisys32AnalyseService,
    private readonly parseService: Minisys32ParseService,
    private readonly postProcessService: Minisys32PostProcessService,
  ) {
  }

  process(source: string, size:number) {
    const me = this;
    const { code:instructions, data:dataFragments } = this.preProcessService.preProcess(source);
    const instructionFormats = instructions.map(ins => me.analyseService.instructionFormat(ins));
    const program = this.postProcessService.fill(instructionFormats.map(frags => frags.map(frag=>me.parseService.parse(frag)).join(''))
      .map(val=>this.postProcessService.convertRadix(val,2,16,8)),size,8).join(',\n');
    const memory = this.postProcessService.fill(dataFragments.map(val=>me.parseService.parse(me.analyseService.dataFormat(val)))
      .map(val=>this.postProcessService.convertRadix(val,2,16,8)),size,8).join(',\n');
      return {program,memory};
  }
}
