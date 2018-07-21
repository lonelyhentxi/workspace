import {Component} from '@angular/core';
import {Minisys32AssemblerService} from './minisys-32-assembler.service';

@Component({
  selector: 'app-minisys-32-assembler',
  templateUrl: './minisys-32-assembler.component.html',
  styleUrls: ['./minisys-32-assembler.component.less']
})
export class Minisys32AssemblerComponent {
  constructor(
    private readonly assemblerService:Minisys32AssemblerService
  ) {
  }

  inputAsm: string = '';
  output: string[] = ['',''];
  outputIndex:number = 0;

  onAssembler() {
    try {
      const {program,memory} = this.assemblerService.process(this.inputAsm,16*Math.pow(2,10));
      this.output[0]= program;
      this.output[1]=memory;
    } catch (e) {
      console.debug(e);
      this.output[0]='错误的输入';
      this.output[1]='错误的输入';
    }
  }
}
