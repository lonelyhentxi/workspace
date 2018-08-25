import {NgModule} from '@angular/core';
import { CommonModule } from '@angular/common';
import {Minisys32PostProcessService} from './minisys-32-post-process-service';
import {Minisys32OperatorRepository, MiniSys32RegRepository} from './schema/index';
import {Minisys32AssemblerService} from './minisys-32-assembler.service';
import {Minisys32ParseService} from './minisys-32-parse.service';
import {Minisys32AnalyseService} from './minisys-32-analyse.service';
import {NumericParseService, RegParseService, OperatorRepositoryToken, RegRepositoryToken} from './assembler-lib/index';
import {Minisys32PreProcessService} from './minisys-32-pre-process.service';
import {Minisys32AssemblerComponent} from './minisys-32-assembler.component';
import {Minisys32AssemblerRoutingModule} from './minisys-32-assembler-routing.module';
import {BrowserModule} from '@angular/platform-browser';
import {BrowserAnimationsModule} from '@angular/platform-browser/animations';
import {FormsModule} from '@angular/forms';
import {HttpClientModule} from '@angular/common/http';
import {NgZorroAntdModule, NZ_I18N, zh_CN} from 'ng-zorro-antd';
import {registerLocaleData} from '@angular/common';
import zh from '@angular/common/locales/zh';

registerLocaleData(zh);


@NgModule({
  imports: [Minisys32AssemblerRoutingModule,CommonModule,BrowserModule,
    BrowserAnimationsModule,
    FormsModule,
    HttpClientModule,
    NgZorroAntdModule],
  providers: [NumericParseService, RegParseService,
    {
      provide: OperatorRepositoryToken, useClass: Minisys32OperatorRepository
    },
    {
      provide: RegRepositoryToken, useClass: MiniSys32RegRepository
    },
    Minisys32PostProcessService, Minisys32ParseService, Minisys32AnalyseService, Minisys32PreProcessService,
    Minisys32AssemblerService,{provide: NZ_I18N, useValue: zh_CN}],
  declarations: [Minisys32AssemblerComponent],
  exports: [],
})
export class Minisys32AssemblerModule {

}
