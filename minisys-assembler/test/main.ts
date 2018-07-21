import {
  Minisys32PreProcessService, Minisys32AssemblerService,
  Minisys32AnalyseService, Minisys32ParseService, Minisys32OperatorRepository, MiniSys32RegRepository, Minisys32PostProcessService,
} from '../src/minisys-32-assmebler';
import { NumericParseService, RegParseService } from '../src/assembler-lib';
import * as path from 'path';

const operatorRepo = new Minisys32OperatorRepository();
const regRepo = new MiniSys32RegRepository();
const numericParesService = new NumericParseService();
const regParseService = new RegParseService(regRepo,numericParesService);
const assembler = new Minisys32AssemblerService(
  new Minisys32PreProcessService(), new Minisys32AnalyseService(operatorRepo),
  new Minisys32ParseService(regParseService,numericParesService,operatorRepo),new Minisys32PostProcessService(numericParesService));