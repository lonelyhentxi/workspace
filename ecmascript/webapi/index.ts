import * as yargs from 'yargs';
import {exec} from 'child_process';
import * as path from 'path';

const webPagePath: string = yargs.argv['w'] as string;

exec(`start ${path.normalize(webPagePath)}`);

