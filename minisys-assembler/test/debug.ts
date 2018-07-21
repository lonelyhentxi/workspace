import * as fse from 'fs-extra';
import * as path from 'path';

export function jdebug(obj:any) {
    fse.appendFileSync(path.join(__dirname,'temp.json'),JSON.stringify(obj),{encoding:'utf8'});
}