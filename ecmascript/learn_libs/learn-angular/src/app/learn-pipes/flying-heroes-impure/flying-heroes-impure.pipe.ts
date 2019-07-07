import { Pipe, PipeTransform } from '@angular/core';

import { Flyer } from './heroes';

@Pipe({
  name: 'flyingHeroesImpure',
  pure: false,
})
export class FlyingHeroesImpurePipe implements PipeTransform {

  transform(allHeroes: Flyer[]): any {
    return allHeroes.filter(hero=>hero.canFly);
  }
}
