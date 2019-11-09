import {Injectable} from '@angular/core';
import {ActivatedRouteSnapshot, CanActivate, CanActivateChild, Router, RouterStateSnapshot, UrlTree} from '@angular/router';
import {Observable} from 'rxjs';
import {ChainbankAgentService} from './chainbank-agent.service';

@Injectable()
export class AuthGuard implements CanActivate, CanActivateChild {

  constructor(
    private readonly chainbank: ChainbankAgentService,
    private readonly router: Router,
  ) {
  }

  canActivate(
    next: ActivatedRouteSnapshot,
    state: RouterStateSnapshot): Observable<boolean | UrlTree> | Promise<boolean | UrlTree> | boolean | UrlTree {
    if(this.chainbank.actor == null) {
      this.goHome();
    }
    return this.chainbank.actor != null;
  }

  canActivateChild(childRoute: ActivatedRouteSnapshot, state: RouterStateSnapshot)
    : Observable<boolean | UrlTree> | Promise<boolean | UrlTree> | boolean | UrlTree {
    if (childRoute.url.toString()==='clerk') {
      const res = this.chainbank.canEditCustomer();
      if(!res) {
        this.goHome();
      }
      return res;
    } else if(childRoute.url.toString().startsWith('customer')) {
      return this.canActivate(childRoute, state);
    } else {
      this.goHome();
      return false;
    }
  }

  goHome() {
    this.router.navigateByUrl('/');
  }
}
