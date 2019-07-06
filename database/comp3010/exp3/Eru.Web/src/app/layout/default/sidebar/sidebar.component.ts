import { Component, ChangeDetectionStrategy, Inject } from '@angular/core';
import { SettingsService } from '@delon/theme';
import { Router } from '@angular/router';
import { DA_SERVICE_TOKEN, ITokenService } from '@delon/auth';
import { NzMessageService } from 'ng-zorro-antd';

@Component({
  selector: 'layout-sidebar',
  templateUrl: './sidebar.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SidebarComponent {
  constructor(
    public settings: SettingsService,
    public msg: NzMessageService,
    private router: Router,
    @Inject(DA_SERVICE_TOKEN) private tokenService: ITokenService,
  ) {
  }

  logout() {
    this.tokenService.clear();
    this.router.navigateByUrl(this.tokenService.login_url);
  }
}
