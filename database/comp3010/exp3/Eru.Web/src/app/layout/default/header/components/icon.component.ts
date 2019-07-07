import { Component, ChangeDetectionStrategy, ChangeDetectorRef } from '@angular/core';
import { ApiService } from '@core/eru/api.service';
import { HttpClient } from '@angular/common/http';
import { ApplicationDto } from '@core/eru/dtos/application.dto';

@Component({
  selector: 'header-icon',
  template: `
    <nz-dropdown nzTrigger="click" nzPlacement="bottomRight" (nzVisibleChange)="change()">
      <div class="alain-default__nav-item" nz-dropdown>
        <i class="anticon anticon-appstore" style="color: gray"></i>
      </div>
      <div nz-menu class="wd-xl animated jello">
        <nz-spin [nzSpinning]="loading" [nzTip]="'正在读取数据...'">
          <div nz-row [nzType]="'flex'" [nzJustify]="'center'" [nzAlign]="'middle'"
               class="app-icons">
            <div *ngFor="let application of applications" nz-col [nzSpan]="6">
              <i nz-icon [nzType]="application.nzType" [ngClass]="application.classNames"></i>
              <small>{{application.name}}</small>
            </div>
          </div>
        </nz-spin>
      </div>
    </nz-dropdown>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class HeaderIconComponent {
  loading = true;
  public applications: { name: string, classNames: string[], nzType: string, isOutline: boolean } [] = [];

  constructor(private cdr: ChangeDetectorRef,
              private apiService: ApiService) {
  }

  change() {
    this.load();
    setTimeout(()=>{
      this.loading = false;
      this.cdr.detectChanges();
    },500);
  }

  load() {
    this.applications = (this.apiService.apiCache.applications as ApplicationDto[])
      .map(i=>{
        const avatarProps = i.Avatar.split(' ');
        return { name: i.Name, classNames: avatarProps.slice(0,2), nzType: avatarProps[2] , isOutline: avatarProps.length===4 };
      });
  }
}


