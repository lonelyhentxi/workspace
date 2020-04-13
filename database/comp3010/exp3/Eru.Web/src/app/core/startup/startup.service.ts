import { Injectable, Injector, Inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { zip } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { MenuService, SettingsService, TitleService, ALAIN_I18N_TOKEN } from '@delon/theme';
import { DA_SERVICE_TOKEN, ITokenService } from '@delon/auth';
import { ACLService } from '@delon/acl';
import { TranslateService } from '@ngx-translate/core';
import { I18NService } from '../i18n/i18n.service';

import { NzIconService } from 'ng-zorro-antd';
import { ICONS_AUTO } from '../../../style-icons-auto';
import { ICONS } from '../../../style-icons';
import { ApiService } from '@core/eru/api.service';

/**
 * 用于应用启动时
 * 一般用来获取应用所需要的基础数据等
 */
@Injectable()
export class StartupService {
  constructor(
    iconSrv: NzIconService,
    private menuService: MenuService,
    private translate: TranslateService,
    @Inject(ALAIN_I18N_TOKEN) private i18n: I18NService,
    private settingService: SettingsService,
    private aclService: ACLService,
    private titleService: TitleService,
    @Inject(DA_SERVICE_TOKEN) private tokenService: ITokenService,
    private httpClient: HttpClient,
    private injector: Injector,
    private apiService: ApiService,
  ) {
    iconSrv.addIcon(...ICONS_AUTO, ...ICONS);
  }

  private viaServer(resolve: any, reject: any) {
    const app: any = {
      name: 'Eru',
      description: 'Eru blog engine framework front-end',
    };
    const menu = [
      {
        text: '主导航',
        group: true,
        children: [
          {
            text: '主页',
            link: '/dashboard',
            icon: { type: 'icon', value: 'home' },
          },
        ],
      },
    ];
    this.titleService.suffix = app.name;
    zip(
      this.httpClient.get(this.apiService.apiJoin('postcategories')),
      this.httpClient.get(this.apiService.apiJoin('applications')),
    ).pipe(
      catchError(([postCategories, applications]) => {
        resolve(null);
        return [postCategories, applications];
      }),
    ).subscribe(([postCategoriesRes, applicationsRes]) => {
        const postCategories: { Name: string, Description: string, Id: number }[] = postCategoriesRes.body;
        this.apiService.apiCache.applications.splice(0,this.apiService.apiCache.applications.length,...applicationsRes.body);
        for (const c of postCategories) {
          menu[0].children.push({
            text: c.Name,
            icon: { type: 'icon', value: 'folder' },
            link: `/postcategories/${c.Id}`,
          });
        }
        // 应用信息：包括站点名、描述、年份
        this.settingService.setApp(app);
        // 初始化菜单
        this.menuService.add(menu as any);
        // 设置页面标题的后缀
        this.titleService.suffix = app.name;
      },
      () => {
      },
      () => {
        resolve(null);
      });
  }

  load(): Promise<any> {
    return new Promise((resolve, reject) => {
      this.httpClient
        .get(`assets/tmp/i18n/${this.i18n.defaultLang}.json`)
        .subscribe(langData => {
          this.translate.setTranslation(this.i18n.defaultLang, langData);
          this.translate.setDefaultLang(this.i18n.defaultLang);

          this.viaServer(resolve, reject);
        });
    });
  }
}
