import {Component, OnInit} from '@angular/core';

@Component({
  selector: 'app-i18n-overview',
  template: `
    <h1 i18n="page header|An introduction header for this sample@@introductionHeader">Hello i18n!</h1>
    <img [src]="logo" i18n-title title="logo" alt="logo"/>
    <span i18n>Updated: {minutes, plural,
      =0 {just now}
      =1 {one minute ago}
      other {{{minutes}} minutes ago by {gender, select, male {male} female {female} other {other}}}}
  </span>
  `,
  styles: []
})
export class I18nOverviewComponent implements OnInit {
  logo = 'https://avatars3.githubusercontent.com/u/23011677?s=460&v=4';
  minutes = 3;
  gender = 'male';
  male() { this.gender = 'male'; }
  female() { this.gender = 'female'; }
  other() { this.gender = 'other'; }
  constructor() {
  }

  ngOnInit() {
  }

}
