import {Component, Input, OnInit} from '@angular/core';

@Component({
  selector: 'app-ad-item',
  template: `
    <div class="job-ad">
      <h4>{{data.headline}}</h4>

      {{data.body}}
    </div>`,
})
export class AdItemComponent implements OnInit {

  constructor() {
  }

  ngOnInit() {
  }

  @Input()
  data: { body: string, headline: string };
}
