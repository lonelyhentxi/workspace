import {Component, OnInit} from '@angular/core';
import {fromEvent} from 'rxjs';

@Component({
  selector: 'app-rxjs-ch4',
  templateUrl: './rxjs-ch4.component.html',
  styleUrls: ['./rxjs-ch4.component.less']
})
export class RxjsCh4Component implements OnInit {
  clickCount: number = 0;

  constructor() {
  }

  ngOnInit() {
    const event$ = fromEvent(document.querySelector('#clickMe'), 'click');
    event$.subscribe(() => this.clickCount++);
  }

}
