import {Component, ComponentFactoryResolver, Input, OnDestroy, OnInit, ViewChild} from '@angular/core';
import {AdDirective} from './ad.directive';
import {Subscription, timer} from 'rxjs';
import {AdItemComponent} from '../ad-item/ad-item.component';

@Component({
  selector: 'app-ad',
  template: `
    <div class="ad-banner-example">
      <h3>Advertisement</h3>
      <ng-template appAdHost></ng-template>
    </div>
  `,
})
export class AdComponent implements OnInit, OnDestroy {
  @Input() ads: { headline: string, body: string }[] = new Array(10).fill(0).map((_, index) => ({
    headline: 'superman' + index,
    body: 'i\'m superman'
  }));
  currentAdIndex = -1;
  @ViewChild(AdDirective, {static: true}) appAdHost: AdDirective;
  timer$$: Subscription;

  constructor(private componentFactoryResolver: ComponentFactoryResolver) {
  }

  ngOnInit() {
    this.timer$$ = timer(0, 1000).subscribe(() => {
      this.loadComponent();
    });
  }

  ngOnDestroy(): void {
    this.timer$$.unsubscribe();
  }

  loadComponent() {
    this.currentAdIndex = (this.currentAdIndex + 1) % this.ads.length;
    const adItem = this.ads[this.currentAdIndex];
    const componentFactory = this.componentFactoryResolver.resolveComponentFactory(AdItemComponent);
    const viewContainerRef = this.appAdHost.viewContainerRef;
    viewContainerRef.clear();
    const componentRef = viewContainerRef.createComponent(componentFactory);
    (<AdItemComponent>componentRef.instance).data = adItem;
  }
}
