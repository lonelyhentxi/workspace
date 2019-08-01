import {AfterViewInit, Component, ElementRef, ViewChild} from '@angular/core';
import {ChainbankAgentService} from '@app/feature/services/chainbank-agent';
import {toCanvas} from 'qrcode';

@Component({
  selector: 'app-console-profile',
  template: `
      <nz-card nzHoverable [nzCover]="coverTemplate" class="transparent-bg console-actor">
          <nz-card-meta [nzTitle]="'Actor'+' '+chainbank.actor.privilege"
                        [nzDescription]="accountAddressTemplate">
          </nz-card-meta>
      </nz-card>
      <ng-template #coverTemplate>
          <canvas #coverCanvas style="margin: auto;"></canvas>
      </ng-template>
      <ng-template #accountAddressTemplate>
          <div style="word-break: break-all;">
              {{chainbank.actor.identity}}
          </div>
      </ng-template>
  `,
  styles: []
})
export class ConsoleProfileComponent implements AfterViewInit {

  constructor(
    private readonly chainbank: ChainbankAgentService,
  ) {
  }

  @ViewChild('coverCanvas', {static: false})
  coverCanvas: ElementRef;

  async ngAfterViewInit() {
    await toCanvas(this.coverCanvas.nativeElement,
      this.chainbank.actor.identity, {
        width: 256
      });
  }

}
