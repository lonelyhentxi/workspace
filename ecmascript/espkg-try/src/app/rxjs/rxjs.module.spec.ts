import { RxjsModule } from './rxjs.module';

describe('RxjsModule', () => {
  let rxjsModule: RxjsModule;

  beforeEach(() => {
    rxjsModule = new RxjsModule();
  });

  it('should create an instance', () => {
    expect(rxjsModule).toBeTruthy();
  });
});
