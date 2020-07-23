const {ccclass, property} = cc._decorator;

@ccclass
export default class Player extends cc.Component {
    @property
    text: string = 'hello';

    @property
    jumpHeight: number = 0;

    @property
    jumpDuration: number = 0;

    @property
    maxMoveSpeed: number = 0;

    @property
    acceleration: number = 0;

    accLeft: boolean = false;

    accRight: boolean = false;

    xSpeed: number = 0;

    // LIFE-CYCLE CALLBACKS:

    onLoad () {
        const jumpAction = this.runAndJump();
        cc.tween(this.node).then(jumpAction).start();

        this.accLeft = false;
        this.accRight = false;
        this.xSpeed = 0;

        cc.systemEvent.on(cc.SystemEvent.EventType.KEY_DOWN, this.onKeyDown, this);
        cc.systemEvent.on(cc.SystemEvent.EventType.KEY_UP, this.onKeyUp, this);
    }

    onDestroy() {
        cc.systemEvent.off(cc.SystemEvent.EventType.KEY_DOWN, this.onKeyDown, this);
        cc.systemEvent.off(cc.SystemEvent.EventType.KEY_UP, this.onKeyUp, this);
    }

    update(dt) {
        if (this.accLeft) {
            this.xSpeed -= this.acceleration * dt;
        } else if (this.accRight) {
            this.xSpeed += this.acceleration * dt;
        }
        if (Math.abs(this.xSpeed) > this.maxMoveSpeed) {
            this.xSpeed = this.maxMoveSpeed * this.xSpeed / Math.abs(this.xSpeed);
        }

        this.node.x += this.xSpeed * dt;
        
    }

    start () {

    }

    runAndJump () {
        const jumpUp = cc.tween().by(this.jumpDuration, { y: this.jumpHeight }, { easing: 'sineOut' });
        const jumpDown = cc.tween().by(this.jumpDuration, { y: -this.jumpHeight }, { easing: 'sineIn' });
        const tween = cc.tween().sequence(jumpUp, jumpDown);
        return cc.tween().repeatForever(tween);
    }

    onKeyDown(event) {
        switch(event.keyCode) {
            case cc.macro.KEY.a:
                this.accLeft = true;
                break;
            case cc.macro.KEY.d:
                this.accRight = true;
                break;
        }
    }

    onKeyUp (event) {
        switch(event.keyCode) {
            case cc.macro.KEY.a:
                this.accLeft = false;
                break;
            case cc.macro.KEY.d:
                this.accRight = false;
                break;
        }
    }
}
