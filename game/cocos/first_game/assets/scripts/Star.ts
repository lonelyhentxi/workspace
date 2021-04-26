const {ccclass, property} = cc._decorator;

import Game from './Game';

@ccclass
export default class Star extends cc.Component {
    @property
    pickRadius: number = 0;

    game: Game = null;

    // LIFE-CYCLE CALLBACKS:

    // onLoad () {}

    start () {

    }

    update () {
        if (this.getPlayerDistance() < this.pickRadius) {
            this.onPicked();
            return;
        }
    }

    getPlayerDistance () {
        const playerPos = this.game.player.getPosition();
        const dist = cc.v2(this.node.position).sub(playerPos).mag();

        return dist;
    }

    onPicked () {
        this.game.spawnNewStar();

        this.game.gainScore();

        this.node.destroy();
    }
}
