const {ccclass, property} = cc._decorator;

@ccclass
export default class Game extends cc.Component {

    @property(cc.Prefab)
    starPrefab: cc.Prefab = null;

    @property
    maxStarDuration: number = 0;

    @property
    minStarDuration: number = 0;

    @property(cc.Node)
    ground: cc.Node = null;

    @property(cc.Node)
    player: cc.Node = null;

    @property(cc.Label)
    scoreDisplay: cc.Label = null;


    groundY: number = 0;

    score: number = 0;

    // LIFE-CYCLE CALLBACKS:

    onLoad () {
        this.groundY = this.ground.y + this.ground.height / 2;
        this.spawnNewStar();
    }

    start () {

    }

    // update (dt) {}

    spawnNewStar () {
        const newStar = cc.instantiate(this.starPrefab);
        this.node.addChild(newStar);
        newStar.setPosition(this.getNewStarPosition());

        newStar.getComponent('Star').game = this;
    }

    getNewStarPosition () {
        let randX = 0;
        const randY = this.groundY + Math.random() * this.player.getComponent('Player').jumpHeight + 50;
        const maxX = this.node.width / 2;
        randX = (Math.random()  - 0.5) * 2 * maxX;
        return cc.v2(randX, randY);
    }

    gainScore() {
        this.score += 1;
        this.scoreDisplay.string = 'Score: ' + this.score;
    }
}
