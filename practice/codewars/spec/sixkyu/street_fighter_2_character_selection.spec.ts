import { assert } from 'chai';
import { streetFighterSelection } from 'sixkyu/street_fighter_2_character_selection';

let fighters = [
	["Ryu", "E.Honda", "Blanka", "Guile", "Balrog", "Vega"],
	["Ken", "Chun Li", "Zangief", "Dhalsim", "Sagat", "M.Bison"]
];

describe("Solution", function(){
let moves = ['up', 'left', 'right', 'left', 'left'];
  it("should work with few moves", function(){
    assert.deepEqual(streetFighterSelection(fighters, [0,0], moves),['Ryu', 'Vega', 'Ryu', 'Vega', 'Balrog']);
  });
});


describe("Solution", function(){
  let moves: string[] = [];
  it("should work with no selection cursor moves", function(){
    assert.deepEqual(streetFighterSelection(fighters, [0,0], moves),[]);
  });
});

describe("Solution", function(){
  let moves = ["left","left","left","left","left","left","left","left"];
  it("should work when always moving left", function(){
    assert.deepEqual(streetFighterSelection(fighters, [0,0], moves),['Vega', 'Balrog', 'Guile', 'Blanka', 'E.Honda', 'Ryu', 'Vega', 'Balrog']);
  });
});

describe("Solution", function(){
  let moves = ["right","right","right","right","right","right","right","right"];
  it("should work when always moving right", function(){
    assert.deepEqual(streetFighterSelection(fighters, [0,0], moves),['E.Honda', 'Blanka', 'Guile', 'Balrog', 'Vega', 'Ryu', 'E.Honda', 'Blanka']);
  });
});

describe("Solution", function(){
  let moves = ["up","left","down","right","up","left","down","right"];
  it("should use all 4 directions clockwise twice", function(){
    assert.deepEqual(streetFighterSelection(fighters, [0,0], moves),['Ryu', 'Vega', 'M.Bison', 'Ken', 'Ryu', 'Vega', 'M.Bison', 'Ken']);
  });
});

describe("Solution", function(){
  let moves = ["down","down","down","down"];
  it("should work when always moving down", function(){
    assert.deepEqual(streetFighterSelection(fighters, [0,0], moves), ['Ken', 'Ken', 'Ken', 'Ken']);
  });
});

describe("Solution", function(){
  let moves = ["up","up","up","up"];
  it("should work when always moving up", function(){
    assert.deepEqual(streetFighterSelection(fighters, [0,0], moves), ['Ryu', 'Ryu', 'Ryu', 'Ryu']);
  });
});