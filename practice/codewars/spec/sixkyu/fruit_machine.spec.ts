import {fruit} from "sixkyu/fruit_machine";
import {assert} from "chai";
let reel, reel1, reel2, reel3, spin;

describe("Sample Tests", function() {
  it("Should pass sample tests", function() {
    reel = ["Wild","Star","Bell","Shell","Seven","Cherry","Bar","King","Queen","Jack"];
    spin = [0,0,0];
    assert.equal(fruit([reel,reel,reel],spin),100, "Should return: '100'");
    
    reel1 = ["Wild","Star","Bell","Shell","Seven","Cherry","Bar","King","Queen","Jack"];
    reel2 = ["Bar", "Wild", "Queen", "Bell", "King", "Seven", "Cherry", "Jack", "Star", "Shell"];
    reel3 = ["Bell", "King", "Wild", "Bar", "Seven", "Jack", "Shell", "Cherry", "Queen", "Star"];
    spin = [5,4,3];
    assert.equal(fruit([reel1,reel2,reel3],spin),0, "Should return: '0'");
    
    reel1 = ["King", "Cherry", "Bar", "Jack", "Seven", "Queen", "Star", "Shell", "Bell", "Wild"];
    reel2 = ["Bell", "Seven", "Jack", "Queen", "Bar", "Star", "Shell", "Wild", "Cherry", "King"];
    reel3 = ["Wild", "King", "Queen", "Seven", "Star", "Bar", "Shell", "Cherry", "Jack", "Bell"];
    spin = [0,0,1];
    assert.equal(fruit([reel1,reel2,reel3],spin),3, "Should return: '3'");
    
    reel1 = ["King", "Jack", "Wild", "Bell", "Star", "Seven", "Queen", "Cherry", "Shell", "Bar"];
    reel2 = ["Star", "Bar", "Jack", "Seven", "Queen", "Wild", "King", "Bell", "Cherry", "Shell"];
    reel3 = ["King", "Bell", "Jack", "Shell", "Star", "Cherry", "Queen", "Bar", "Wild", "Seven"];
    spin = [0,5,0];
    assert.equal(fruit([reel1,reel2,reel3],spin),6, "Should return: '6'");
  });
});
