import { assert } from "chai";
import {
  indexOf,
  length,
  lastIndexOf,
  countIf,
  allMatch,
  anyMatch,
  listFromArray,
  FunWithListsNode,
  nodeEqual,
  filter,
  map,
  reduce,
} from "sevenkyu/fun_with_lists";

describe("indexOf", function() {
  it("null tests", function() {
    assert(indexOf(null, 17) === -1);
  });

  it("basic tests", () => {
    assert(indexOf(listFromArray([1, 2, 3]), 2) === 1);
    assert(indexOf(listFromArray(["aaa", "b", "abc"]), "aaa") === 0);
    assert(indexOf(listFromArray([17, "17", 1.2]), 17) === 0);
    assert(indexOf(listFromArray([17, "17", 1.2]), "17") === 1);
    assert(indexOf(listFromArray([1, 2, 3, 3]), 3) === 2);
  });
});

describe("length", function() {
  it("basic tests", function() {
    assert(length(null) == 0);
    assert(length(listFromArray([1, 2, 3, 4])) == 4);
  });
});

describe("lastIndexOf", function() {
  it("basic tests", function() {
    assert(lastIndexOf(null, 17) === -1);
    assert(lastIndexOf(listFromArray([1, 2, 3]), 2) === 1);
    assert(lastIndexOf(listFromArray(["aaa", "b", "abc"]), "aaa") === 0);
    assert(lastIndexOf(listFromArray([17, "17", 1.2]), 17) === 0);
    assert(lastIndexOf(listFromArray([17, "17", 1.2]), "17") === 1);
    assert(lastIndexOf(listFromArray([1, 2, 3, 3]), 3) === 3);
  });
});

describe("countIf", function() {
  it("basic tests", function() {
    assert(countIf(null, x => false) === 0);
    assert(countIf(listFromArray([1, 2, 3]), x => true) === 3);
  });

  it("array of ints", function() {
    assert(countIf(listFromArray([1, 2, 3]), x => x <= 2) === 2);
  });
});

describe("anyMatch & allMatch", function() {
  it("null tests", function() {
    assert(anyMatch(null, x => false) === false);
    assert(anyMatch(null, x => true) === false);
    assert(allMatch(null, x => false) === true);
    assert(allMatch(null, x => true) === true);
  });
  it("any tests", ()=>{
    assert(anyMatch(listFromArray([1, 2, 3]), x => x > 1) === true, ""+(anyMatch(listFromArray([1, 2, 3]), x => x > 1)));
  })

  it("all tests",()=>{
    assert(allMatch(listFromArray([1, 2, 3]), x => x > 1) === false,""+(allMatch(listFromArray([1, 2, 3]), x => x > 1)));
  })
});


describe("filter", function() {

    function testFilter(input_list_head: FunWithListsNode|null, predicate: (value: any)=>boolean, expected_list_head: FunWithListsNode|null) {
        assert(nodeEqual(filter(input_list_head, predicate), expected_list_head));
    }
    
    it("basic tests", function() {
      testFilter(null, x => false, null);
      testFilter(new FunWithListsNode(1, new FunWithListsNode(2, new FunWithListsNode(3))), x => true, new FunWithListsNode(1, new FunWithListsNode(2, new FunWithListsNode(3))));
      testFilter(new FunWithListsNode(1, new FunWithListsNode(2, new FunWithListsNode(3))), x => x <= 2, new FunWithListsNode(1, new FunWithListsNode(2)));
      testFilter(new FunWithListsNode(1, new FunWithListsNode(2, new FunWithListsNode(3))), x => x === 1, new FunWithListsNode(1));
    });
    
  });

  describe("map", function() {

    function testMap(input_list_head: FunWithListsNode|null, f: (value: any)=>any, expected_list_head: FunWithListsNode|null) {
      assert(nodeEqual(map(input_list_head, f), expected_list_head));
  }
  
    it("basic tests", function() {
      testMap(null, x => false, null);
      testMap(new FunWithListsNode(1, new FunWithListsNode(2, new FunWithListsNode(3))), x => x * 2, new FunWithListsNode(2, new FunWithListsNode(4, new FunWithListsNode(6))));
    });
  });


  describe("reduce", function() {

    it("basic tests", function() {
      assert(reduce(null, (a, b) => a + b, 0)=== 0);
      assert(reduce(new FunWithListsNode(1, new FunWithListsNode(2, new FunWithListsNode(3))), (a, b) => a + b, 0)=== 6);
      assert(reduce(new FunWithListsNode(1, new FunWithListsNode(2, new FunWithListsNode(3, new FunWithListsNode(4)))), (a, b) => a * b, 1)=== 24);
      assert(reduce(new FunWithListsNode('a', new FunWithListsNode('b', new FunWithListsNode('c'))), (a, b) => a + b, '')=== 'abc');
    });
    
  });