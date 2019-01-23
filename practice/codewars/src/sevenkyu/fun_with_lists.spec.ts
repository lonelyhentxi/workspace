import { assert } from "chai";
import { indexOf, FunWithListsNode, length } from "./fun_with_lists";

function listFromArray(array: any[]): FunWithListsNode | null {
  const mapedArray = array.map(item => new FunWithListsNode(item, null));
  const length = mapedArray.length;
  mapedArray.forEach((node, index) => {
    node.next = index + 1 < length ? mapedArray[index + 1] : null;
  });
  return mapedArray.length > 0 ? mapedArray[0] : null;
}

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
