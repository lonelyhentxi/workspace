package fourkyu;

import java.util.ArrayList;
import java.util.Optional;


public class BirdMountainTheRiver {

    public static int[] dryGround(char[][] terrain) {
        return dryGroundWithExplicitDays(terrain,4);
    }

    public static int[] dryGroundWithExplicitDays(char[][] terrain, int days) {
        var res = new int[days];
        var current = peekUntilEnough(terrain,days);
        var testWetIndex = -1;
        var gridNum = current.getHeight() * current.getWidth();
        for (var i = 0; i < gridNum; i++) {
            var grid = current.indexToGrid(i);
            if (grid.isPresent() && grid.get() == -1) {
                testWetIndex = i;
                break;
            }
        }
        if (testWetIndex == -1) {
            for (var i = 0; i < days; i++) {
                res[i] = current.getWidth() * current.getHeight();
            }
            return res;
        }
        UnionFind uf = new UnionFind(gridNum);
        for (var d = 0; d < days; d++) {
            var wetAltitude = -0.5 + d;
            var remained = gridNum;
            for (var i = 0; i < current.getHeight(); i++) {
                for (var j = 0; j < current.getWidth(); j++) {
                    current.unionMask(uf, i, j, wetAltitude);
                }
            }
            for (var i = 0; i < gridNum; i++) {
                if(uf.connected(testWetIndex,i)) {
                    remained--;
                }
            }
            res[d]=remained;
        }
        return res;
    }

    public static Ground peekUntilEnough(char[][] terrain, int days) {
        var prev = new Ground(terrain);
        Ground current;
        var altitude = 1;
        while (true) {
            var count = 0;
            current = prev.copy();
            for (var i = 0; i < prev.getHeight(); i++) {
                for (var j = 0; j < prev.getWidth(); j++) {
                    if (prev.altitudeMask(i, j, altitude)) {
                        count += 1;
                        current.setGrid(i, j, altitude);
                    }
                }
            }
            prev = current;
            if (count != 0 && altitude < days - 1) {
                altitude += 1;
            } else {
                break;
            }
        }
        return prev;
    }
}

class UnionFind {
    private int[] id;
    private int[] sz;
    private int count;

    public UnionFind(int N) {
        count = 0;
        id = new int[N];
        sz = new int[N];
        for (int i = 0; i < N; i++) {
            id[i] = i;
        }
        for (int i = 0; i < N; i++) {
            sz[i] = 1;
        }
    }

    public int count() {
        return count;
    }

    private int find(int i) {
        while (i != id[i]) {
            id[i] = id[id[i]];
            i = id[i];
        }
        return i;
    }

    public boolean connected(int p, int q) {
        return find(p) == find(q);
    }

    public void union(int p, int q) {
        int i = find(p);
        int j = find(q);
        if (i == j)
            return;
        if (sz[i] < sz[j]) {
            id[i] = j;
            sz[j] += sz[i];
        } else {
            id[j] = i;
            sz[i] += sz[j];
        }
        count++;
    }
}

class Ground {
    private ArrayList<ArrayList<Optional<Integer>>> _terrain;
    private static int[] _direction_x = new int[]{1, -1, 0, 0};
    private static int[] _direction_y = new int[]{0, 0, 1, -1};

    Ground(char[][] terrain) {
        if(terrain==null||terrain.length==0) {
            _terrain=new ArrayList<ArrayList<Optional<Integer>>>();
            return;
        }
        var converted = new ArrayList<ArrayList<Optional<Integer>>>();
        for (var line : terrain) {
            var convertedLine = new ArrayList<Optional<Integer>>();
            for (var ch : line) {
                convertedLine.add(parseGridContent(ch));
            }
            converted.add(convertedLine);
        }
        _terrain = converted;
    }

    Ground(ArrayList<ArrayList<Optional<Integer>>> terrain) {
        _terrain = terrain;
    }

    int getHeight() {
        return _terrain.size();
    }

    int getWidth() {
        return _terrain.size()==0?0:_terrain.get(0).size();
    }

    private static Optional<Integer> parseGridContent(char grid) {
        if (grid == ' ') {
            return Optional.of(0);
        } else if (grid == '^') {
            return Optional.empty();
        } else if (grid == '-') {
            return Optional.of(-1);
        } else {
            return Optional.of(Integer.parseInt(String.valueOf(grid)));
        }
    }

    Optional<Integer> getGrid(int y, int x) {
        return _terrain.get(y).get(x);
    }

    Optional<Integer> parseGrid(int y, int x) {
        if (y < 0 || x < 0 || y >= getHeight() || x >= getWidth()) {
            return Optional.of(0);
        }
        return getGrid(y, x);
    }

    boolean altitudeMask(int y, int x, int altitude) {
        var current = parseGrid(y, x);
        if (current.isPresent()) {
            return false;
        }
        for (var i = 0; i < _direction_x.length; i++) {
            var next_y = y + _direction_y[i];
            var next_x = x + _direction_x[i];
            var target = parseGrid(next_y, next_x);
            if (target.isPresent() && target.get() < altitude) {
                return true;
            }
        }
        return false;
    }

    void setGrid(int y, int x, int altitude) {
        _terrain.get(y).set(x, Optional.of(altitude));
    }

    Ground copy() {
        var newTerrain = new ArrayList<ArrayList<Optional<Integer>>>();
        for (var i = 0; i < getHeight(); i++) {
            var newLine = new ArrayList<Optional<Integer>>();
            for (var j = 0; j < getWidth(); j++) {
                newLine.add(_terrain.get(i).get(j));
            }
            newTerrain.add(newLine);
        }
        return new Ground(newTerrain);
    }

    int positionToIndex(int y, int x) {
        return y * getWidth() + x;
    }

    Optional<Integer> indexToGrid(int index) {
        var y = index / getWidth();
        var x = y % getWidth();
        return getGrid(y, x);
    }

    void unionMask(UnionFind uf, int y, int x, double wetAltitude) {
        var current = parseGrid(y, x);
        if (current.isEmpty() || current.get() > wetAltitude) {
            return;
        }
        var currentGridIndex = positionToIndex(y, x);
        for (var i = 0; i < _direction_x.length; i++) {
            var next_y = y + _direction_y[i];
            var next_x = x + _direction_x[i];
            if (next_x >= 0 && next_y >= 0 && next_x < getWidth() && next_y < getHeight()) {
                var nextGrid = parseGrid(next_y, next_x);
                if (nextGrid.isPresent() && nextGrid.get() <= wetAltitude) {
                    uf.union(positionToIndex(next_y, next_x), currentGridIndex);
                }
            }

        }
    }
}