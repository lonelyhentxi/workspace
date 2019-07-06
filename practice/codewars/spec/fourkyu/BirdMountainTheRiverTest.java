package fourkyu;

import org.junit.Test;
import static org.junit.Assert.assertArrayEquals;
import fourkyu.BirdMountainTheRiver;

public class BirdMountainTheRiverTest {
    @Test
    public void testDryGround() {
        char[][] terrain = {
                "  ^^^^^^             ".toCharArray(),
                "^^^^^^^^       ^^^   ".toCharArray(),
                "^^^^^^^  ^^^         ".toCharArray(),
                "^^^^^^^  ^^^         ".toCharArray(),
                "^^^^^^^  ^^^         ".toCharArray(),
                "---------------------".toCharArray(),
                "^^^^^                ".toCharArray(),
                "   ^^^^^^^^  ^^^^^^^ ".toCharArray(),
                "^^^^^^^^     ^     ^ ".toCharArray(),
                "^^^^^        ^^^^^^^ ".toCharArray()
        };
        assertArrayEquals(new int[]{189,99,19,3}, BirdMountainTheRiver.dryGround(terrain));
    }
}