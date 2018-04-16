package fi.dy.masa.minihud.util;

import java.util.Random;

public class MiscUtils
{
    private static final Random RAND = new Random();

    public static boolean canSlimeSpawnAt(int posX, int posZ, long worldSeed)
    {
        return canSlimeSpawnInChunk(posX >> 4, posZ >> 4, worldSeed);
    }

    public static boolean canSlimeSpawnInChunk(int chunkX, int chunkZ, long worldSeed)
    {
        long slimeSeed = 987234911L;
        long rngSeed = worldSeed +
                       (long) (chunkX * chunkX *  4987142) + (long) (chunkX * 5947611) +
                       (long) (chunkZ * chunkZ) * 4392871L + (long) (chunkZ * 389711) ^ slimeSeed;

        RAND.setSeed(rngSeed);

        return RAND.nextInt(10) == 0;
    }
}
