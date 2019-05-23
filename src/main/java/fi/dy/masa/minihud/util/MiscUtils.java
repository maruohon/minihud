package fi.dy.masa.minihud.util;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Random;
import fi.dy.masa.minihud.MiniHUD;
import net.minecraft.client.MinecraftClient;
import net.minecraft.network.chat.ChatMessageType;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.util.math.ChunkPos;

public class MiscUtils
{
    private static final Random RAND = new Random();

    public static long bytesToMb(long bytes)
    {
        return bytes / 1024L / 1024L;
    }

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

    public static void printInfoMessage(String key, Object... args)
    {
        MinecraftClient.getInstance().inGameHud.addChatMessage(ChatMessageType.GAME_INFO, new TranslatableComponent(key, args));
    }

    /**
     * This method has been taken from the Carpet mod, by gnembon
     */
    public static int getChunkOrder(int chunkX, int chunkZ, int hashSize)
    {
        try
        {
            Method method = HashMap.class.getDeclaredMethod("hash", Object.class);
            method.setAccessible(true);

            return (Integer) method.invoke(null, Long.hashCode(ChunkPos.toLong(chunkX, chunkZ))) & (hashSize - 1);
        }
        catch (Exception e)
        {
            MiniHUD.logger.error("Error while trying to get the chunk unload order");
            return -1;
        }
    }
}
