package fi.dy.masa.itemscroller.util;

import java.util.ArrayList;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.network.ClientPlayerEntity;
import net.minecraft.network.Packet;

public class ClickPacketBuffer
{
    private static final ArrayList<Packet<?>> BUFFER = new ArrayList<>();
    private static boolean shouldBufferPackets;

    public static void reset()
    {
        shouldBufferPackets = false;
        BUFFER.clear();
    }

    public static boolean shouldBufferClickPackets()
    {
        return shouldBufferPackets;
    }

    public static boolean shouldCancelWindowClicks()
    {
        // Don't cancel the clicks on the client if we have some Item Scroller actions in progress
        return shouldBufferPackets == false && BUFFER.isEmpty() == false;
    }

    public static void setShouldBufferClickPackets(boolean shouldBuffer)
    {
        shouldBufferPackets = shouldBuffer;
    }

    public static void bufferPacket(Packet<?> packet)
    {
        BUFFER.add(packet);
    }

    public static void sendBufferedPackets(int maxCount)
    {
        ClientPlayerEntity player = MinecraftClient.getInstance().player;

        if (player != null && BUFFER.isEmpty() == false)
        {
            maxCount = Math.min(maxCount, BUFFER.size());

            for (int i = 0; i < maxCount; ++i)
            {
                player.networkHandler.sendPacket(BUFFER.remove(0));
            }
        }
    }
}
