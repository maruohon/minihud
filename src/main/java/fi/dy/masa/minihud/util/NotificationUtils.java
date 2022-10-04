package fi.dy.masa.minihud.util;

import java.util.List;
import net.minecraft.block.BlockState;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.network.packet.s2c.play.ChunkDeltaUpdateS2CPacket;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkSectionPos;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.renderer.OverlayRendererBeaconRange;

public class NotificationUtils
{
    public static void onBlockChange(BlockPos pos, BlockState stateNew)
    {
        DataStorage.getInstance().markChunkForHeightmapCheck(pos.getX() >> 4, pos.getZ() >> 4);
        OverlayRendererBeaconRange.checkNeedsUpdate(pos, stateNew);
    }

    public static void onMultiBlockChange(ChunkSectionPos chunkPos, ChunkDeltaUpdateS2CPacket packet)
    {
        DataStorage.getInstance().markChunkForHeightmapCheck(chunkPos.getSectionX(), chunkPos.getSectionZ());

        if (RendererToggle.OVERLAY_BEACON_RANGE.getBooleanValue() &&
            Configs.Generic.BEACON_RANGE_AUTO_UPDATE.getBooleanValue())
        {
            packet.visitUpdates(OverlayRendererBeaconRange::checkNeedsUpdate);
        }
    }

    public static void onChunkData(int chunkX, int chunkZ, List<NbtCompound> beList)
    {
        DataStorage.getInstance().markChunkForHeightmapCheck(chunkX, chunkZ);

        if (RendererToggle.OVERLAY_BEACON_RANGE.getBooleanValue() &&
            Configs.Generic.BEACON_RANGE_AUTO_UPDATE.getBooleanValue())
        {
            if (OverlayRendererBeaconRange.checkNeedsUpdate(chunkX, chunkZ) == false)
            {
                OverlayRendererBeaconRange.checkNeedsUpdate(beList);
            }
        }
    }
}
