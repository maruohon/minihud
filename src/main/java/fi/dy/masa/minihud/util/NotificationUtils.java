package fi.dy.masa.minihud.util;

import java.util.List;
import net.minecraft.block.state.IBlockState;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.play.server.SPacketMultiBlockChange;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.renderer.OverlayRendererBeaconRange;

public class NotificationUtils
{
    public static void onBlockChange(BlockPos pos, IBlockState stateNew)
    {
        DataStorage.getInstance().markChunkForHeightmapCheck(pos.getX() >> 4, pos.getZ() >> 4);
        OverlayRendererBeaconRange.checkNeedsUpdate(pos, stateNew);
    }

    public static void onMultiBlockChange(ChunkPos chunkPos, SPacketMultiBlockChange.BlockUpdateData[] data)
    {
        DataStorage.getInstance().markChunkForHeightmapCheck(chunkPos.x, chunkPos.z);

        if (RendererToggle.OVERLAY_BEACON_RANGE.isRendererEnabled() &&
            Configs.Generic.BEACON_RANGE_AUTO_UPDATE.getBooleanValue())
        {
            for (SPacketMultiBlockChange.BlockUpdateData d : data)
            {
                OverlayRendererBeaconRange.checkNeedsUpdate(d.getPos(), d.getBlockState());
            }
        }
    }

    public static void onChunkData(int chunkX, int chunkZ, List<NBTTagCompound> blockEntities)
    {
        DataStorage.getInstance().markChunkForHeightmapCheck(chunkX, chunkZ);

        if (RendererToggle.OVERLAY_BEACON_RANGE.isRendererEnabled() &&
            Configs.Generic.BEACON_RANGE_AUTO_UPDATE.getBooleanValue())
        {
            OverlayRendererBeaconRange.checkNeedsUpdate(new ChunkPos(chunkX, chunkZ));
        }
    }
}
