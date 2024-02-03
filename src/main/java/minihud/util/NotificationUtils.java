package minihud.util;

import java.util.List;

import net.minecraft.block.state.IBlockState;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.play.server.SPacketMultiBlockChange;

import malilib.util.position.BlockPos;
import malilib.util.position.ChunkPos;
import minihud.config.Configs;
import minihud.config.RendererToggle;
import minihud.data.DataStorage;
import minihud.renderer.RenderContainer;

public class NotificationUtils
{
    public static void onBlockChange(BlockPos pos, IBlockState stateNew)
    {
        DataStorage.getInstance().onBlocksChangedInChunk(pos.getX() >> 4, pos.getZ() >> 4);
        RenderContainer.BEACON_OVERLAY.checkNeedsUpdate(pos, stateNew);
    }

    public static void onMultiBlockChange(ChunkPos chunkPos, SPacketMultiBlockChange.BlockUpdateData[] data)
    {
        DataStorage.getInstance().onBlocksChangedInChunk(chunkPos.x, chunkPos.z);

        if (RendererToggle.BEACON_RANGE.isRendererEnabled() &&
            Configs.Generic.BEACON_RANGE_AUTO_UPDATE.getBooleanValue())
        {
            for (SPacketMultiBlockChange.BlockUpdateData d : data)
            {
                RenderContainer.BEACON_OVERLAY.checkNeedsUpdate(BlockPos.of(d.getPos()), d.getBlockState());
            }
        }
    }

    public static void onChunkData(int chunkX, int chunkZ, List<NBTTagCompound> blockEntities)
    {
        DataStorage.getInstance().onBlocksChangedInChunk(chunkX, chunkZ);

        if (RendererToggle.BEACON_RANGE.isRendererEnabled() &&
            Configs.Generic.BEACON_RANGE_AUTO_UPDATE.getBooleanValue())
        {
            RenderContainer.BEACON_OVERLAY.checkNeedsUpdate(new ChunkPos(chunkX, chunkZ));
        }
    }
}
