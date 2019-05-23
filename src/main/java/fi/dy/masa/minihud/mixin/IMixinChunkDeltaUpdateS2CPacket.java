package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.client.network.packet.ChunkDeltaUpdateS2CPacket;
import net.minecraft.util.math.ChunkPos;

@Mixin(ChunkDeltaUpdateS2CPacket.class)
public interface IMixinChunkDeltaUpdateS2CPacket
{
    @Accessor("chunkPos")
    ChunkPos getChunkPos();
}
