package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.client.network.packet.ChunkDeltaUpdateClientPacket;
import net.minecraft.world.chunk.ChunkPos;

@Mixin(ChunkDeltaUpdateClientPacket.class)
public interface IMixinChunkDeltaUpdateClientPacket
{
    @Accessor
    ChunkPos getChunkPos();
}
