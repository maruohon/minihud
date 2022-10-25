package minihud.mixin.mod_data_update;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

import net.minecraft.network.play.server.SPacketMultiBlockChange;
import net.minecraft.util.math.ChunkPos;

@Mixin(SPacketMultiBlockChange.class)
public interface SPacketMultiBlockChangeMixin
{
    @Accessor("chunkPos")
    ChunkPos minihud_getChunkPos();
}
