package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.network.play.server.SPacketMultiBlockChange;
import net.minecraft.util.math.ChunkPos;

@Mixin(SPacketMultiBlockChange.class)
public interface IMixinSPacketMultiBlockChange
{
    @Accessor
    ChunkPos getChunkPos();
}
