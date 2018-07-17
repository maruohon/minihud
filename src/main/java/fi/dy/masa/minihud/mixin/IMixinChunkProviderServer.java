package fi.dy.masa.minihud.mixin;

import java.util.Set;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.world.gen.ChunkProviderServer;

@Mixin(ChunkProviderServer.class)
public interface IMixinChunkProviderServer
{
    @Accessor
    Set<Long> getDroppedChunksSet();
}
