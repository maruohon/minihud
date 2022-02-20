package fi.dy.masa.minihud.mixin.info_lines;

import java.util.Set;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.world.gen.ChunkProviderServer;

@Mixin(ChunkProviderServer.class)
public interface ChunkProviderServerMixin
{
    @Accessor("droppedChunks")
    Set<Long> minihud_getDroppedChunks();
}
