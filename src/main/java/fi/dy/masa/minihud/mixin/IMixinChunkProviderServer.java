package fi.dy.masa.minihud.mixin;

import java.util.Set;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.world.gen.ChunkProviderServer;
import net.minecraft.world.gen.IChunkGenerator;

@Mixin(ChunkProviderServer.class)
public interface IMixinChunkProviderServer
{
    @Accessor
    Set<Long> getDroppedChunks();

    @Accessor("chunkGenerator")
    IChunkGenerator getChunkGenerator();
}
