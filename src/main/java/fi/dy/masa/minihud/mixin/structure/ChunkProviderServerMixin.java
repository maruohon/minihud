package fi.dy.masa.minihud.mixin.structure;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.world.gen.ChunkProviderServer;
import net.minecraft.world.gen.IChunkGenerator;

@Mixin(ChunkProviderServer.class)
public interface ChunkProviderServerMixin
{
    @Accessor("chunkGenerator")
    IChunkGenerator minihud_getChunkGenerator();
}
