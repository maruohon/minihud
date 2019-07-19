package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.world.gen.ChunkGeneratorHell;
import net.minecraft.world.gen.structure.MapGenNetherBridge;

@Mixin(ChunkGeneratorHell.class)
public interface IMixinChunkGeneratorHell
{
    @Accessor("genNetherBridge")
    MapGenNetherBridge getFortressGenerator();
}
