package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.world.gen.ChunkGeneratorEnd;
import net.minecraft.world.gen.structure.MapGenEndCity;

@Mixin(ChunkGeneratorEnd.class)
public interface IMixinChunkGeneratorEnd
{
    @Accessor("endCityGen")
    MapGenEndCity getEndCityGenerator();
}
