package fi.dy.masa.minihud.mixin.structure;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

import net.minecraft.world.gen.ChunkGeneratorEnd;
import net.minecraft.world.gen.structure.MapGenEndCity;

@Mixin(ChunkGeneratorEnd.class)
public interface ChunkGeneratorEndMixin
{
    @Accessor("endCityGen")
    MapGenEndCity minihud_getEndCityGenerator();
}
