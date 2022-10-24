package fi.dy.masa.minihud.mixin.structure;

import it.unimi.dsi.fastutil.longs.Long2ObjectMap;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

import net.minecraft.world.gen.structure.MapGenStructure;
import net.minecraft.world.gen.structure.StructureStart;

@Mixin(MapGenStructure.class)
public interface MapGenStructureMixin
{
    @Accessor("structureMap")
    Long2ObjectMap<StructureStart> minihud_getStructureMap();
}
