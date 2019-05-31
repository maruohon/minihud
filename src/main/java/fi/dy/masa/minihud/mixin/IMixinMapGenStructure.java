package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import it.unimi.dsi.fastutil.longs.Long2ObjectMap;
import net.minecraft.world.gen.structure.MapGenStructure;
import net.minecraft.world.gen.structure.StructureStart;

@Mixin(MapGenStructure.class)
public interface IMixinMapGenStructure
{
    @Accessor("structureMap")
    Long2ObjectMap<StructureStart> getStructureMap();
}
