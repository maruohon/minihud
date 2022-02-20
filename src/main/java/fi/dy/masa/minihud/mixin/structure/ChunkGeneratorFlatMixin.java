package fi.dy.masa.minihud.mixin.structure;

import java.util.Map;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.world.gen.ChunkGeneratorFlat;
import net.minecraft.world.gen.structure.MapGenStructure;

@Mixin(ChunkGeneratorFlat.class)
public interface ChunkGeneratorFlatMixin
{
    @Accessor("structureGenerators")
    Map<String, MapGenStructure> minihud_getStructureGenerators();
}
